## Run tab: launch a saved environment as a background process and monitor
## its progress. Connected to the Analyze tab only through the filesystem
## (the results/<env>/ directory it produces) - see mod_analyze.R.
##
## The main panel is a readout: what state the run is in, the three figures
## worth watching while it works, and the tail of its log.

mod_run_ui <- function(id) {
  ns <- shiny::NS(id)
  rbext_page(
    shiny::tags$h1("Run simulation", class = "visually-hidden"),
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 320,
        resizable = FALSE,
        title = shiny::tags$h2("Environment", class = "sidebar-title"),
        open = list(desktop = "always", mobile = "always-above"),
        shiny::selectInput(ns("env"), "Saved environment", choices = NULL, width = "100%"),
        shiny::uiOutput(ns("run_workload")),
        shiny::uiOutput(ns("run_actions")),
        shiny::p(
          class = "rbext-note",
          "The run happens in a separate R process, so you can keep working in ",
          "the other tabs while it goes."
        )
      ),
      bslib::card(
        bslib::card_header("Run status"),
        bslib::card_body(shiny::uiOutput(ns("status"), `aria-live` = "polite"))
      ),
      bslib::card(
        bslib::card_header("Log"),
        bslib::card_body(shiny::uiOutput(ns("log_tail")))
      )
    )
  )
}

## env_saved: reactive that changes value whenever an environment is saved
## elsewhere in the app (the Configure tab), so the environment picker here
## can refresh itself instead of relying on a manual button.
mod_run_server <- function(id, on_run_complete = NULL, env_saved = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    state <- shiny::reactiveValues(
      proc = NULL,
      env = NULL,
      log_file = NULL,
      start_time = NULL,
      total = NA_integer_,
      done = FALSE,
      exit_status = NULL,
      error_message = NULL
    )
    pending_launch_env <- shiny::reactiveVal(NULL)

    refresh_env_choices <- function() {
      envs <- list_environments()
      choices <- envs$name
      names(choices) <- paste0(envs$name, " (", envs$source, ")")
      shiny::updateSelectInput(session, "env", choices = choices)
    }
    refresh_env_choices()
    if (!is.null(env_saved)) {
      shiny::observeEvent(env_saved(), refresh_env_choices(), ignoreInit = TRUE)
    }

    output$run_workload <- shiny::renderUI({
      env <- input$env
      if (is.null(env) || !nzchar(env)) {
        return(shiny::p(class = "rbext-note", "No runnable environments are available."))
      }
      total <- estimate_total_scenarios(env)
      if (is.na(total)) {
        return(shiny::p(class = "rbext-note", "Workload estimate unavailable for this environment."))
      }
      shiny::div(
        class = "rbext-workload",
        shiny::strong(sprintf("About %s scenarios", format(total, big.mark = ",")))
      )
    })

    begin_run <- function(env) {
      if (is.null(env) || env == "") {
        return(invisible(NULL))
      }
      if (!is.null(state$proc) && state$proc$is_alive()) {
        shiny::showNotification("A run is already in progress.", type = "warning")
        return(invisible(NULL))
      }

      state$total <- estimate_total_scenarios(env)
      state$env <- env
      state$start_time <- Sys.time()
      state$done <- FALSE
      state$exit_status <- NULL
      state$error_message <- NULL
      state$log_file <- file.path("logs", env, "error_logs", "error_log.log")

      state$proc <- launch_simulation_run(env)
      invisible(NULL)
    }

    shiny::observeEvent(input$launch, {
      env <- input$env
      if (is.null(env) || !nzchar(env)) return(NULL)
      if (rbext_has_results(env)) {
        pending_launch_env(env)
        total <- estimate_total_scenarios(env)
        estimate_text <- if (is.na(total)) "" else sprintf(" The new run contains about %s scenarios.", format(total, big.mark = ","))
        shiny::showModal(shiny::modalDialog(
          title = "Replace existing results?",
          sprintf("Results already exist for '%s'. Launching will permanently replace them.%s", env, estimate_text),
          footer = shiny::tagList(
            shiny::modalButton("Keep existing results"),
            rbext_action_button(session$ns("confirm_launch"), "Replace and launch", class = "btn-danger")
          ),
          easyClose = FALSE
        ))
        return(NULL)
      }
      begin_run(env)
    })

    shiny::observeEvent(input$confirm_launch, {
      env <- pending_launch_env()
      shiny::removeModal()
      pending_launch_env(NULL)
      begin_run(env)
    })

    shiny::observeEvent(input$cancel, {
      if (!is.null(state$proc) && state$proc$is_alive()) {
        shiny::showModal(shiny::modalDialog(
          title = "Cancel this run?",
          "Completed work from the current run may be incomplete and will not be available for analysis.",
          footer = shiny::tagList(
            shiny::modalButton("Continue running"),
            rbext_action_button(session$ns("confirm_cancel"), "Cancel run", class = "btn-danger")
          ),
          easyClose = FALSE
        ))
      }
    })

    shiny::observeEvent(input$confirm_cancel, {
      shiny::removeModal()
      if (!is.null(state$proc) && state$proc$is_alive()) {
        state$proc$kill()
        shiny::showNotification("Run cancelled.", type = "warning")
      }
    })

    ## How many scenarios the run has simulated so far. A run reports this
    ## itself, scenario by scenario, in logs/<env>/progress.json; counting the
    ## rows it has written is the fallback for a run that has not written that
    ## file, and is coarser - a method that runs in parallel collects its
    ## scenarios in the master and writes them all at once when it finishes.
    scenarios_done <- function(progress, env) {
      progress$done %||% count_result_rows(env, since = state$start_time)
    }

    poll <- shiny::reactivePoll(
      1500, session,
      checkFunc = function() {
        if (is.null(state$proc)) {
          return(0)
        }
        if (state$done) {
          return(-1)
        }
        # Sys.time() makes this differ on every tick even when no new result
        # rows have been written yet (setup, model compilation, the first
        # replicate...), so the elapsed-time readout keeps advancing instead
        # of freezing at whatever it showed on the first render.
        progress <- read_run_progress(state$env, since = state$start_time)
        list(Sys.time(), state$proc$is_alive(), scenarios_done(progress, state$env))
      },
      valueFunc = function() {
        if (is.null(state$proc)) {
          return(NULL)
        }
        alive <- state$proc$is_alive()
        if (!alive && !state$done) {
          state$done <- TRUE
          state$exit_status <- state$proc$get_exit_status()
          if (!identical(state$exit_status, 0L)) {
            state$error_message <- tryCatch({
              state$proc$get_result()
              NULL
            }, error = function(e) conditionMessage(e))
            if (is.null(state$error_message)) {
              err_lines <- tryCatch(readLines(state$proc$get_error_file()), error = function(e) character(0))
              state$error_message <- paste(utils::tail(err_lines, 30), collapse = "\n")
            }
          } else if (!is.null(on_run_complete)) {
            on_run_complete(state$env)
          }
        }
        progress <- read_run_progress(state$env, since = state$start_time)
        list(
          alive = alive,
          rows = scenarios_done(progress, state$env),
          # The run knows its own total exactly; state$total is the estimate
          # the app made from the same configuration before launching, and
          # only stands in until the run has said otherwise.
          total = progress$total %||% state$total,
          exit_status = state$exit_status,
          error_message = state$error_message
        )
      }
    )

    ## Whole minutes read better than four-digit second counts once a run has
    ## been going for a while.
    format_elapsed <- function(seconds) {
      seconds <- as.numeric(seconds)
      if (seconds < 60) {
        return(sprintf("%ss", round(seconds)))
      }
      if (seconds < 3600) {
        return(sprintf("%dm %02ds", floor(seconds / 60), round(seconds) %% 60))
      }
      sprintf("%dh %02dm", floor(seconds / 3600), floor((seconds %% 3600) / 60))
    }

    output$status <- shiny::renderUI({
      p <- poll()
      if (is.null(p)) {
        return(rbext_empty("Pick an environment and launch it to see progress here."))
      }
      elapsed <- difftime(Sys.time(), state$start_time, units = "secs")
      progress_pct <- if (is.na(p$total) || p$total == 0) {
        NA
      } else {
        min(100, round(100 * p$rows / p$total))
      }

      state_pill <- if (p$alive) {
        rbext_state("Running", "live")
      } else if (identical(p$exit_status, 0L)) {
        rbext_state("Finished", "done")
      } else {
        rbext_state("Failed", "failed")
      }

      caption <- if (p$alive) {
        sprintf("Simulating '%s'.", state$env)
      } else if (identical(p$exit_status, 0L)) {
        sprintf("'%s' is done. Open it in the Analyze tab.", state$env)
      } else {
        sprintf("'%s' stopped before finishing. The trace below says why.", state$env)
      }

      shiny::tagList(
        shiny::div(
          class = "rbext-readout",
          state_pill,
          shiny::span(class = "rbext-muted", caption)
        ),
        shiny::div(
          class = "rbext-readout",
          style = "margin-top: 1rem;",
          rbext_metric(format_elapsed(elapsed), "Elapsed"),
          rbext_metric(format(p$rows, big.mark = ","), "Scenarios simulated"),
          rbext_metric(
            if (is.na(progress_pct)) "--" else paste0(progress_pct, "%"),
            if (is.na(progress_pct)) "Share of run" else sprintf("of ~%s expected", format(p$total, big.mark = ","))
          )
        ),
        if (!is.na(progress_pct)) {
          shiny::div(
            class = "progress",
            style = "margin-top: 1rem;",
            role = "progressbar",
            `aria-label` = "Simulation progress",
            `aria-valuenow` = progress_pct, `aria-valuemin` = "0", `aria-valuemax` = "100",
            `aria-valuetext` = sprintf("%s percent, %s of about %s scenarios", progress_pct, p$rows, p$total),
            shiny::div(
              class = paste(
                "progress-bar",
                if (p$alive) "progress-bar-striped progress-bar-animated" else ""
              ),
              style = sprintf("width: %s%%;", progress_pct)
            )
          )
        },
        if (!p$alive && !identical(p$exit_status, 0L) && !is.null(p$error_message)) {
          shiny::tags$pre(class = "rbext-trace", p$error_message)
        }
      )
    })

    output$log_tail <- shiny::renderUI({
      poll()
      log_dir <- if (is.null(state$env)) "" else file.path("logs", state$env, "error_logs")
      candidates <- if (nzchar(log_dir) && dir.exists(log_dir)) {
        list.files(log_dir, pattern = "\\.log$", full.names = TRUE)
      } else character(0)
      if (!is.null(state$start_time) && length(candidates) > 0) {
        candidates <- candidates[file.mtime(candidates) >= state$start_time]
      }
      if (length(candidates) == 0) {
        return(rbext_empty("The log appears once a run starts writing to it."))
      }
      log_file <- candidates[which.max(file.mtime(candidates))]
      lines <- tryCatch(readLines(log_file, warn = FALSE), error = function(e) character(0))
      shiny::tags$pre(
        class = "rbext-log", `aria-label` = "Latest simulation log", `aria-live` = "polite",
        paste(utils::tail(lines, 25), collapse = "\n")
      )
    })

    output$run_actions <- shiny::renderUI({
      p <- poll()
      alive <- !is.null(p) && isTRUE(p$alive)
      no_env <- is.null(input$env) || !nzchar(input$env)
      shiny::div(
        class = "rbext-actions",
        rbext_action_button(session$ns("launch"), "Launch run", class = "btn-primary",
                            disabled = alive || no_env),
        rbext_action_button(session$ns("cancel"), "Cancel run", class = "btn-danger",
                            disabled = !alive)
      )
    })

    invisible(NULL)
  })
}
