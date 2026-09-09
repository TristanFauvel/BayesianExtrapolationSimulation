## Run tab: launch a saved environment as a background process and monitor
## its progress. Connected to the Analyze tab only through the filesystem
## (the results/<env>/ directory it produces) - see mod_analyze.R.

mod_run_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        6,
        shiny::selectInput(ns("env"), "Environment", choices = NULL, width = "100%"),
        shiny::actionButton(ns("refresh_envs"), "Refresh list"),
        shiny::actionButton(ns("launch"), "Launch", class = "btn-primary"),
        shiny::actionButton(ns("cancel"), "Cancel run", class = "btn-danger")
      )
    ),
    shiny::hr(),
    shiny::uiOutput(ns("status")),
    shiny::hr(),
    shiny::h5("Log tail"),
    shiny::verbatimTextOutput(ns("log_tail"))
  )
}

mod_run_server <- function(id, on_run_complete = NULL) {
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

    refresh_env_choices <- function() {
      envs <- list_environments()
      choices <- envs$name
      names(choices) <- paste0(envs$name, " (", envs$source, ")")
      shiny::updateSelectInput(session, "env", choices = choices)
    }
    refresh_env_choices()
    shiny::observeEvent(input$refresh_envs, refresh_env_choices())

    shiny::observeEvent(input$launch, {
      env <- input$env
      if (is.null(env) || env == "") {
        return(NULL)
      }
      if (!is.null(state$proc) && state$proc$is_alive()) {
        shiny::showNotification("A run is already in progress.", type = "warning")
        return(NULL)
      }

      ensure_case_studies_snapshot(env)
      config_dir <- env_config_dir(env)
      case_studies_config_dir <- paste0(USER_CASE_STUDIES_DIR, "/")

      analysis_config <- yaml::read_yaml(system.file("conf/analysis_config.yml", package = "RBExT"))
      simulation_config <- yaml::read_yaml(system.file("conf/simulation_config.yml", package = "RBExT"))
      metrics_env <- new.env()
      source(system.file("conf/metrics_config.R", package = "RBExT"), local = metrics_env)

      state$total <- estimate_total_scenarios(env)
      state$env <- env
      state$start_time <- Sys.time()
      state$done <- FALSE
      state$exit_status <- NULL
      state$error_message <- NULL
      state$log_file <- file.path("logs", env, "error_logs", "error_log.log")

      state$proc <- callr::r_bg(
        func = function(pkg_root, wd, env, config_dir, case_studies_config_dir,
                        simulation_config, analysis_config, frequentist_metrics) {
          setwd(wd)
          # devtools::load_all() (rather than requiring RBExT to be
          # installed) mirrors how inst/scripts/main.R already runs the
          # simulation, so the app works the same way in dev mode.
          devtools::load_all(pkg_root, quiet = TRUE)
          run_simulation_env(
            env = env,
            config_dir = config_dir,
            case_studies_config_dir = case_studies_config_dir,
            simulation_config = simulation_config,
            analysis_config = analysis_config,
            frequentist_metrics = frequentist_metrics
          )
        },
        args = list(
          pkg_root = find.package("RBExT"),
          wd = getwd(),
          env = env,
          config_dir = config_dir,
          case_studies_config_dir = case_studies_config_dir,
          simulation_config = simulation_config,
          analysis_config = analysis_config,
          frequentist_metrics = metrics_env$frequentist_metrics
        ),
        stdout = tempfile(fileext = ".out"),
        stderr = tempfile(fileext = ".err")
      )
    })

    shiny::observeEvent(input$cancel, {
      if (!is.null(state$proc) && state$proc$is_alive()) {
        state$proc$kill()
        shiny::showNotification("Run cancelled.", type = "warning")
      }
    })

    poll <- shiny::reactivePoll(
      1500, session,
      checkFunc = function() {
        if (is.null(state$proc)) {
          return(0)
        }
        if (state$done) {
          return(-1)
        }
        c(state$proc$is_alive(), count_result_rows(state$env))
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
        list(
          alive = alive,
          rows = count_result_rows(state$env),
          total = state$total,
          exit_status = state$exit_status,
          error_message = state$error_message
        )
      }
    )

    output$status <- shiny::renderUI({
      p <- poll()
      if (is.null(p)) {
        return(shiny::p("No run started yet."))
      }
      elapsed <- round(difftime(Sys.time(), state$start_time, units = "secs"))
      if (is.na(p$total) || p$total == 0) {
        progress_pct <- NA
      } else {
        progress_pct <- min(100, round(100 * p$rows / p$total))
      }

      status_line <- if (p$alive) {
        sprintf("Running environment '%s' - elapsed %ss - %s rows written%s",
                state$env, elapsed, p$rows,
                if (is.na(progress_pct)) "" else sprintf(" (%s%% of ~%s expected)", progress_pct, p$total))
      } else if (identical(p$exit_status, 0L)) {
        sprintf("Finished environment '%s' in %ss - %s rows written. See it in the Analyze tab.",
                state$env, elapsed, p$rows)
      } else {
        sprintf("Run of '%s' failed after %ss.", state$env, elapsed)
      }

      tags <- list(shiny::p(status_line))
      if (!is.na(progress_pct)) {
        tags <- c(tags, list(shiny::tags$progress(value = progress_pct, max = 100, style = "width:100%")))
      }
      if (!p$alive && !identical(p$exit_status, 0L) && !is.null(p$error_message)) {
        tags <- c(tags, list(shiny::tags$pre(p$error_message)))
      }
      shiny::tagList(tags)
    })

    output$log_tail <- shiny::renderText({
      poll()
      if (is.null(state$log_file) || !file.exists(state$log_file)) {
        return("(no log yet)")
      }
      lines <- tryCatch(readLines(state$log_file, warn = FALSE), error = function(e) character(0))
      paste(utils::tail(lines, 25), collapse = "\n")
    })

    invisible(NULL)
  })
}
