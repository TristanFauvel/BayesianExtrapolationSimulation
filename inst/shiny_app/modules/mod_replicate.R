## The Replicate paper page: pick which published figures and tables to
## reproduce, run only the simulations they need, then export them.
##
## The figure/table -> generator mapping lives in R/paper_figures_manifest.R;
## this module is a thin UI over it.

replicate_choice_labels <- function() {
  entries <- paper_manifest()
  labels <- vapply(entries, function(entry) {
    prefix <- if (entry$kind == "table") {
      paste0("Table S", sub("^TS", "", entry$id))
    } else {
      paste0("Figure ", entry$id)
    }
    paste0(prefix, " - ", entry$caption)
  }, character(1))
  stats::setNames(vapply(entries, function(entry) entry$id, character(1)), labels)
}

replicate_main_ids <- function() {
  ids <- paper_manifest_ids()
  ids[!startsWith(ids, "S") & !startsWith(ids, "TS")]
}

mod_replicate_ui <- function(id) {
  ns <- shiny::NS(id)
  rbext_page(
    rbext_step(
      1, "Choose what to reproduce",
      note = "Each item names the generator call that produces it. Coverage is checked against the results directory you pick below.",
      rbext_fields(
        shiny::selectInput(ns("results_dir"), "Results directory", choices = NULL),
        shiny::div(
          rbext_action_button(ns("select_all"), "Select all"),
          rbext_action_button(ns("select_main"), "Main figures only"),
          rbext_action_button(ns("select_none"), "Clear")
        )
      ),
      shiny::checkboxGroupInput(ns("items"), NULL, choices = NULL),
      shiny::uiOutput(ns("coverage"))
    ),
    rbext_step(
      2, "Run the simulations the selection needs",
      note = "Only the case studies and sample sizes your selection plots are simulated. Anything the results directory already covers is skipped.",
      shiny::uiOutput(ns("workload")),
      rbext_action_button(ns("run"), "Run required simulations"),
      shiny::uiOutput(ns("run_state")),
      shiny::verbatimTextOutput(ns("run_log"))
    ),
    rbext_step(
      3, "Produce the figures and tables",
      note = "Figures keep their generated filenames; manifest.csv maps each one to its paper number.",
      rbext_action_button(ns("export"), "Produce figures and tables"),
      shiny::uiOutput(ns("export_status")),
      shiny::tableOutput(ns("export_table"))
    )
  )
}

mod_replicate_server <- function(id, color_mode = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    state <- shiny::reactiveValues(proc = NULL, env = NULL, export = NULL)

    shiny::updateCheckboxGroupInput(
      session, "items",
      choices = replicate_choice_labels(),
      selected = replicate_main_ids()
    )

    refresh_results_dirs <- function() {
      dirs <- list_results_dirs()
      shiny::updateSelectInput(session, "results_dir", choices = dirs)
    }
    refresh_results_dirs()

    case_studies_dir <- function() {
      paste0(system.file("conf/case_studies", package = "RBExT"), "/")
    }

    shiny::observeEvent(input$select_all, {
      shiny::updateCheckboxGroupInput(session, "items", selected = paper_manifest_ids())
    })
    shiny::observeEvent(input$select_main, {
      shiny::updateCheckboxGroupInput(session, "items", selected = replicate_main_ids())
    })
    shiny::observeEvent(input$select_none, {
      shiny::updateCheckboxGroupInput(session, "items", selected = character(0))
    })

    coverage <- shiny::reactive({
      ids <- input$items
      shiny::req(length(ids) > 0)
      dir <- input$results_dir
      if (is.null(dir) || !nzchar(dir)) {
        return(NULL)
      }
      df <- readr::read_csv(
        file.path(dir, "results_frequentist.csv"), show_col_types = FALSE
      )
      paper_replication_coverage(df, ids, case_studies_dir())
    })

    output$coverage <- shiny::renderUI({
      cov <- coverage()
      if (is.null(cov)) {
        return(rbext_empty("Pick a results directory to check coverage."))
      }
      covered <- sum(cov$covered)
      ## A badge per checkbox row is not reachable through
      ## checkboxGroupInput(), so the uncovered items are named here instead.
      uncovered <- cov[!cov$covered, , drop = FALSE]
      shiny::tagList(
        rbext_status(
          sprintf("%d of %d selected items are covered by this results directory.",
                  covered, nrow(cov)),
          ok = covered == nrow(cov)
        ),
        if (nrow(uncovered) > 0) {
          shiny::p(
            class = "rbext-note",
            paste0(
              "Not covered: ",
              paste(uncovered$id, " (", uncovered$reason, ")",
                    sep = "", collapse = "; "),
              "."
            )
          )
        }
      )
    })

    missing_ids <- shiny::reactive({
      cov <- coverage()
      if (is.null(cov)) input$items else cov$id[!cov$covered]
    })

    output$workload <- shiny::renderUI({
      ids <- missing_ids()
      if (length(ids) == 0) {
        return(rbext_status("Nothing to run - the results directory covers everything selected."))
      }
      requirements <- paper_replication_requirements(ids, case_studies_dir())
      shiny::div(
        class = "rbext-workload",
        shiny::strong(sprintf(
          "%d case studies, sample size factors %s, %d replicates, %d drift points",
          length(requirements$case_studies),
          paste(requirements$sample_size_factors, collapse = ", "),
          requirements$n_replicates,
          requirements$ndrift
        ))
      )
    })

    shiny::observeEvent(input$run, {
      ids <- missing_ids()
      if (length(ids) == 0) {
        shiny::showNotification("Nothing to run.", type = "message")
        return()
      }
      if (!is.null(state$proc) && state$proc$is_alive()) {
        shiny::showNotification("A run is already in progress.", type = "warning")
        return()
      }

      requirements <- paper_replication_requirements(ids, case_studies_dir())
      env <- paste0("paper_replication_", format(Sys.time(), "%Y%m%d_%H%M%S"))
      methods_dict <- read_methods_template()[requirements$methods]

      save_environment(
        env = env,
        scenarios_config = requirements,
        mcmc_config = yaml::read_yaml(
          file.path(system.file("conf/combined", package = "RBExT"), "mcmc_config.yml")
        ),
        methods_dict_selected = methods_dict
      )

      state$env <- env
      state$proc <- launch_simulation_run(env)
      shiny::showNotification(paste0("Running ", env, "."), type = "message")
    })

    output$run_state <- shiny::renderUI({
      if (is.null(state$proc)) {
        return(rbext_state("idle"))
      }
      shiny::invalidateLater(2000, session)
      if (state$proc$is_alive()) rbext_state("running", "live") else rbext_state("finished", "done")
    })

    output$run_log <- shiny::renderText({
      shiny::req(state$env)
      shiny::invalidateLater(2000, session)
      log_file <- file.path("logs", state$env, "error_logs", "error_log.log")
      if (!file.exists(log_file)) {
        return("")
      }
      paste(utils::tail(readLines(log_file, warn = FALSE), 40), collapse = "\n")
    })

    shiny::observeEvent(input$export, {
      ids <- input$items
      shiny::req(length(ids) > 0)
      results_dir <- input$results_dir
      shiny::req(nzchar(results_dir))

      run_name <- basename(results_dir)
      figures_dir <- file.path("figures", "publication_figures", run_name, "")
      tables_dir <- file.path("tables", "publication_tables", run_name)

      shiny::withProgress(message = "Producing figures and tables", value = 0, {
        state$export <- export_paper_outputs(
          results_dir = results_dir,
          figures_dir = figures_dir,
          tables_dir = tables_dir,
          ids = ids,
          case_studies_config_dir = case_studies_dir(),
          progress = function(index, total, id) {
            shiny::setProgress(value = index / total, detail = id)
          }
        )
      })
    })

    output$export_status <- shiny::renderUI({
      status <- state$export
      if (is.null(status)) {
        return(rbext_empty("Nothing produced yet."))
      }
      ok <- sum(status$status == "ok")
      rbext_status(
        sprintf("%d of %d produced. Manifest written alongside the tables.",
                ok, nrow(status)),
        ok = ok == nrow(status)
      )
    })

    output$export_table <- shiny::renderTable({
      status <- state$export
      shiny::req(status)
      status[, c("id", "kind", "case_study", "target_sample_size_per_arm",
                 "metric", "status", "message")]
    })
  })
}
