## Analyze/Visualize tab: browse any results/<env>/ directory (produced by
## the Run tab, or copied back from an HPC run) and render plots/tables from
## it. Deliberately decoupled from the Run tab - it only reads from disk.
##
## Note on how the plots are rendered: the underlying plot_*() functions in
## R/plot_*.R are batch report generators - they save PDF/PNG figures
## straight to disk (via export_plots()) and resolve several inputs
## (case_studies_config_dir, methods_dict, figures_dir, plot style constants)
## as free variables that are expected to already be sourced into .GlobalEnv
## (exactly as inst/scripts/plots.R does). They also now return the ggplot
## object(s) they built (a single plot, or a named list when a function loops
## over several parameter combinations), which this module wraps with
## plotly::ggplotly() for interactive zoom/hover/pan. The PDF/PNG files are
## still written as a side effect - useful if you want a publication-quality
## static export - under the path shown below each chart.

analyze_globals_ready <- new.env()

ensure_plot_globals <- function() {
  if (isTRUE(analyze_globals_ready$done)) {
    return(invisible(NULL))
  }
  source(system.file("conf/plots_config.R", package = "RBExT"))
  source(system.file("conf/methods_plots_config.R", package = "RBExT"))
  source(system.file("conf/metrics_config.R", package = "RBExT"))
  analyze_globals_ready$done <- TRUE
  invisible(NULL)
}

analyze_figures_dir <- function(session_token) {
  dir <- file.path(tempdir(), "rbext_shiny_figures", session_token, "")
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  dir
}

#' Point every global the plot_*() functions expect at the right place for
#' this results directory, best-effort: if the env's own methods_config.R /
#' case studies can be found (because it was run through this app, or its
#' config folder happens to sit alongside), use them for correct parameter
#' labels; otherwise fall back to the package's default template so the
#' plots still render, with generic labels.
prepare_plot_globals_for_env <- function(env, figures_dir) {
  ensure_plot_globals()
  assign("figures_dir", figures_dir, envir = .GlobalEnv)
  assign("remake_figures", TRUE, envir = .GlobalEnv)

  methods_config_env <- new.env()
  used_fallback <- TRUE
  config_dir <- tryCatch(env_config_dir(env), error = function(e) "")
  methods_config_path <- if (nzchar(config_dir)) file.path(config_dir, "methods_config.R") else ""
  if (nzchar(methods_config_path) && file.exists(methods_config_path)) {
    tryCatch({
      source(methods_config_path, local = methods_config_env)
      used_fallback <- is.null(methods_config_env$methods_dict)
    }, error = function(e) NULL)
  }
  if (used_fallback) {
    assign("methods_dict", read_methods_template(), envir = .GlobalEnv)
  } else {
    assign("methods_dict", methods_config_env$methods_dict, envir = .GlobalEnv)
  }

  tryCatch(ensure_case_studies_snapshot(env), error = function(e) NULL)
  assign("case_studies_config_dir", paste0(USER_CASE_STUDIES_DIR, "/"), envir = .GlobalEnv)

  invisible(used_fallback)
}

read_results <- function(results_dir) {
  read_if_exists <- function(name, reader) {
    path <- file.path(results_dir, name)
    if (file.exists(path)) tryCatch(reader(path), error = function(e) NULL) else NULL
  }
  list(
    freq = read_if_exists("results_frequentist.csv", function(p) readr::read_csv(p, show_col_types = FALSE)),
    bayes_simpson = read_if_exists("results_bayesian_simpson.csv", function(p) readr::read_csv(p, show_col_types = FALSE)),
    bayes_mc = read_if_exists("results_bayesian_mc.csv", function(p) readr::read_csv(p, show_col_types = FALSE)),
    sweet_spot = read_if_exists("sweet_spot.json", function(p) jsonlite::fromJSON(p))
  )
}

#' Normalize a plot_*() return value into a named list of ggplot objects: a
#' single-plot function returns one ggplot, a looping function (e.g.
#' plot_metric_vs_parameters()) already returns a named list.
as_plot_list <- function(result) {
  if (is.null(result)) {
    return(list())
  }
  if (inherits(result, "ggplot")) {
    return(stats::setNames(list(result), "plot"))
  }
  Filter(function(x) inherits(x, "ggplot"), result)
}

mod_analyze_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        6,
        shiny::selectInput(ns("results_dir"), "Results directory", choices = NULL, width = "100%"),
        shiny::actionButton(ns("refresh_dirs"), "Refresh list"),
        shiny::actionButton(ns("load"), "Load", class = "btn-primary")
      )
    ),
    shiny::hr(),
    shiny::uiOutput(ns("filters")),
    shiny::tabsetPanel(
      shiny::tabPanel(
        "Plots",
        shiny::selectInput(ns("plot_kind"), "Plot", choices = c(
          "Metric vs drift" = "vs_drift",
          "Metric vs sample size" = "vs_sample_size",
          "Metric vs parameters" = "vs_parameters",
          "Bayesian metric vs sample size" = "bayes_vs_sample_size",
          "Bayesian metric vs parameters" = "bayes_vs_parameters"
        )),
        shiny::actionButton(ns("make_plot"), "Generate plot", class = "btn-primary"),
        shiny::uiOutput(ns("plot_gallery"))
      ),
      shiny::tabPanel(
        "Table",
        shiny::downloadButton(ns("download_table"), "Download filtered data as CSV"),
        DT::dataTableOutput(ns("table"))
      )
    )
  )
}

mod_analyze_server <- function(id, just_finished_env = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    session_token <- session$token
    figures_dir <- analyze_figures_dir(session_token)

    refresh_dir_choices <- function(select = NULL) {
      dirs <- list_results_dirs()
      shiny::updateSelectInput(session, "results_dir", choices = dirs, selected = select %||% dirs[1])
    }
    refresh_dir_choices()
    shiny::observeEvent(input$refresh_dirs, refresh_dir_choices())

    if (!is.null(just_finished_env)) {
      shiny::observeEvent(just_finished_env(), {
        env <- just_finished_env()
        if (!is.null(env)) {
          refresh_dir_choices(select = file.path("results", env))
        }
      })
    }

    loaded <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$load, {
      rd <- input$results_dir
      if (is.null(rd) || rd == "") {
        return(NULL)
      }
      res <- read_results(rd)
      if (is.null(res$freq)) {
        shiny::showNotification("No results_frequentist.csv found in that directory.", type = "error")
        return(NULL)
      }
      loaded(list(env = basename(rd), results_dir = rd, data = res))
    })

    output$filters <- shiny::renderUI({
      l <- loaded()
      if (is.null(l)) {
        return(shiny::p("Load a results directory to see filters."))
      }
      df <- l$data$freq
      shiny::fluidRow(
        shiny::column(3, shiny::selectInput(ns("f_case_study"), "Case study", choices = unique(df$case_study))),
        shiny::column(3, shiny::selectInput(ns("f_method"), "Method", choices = unique(df$method))),
        shiny::column(3, shiny::selectInput(ns("f_metric"), "Metric", choices = c(
          "success_proba", "tie", "coverage", "mse", "bias", "precision"
        ))),
        shiny::column(3, shiny::selectInput(ns("f_sample_size"), "Target sample size / arm",
          choices = sort(unique(df$target_sample_size_per_arm))))
      )
    })

    output$table <- DT::renderDataTable({
      l <- loaded()
      if (is.null(l)) {
        return(NULL)
      }
      df <- l$data$freq
      if (!is.null(input$f_case_study)) df <- df[df$case_study == input$f_case_study, ]
      if (!is.null(input$f_method)) df <- df[df$method == input$f_method, ]
      DT::datatable(df, options = list(scrollX = TRUE, pageLength = 15))
    })

    output$download_table <- shiny::downloadHandler(
      filename = function() paste0(loaded()$env, "_filtered.csv"),
      content = function(file) {
        l <- loaded()
        df <- l$data$freq
        if (!is.null(input$f_case_study)) df <- df[df$case_study == input$f_case_study, ]
        if (!is.null(input$f_method)) df <- df[df$method == input$f_method, ]
        readr::write_csv(df, file)
      }
    )

    shiny::observeEvent(input$make_plot, {
      l <- loaded()
      if (is.null(l)) {
        shiny::showNotification("Load a results directory first.", type = "warning")
        return(NULL)
      }
      env <- l$env
      case_study <- input$f_case_study
      method <- input$f_method
      metric <- input$f_metric
      sample_size <- as.numeric(input$f_sample_size)
      analysis_config <- yaml::read_yaml(system.file("conf/analysis_config.yml", package = "RBExT"))

      tryCatch({
        prepare_plot_globals_for_env(env, figures_dir)
        parameters_combinations <- unique(get_parameters(l$data$freq[l$data$freq$method == method, "parameters", drop = FALSE]))

        result <- switch(input$plot_kind,
          vs_drift = plot_metric_vs_drift(
            metric = metric, results_metrics_df = l$data$freq, theta_0 = 0,
            case_study = case_study, method = method, category = "parameters",
            control_drift = FALSE, target_sample_size_per_arm = sample_size,
            parameters_combinations = parameters_combinations,
            xvars = xvars, analysis_config = analysis_config
          ),
          vs_sample_size = plot_metric_vs_sample_size(
            metric = metric, results_metrics_df = l$data$freq, case_study = case_study,
            method = method, parameters_combinations = parameters_combinations[1, , drop = FALSE],
            analysis_config = analysis_config
          ),
          vs_parameters = plot_metric_vs_parameters(
            results_metrics_df = l$data$freq, metric = metric, case_study = case_study,
            method = method, target_sample_size_per_arm = sample_size, analysis_config = analysis_config
          ),
          bayes_vs_sample_size = {
            if (is.null(l$data$bayes_simpson)) stop("No results_bayesian_simpson.csv in this results directory.")
            bayesian_metric_vs_sample_size(
              metric = metric, input_df = l$data$bayes_simpson, case_study = case_study,
              method = method, parameters_combinations = parameters_combinations[1, , drop = FALSE]
            )
          },
          bayes_vs_parameters = {
            if (is.null(l$data$bayes_simpson)) stop("No results_bayesian_simpson.csv in this results directory.")
            bayesian_metric_vs_parameters(
              results_metrics_df = l$data$bayes_simpson, metric = metric, case_study = case_study,
              method = method, target_sample_size_per_arm = sample_size
            )
          }
        )

        plots <- as_plot_list(result)
        if (length(plots) == 0) {
          shiny::showNotification("No plot produced for this combination of filters (often because too few values vary).", type = "warning")
        }

        plot_ids <- paste0("plotly_", seq_along(plots))
        plot_names <- names(plots)
        lapply(seq_along(plots), function(i) {
          local({
            ii <- i
            output[[plot_ids[ii]]] <- plotly::renderPlotly(plotly::ggplotly(plots[[ii]]))
          })
        })

        output$plot_gallery <- shiny::renderUI({
          shiny::tagList(lapply(seq_along(plots), function(i) {
            shiny::tagList(
              plotly::plotlyOutput(ns(plot_ids[i])),
              shiny::tags$p(shiny::tags$small(plot_names[i])),
              shiny::hr()
            )
          }))
        })
      }, error = function(e) {
        shiny::showNotification(paste("Plot error:", conditionMessage(e)), type = "error", duration = NULL)
      })
    })

    invisible(NULL)
  })
}
