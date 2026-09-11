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
## bexte_plotly() for interactive zoom/hover/pan in the app's colours. The
## PDF/PNG files are still written as a side effect - useful if you want a
## publication-quality static export - under the path shown below each chart.

## The chart the user asks for, and the label it is shown under.
ANALYZE_PLOT_KINDS <- c(
  "Metric vs drift" = "vs_drift",
  "Metric vs sample size" = "vs_sample_size",
  "Metric vs parameters" = "vs_parameters",
  "Bayesian metric vs sample size" = "bayes_vs_sample_size",
  "Bayesian metric vs parameters" = "bayes_vs_parameters",
  "Forest plot" = "forest_plot",
  "Bayesian forest plot" = "bayes_forest_plot"
)

## Plot kinds rendered as a static image (forest_plot()/forest_plot_bayesian()
## build a multi-panel gtable via gridExtra::grid.arrange(), not a single
## ggplot object, so they can't go through bexte_plotly() like the other
## plot_*() functions) rather than through the interactive plotly pipeline.
ANALYZE_IMAGE_PLOT_KINDS <- c("forest_plot", "bayes_forest_plot")

## Which filters each plot kind needs (and which metric list its "Metric"
## dropdown should offer), so the filters shown can depend on the plot kind
## the user picked rather than always showing every filter.
PLOT_KIND_FILTERS <- list(
  vs_drift             = list(method = TRUE,  sample_size = TRUE,  metrics = "frequentist"),
  vs_sample_size       = list(method = TRUE,  sample_size = FALSE, metrics = "frequentist"),
  vs_parameters        = list(method = TRUE,  sample_size = TRUE,  metrics = "frequentist"),
  bayes_vs_sample_size = list(method = TRUE,  sample_size = FALSE, metrics = "bayesian"),
  bayes_vs_parameters  = list(method = TRUE,  sample_size = TRUE,  metrics = "bayesian"),
  forest_plot          = list(method = FALSE, sample_size = TRUE,  metrics = "frequentist"),
  bayes_forest_plot    = list(method = FALSE, sample_size = TRUE,  metrics = "bayesian")
)

#' Named-vector choices for a selectInput("Metric"), labelled with each
#' metric's human-readable label (from conf/metrics_config.R) rather than its
#' raw column name.
metric_choices_for_kind <- function(plot_kind) {
  metrics_list <- if (identical(PLOT_KIND_FILTERS[[plot_kind]]$metrics, "bayesian")) {
    bayesian_metrics
  } else {
    frequentist_metrics
  }
  metric_names <- names(metrics_list)
  fallback_labels <- vapply(metrics_list, function(m) m$label, character(1))
  stats::setNames(
    metric_names,
    mapply(function(name, fallback) bexte_metric_label(name, fallback), metric_names, fallback_labels,
           USE.NAMES = FALSE)
  )
}

available_plot_kinds <- function(results = NULL) {
  choices <- ANALYZE_PLOT_KINDS
  if (is.null(results) || is.null(results$bayes_simpson)) {
    choices <- choices[!(choices %in% c(
      "bayes_vs_sample_size", "bayes_vs_parameters", "bayes_forest_plot"
    ))]
  }
  choices
}

#' Narrow a results data frame to a single case study / sample size / source
#' denominator change factor / target-to-source std ratio combination, the
#' shape forest_plot() and forest_plot_bayesian() expect (mirroring the
#' filtering forest_plot_methods_comparison() does before calling them).
#' Best-effort: a filter is skipped if applying it would empty the data.
narrow_for_forest_plot <- function(df, case_study, sample_size) {
  ## NA-safe equality: plain `==` on an NA propagates NA into the row index,
  ## which would keep an all-NA row instead of dropping it.
  eq <- function(x, value) !is.na(x) & x == value
  narrow_if_nonempty <- function(d, keep) {
    candidate <- d[keep, , drop = FALSE]
    if (nrow(candidate) > 0) candidate else d
  }
  df <- df[eq(df$case_study, case_study) & eq(df$target_sample_size_per_arm, sample_size), , drop = FALSE]
  df <- narrow_if_nonempty(df, eq(df$source_denominator_change_factor, 1))
  if (length(unique(df$target_to_source_std_ratio)) > 1) {
    df <- df[eq(df$target_to_source_std_ratio, unique(df$target_to_source_std_ratio)[1]), , drop = FALSE]
  }
  df
}

#' Build the forest-plot PNG for one colour mode and return its path.
#'
#' Split out of the render observer because the image has to be rebuilt when
#' the light/dark toggle moves, not only when the user asks for a new plot:
#' unlike the plotly charts, a PNG cannot be recoloured after the fact.
forest_image_for_mode <- function(plot_kind, df, metric, mode) {
  palette <- bexte_plot_palette(mode)
  switch(plot_kind,
    forest_plot = forest_plot(df, metric, palette = palette),
    bayes_forest_plot = forest_plot_bayesian(df, metric, palette = palette)
  )
}

analyze_figures_dir <- function(session_token) {
  dir <- file.path(tempdir(), "bexte_shiny_figures", session_token, "")
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  dir
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

filter_results_table <- function(df, case_study = NULL, method = NULL,
                                 sample_size = NULL, metric = NULL,
                                 filter_method = TRUE, filter_sample_size = TRUE) {
  if (!is.null(case_study)) df <- df[df$case_study == case_study, , drop = FALSE]
  if (filter_method && !is.null(method)) df <- df[df$method == method, , drop = FALSE]
  if (filter_sample_size && !is.null(sample_size)) {
    df <- df[df$target_sample_size_per_arm == as.numeric(sample_size), , drop = FALSE]
  }

  scenario_columns <- c(
    "case_study", "method", "target_sample_size_per_arm", "drift",
    "source_denominator_change_factor", "target_to_source_std_ratio", "parameters"
  )
  metric_columns <- if (is.null(metric)) character(0) else {
    names(df)[names(df) == metric |
      startsWith(names(df), paste0("mcse_", metric)) |
      startsWith(names(df), paste0("conf_int_", metric))]
  }
  keep <- unique(c(intersect(scenario_columns, names(df)), metric_columns))
  df[, keep, drop = FALSE]
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
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 320,
      resizable = FALSE,
      title = shiny::tags$h1("Analyze results", class = "sidebar-title"),
      open = list(desktop = "always", mobile = "always-above"),
      shiny::selectInput(ns("results_dir"), "Results set", choices = NULL, width = "100%"),
      shiny::div(
        class = "bexte-actions",
        bexte_action_button(ns("refresh_dirs"), "Refresh results")
      ),
      shiny::uiOutput(ns("loaded_status")),
      shiny::uiOutput(ns("plot_kind_input")),
      shiny::uiOutput(ns("filters")),
      shiny::uiOutput(ns("plot_action"))
    ),
    bslib::navset_card_tab(
      bslib::nav_panel(
        "Plots",
        shiny::uiOutput(ns("plot_gallery"))
      ),
      bslib::nav_panel(
        "Table",
        shiny::div(
          class = "bexte-actions",
          style = "margin-bottom: 0.75rem;",
          shiny::uiOutput(ns("download_action"))
        ),
        DT::dataTableOutput(ns("table"))
      )
    )
  )
}

mod_analyze_server <- function(id, just_finished_env = NULL, color_mode = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    session_token <- session$token
    figures_dir <- analyze_figures_dir(session_token)
    current_mode <- if (is.null(color_mode)) shiny::reactive("light") else color_mode

    ## Needed up front (before any plot is generated) so the "Metric" filter
    ## can already offer the right choices for whichever plot kind is picked.
    ensure_plot_globals()
    loaded <- shiny::reactiveVal(NULL)
    load_error <- shiny::reactiveVal(NULL)
    plot_busy <- shiny::reactiveVal(FALSE)
    has_plot <- shiny::reactiveVal(FALSE)
    plot_content <- shiny::reactiveVal(bexte_empty("Choose a plot and generate it to see it here."))
    ## What the on-screen forest PNG was built from, so it can be rebuilt in the
    ## other colour scheme when the theme toggle moves.
    forest_spec <- shiny::reactiveVal(NULL)

    refresh_dir_choices <- function(select = NULL) {
      dirs <- list_results_dirs()
      choices <- stats::setNames(dirs, basename(dirs))
      selected <- select %||% if (length(dirs) > 0) dirs[[1]] else character(0)
      shiny::updateSelectInput(session, "results_dir", choices = choices, selected = selected)
    }
    refresh_dir_choices()
    shiny::observeEvent(input$refresh_dirs, {
      selected <- input$results_dir
      refresh_dir_choices(select = selected)
      session$onFlushed(function() load_results(selected), once = TRUE)
    })

    if (!is.null(just_finished_env)) {
      shiny::observeEvent(just_finished_env(), {
        env <- just_finished_env()
        if (!is.null(env)) {
          results_dir <- file.path("results", env)
          refresh_dir_choices(select = results_dir)
          session$onFlushed(function() load_results(results_dir), once = TRUE)
        }
      })
    }

    load_results <- function(rd) {
      loaded(NULL)
      load_error(NULL)
      has_plot(FALSE)
      plot_content(bexte_empty("Choose a plot and generate it to see it here."))
      if (is.null(rd) || rd == "") {
        return(invisible(NULL))
      }
      res <- shiny::withProgress(message = "Loading results…", value = 0.5, read_results(rd))
      if (is.null(res$freq)) {
        load_error("The frequentist results file could not be read.")
        return(invisible(NULL))
      }
      loaded(list(env = basename(rd), results_dir = rd, data = res))
      invisible(NULL)
    }

    shiny::observeEvent(input$results_dir, {
      load_results(input$results_dir)
    })

    output$loaded_status <- shiny::renderUI({
      if (!is.null(load_error())) return(bexte_status(load_error(), ok = FALSE))
      l <- loaded()
      if (is.null(l)) {
        return(shiny::p(class = "bexte-note", "Choose a results set to begin."))
      }
      files <- "Frequentist results"
      if (!is.null(l$data$bayes_simpson)) files <- paste(files, "and Bayesian results")
      shiny::div(
        class = "bexte-loaded", role = "status",
        shiny::strong(sprintf("Loaded %s", l$env)),
        shiny::span(files)
      )
    })

    output$plot_kind_input <- shiny::renderUI({
      l <- loaded()
      choices <- available_plot_kinds(if (is.null(l)) NULL else l$data)
      current <- shiny::isolate(input$plot_kind)
      selected <- if (!is.null(current) && current %in% choices) current else choices[[1]]
      shiny::selectInput(session$ns("plot_kind"), "Plot", choices = choices, selected = selected)
    })

    output$filters <- shiny::renderUI({
      l <- loaded()
      if (is.null(l)) {
        return(shiny::p(class = "bexte-note", "Load a results directory to filter it."))
      }
      plot_kind <- input$plot_kind %||% ANALYZE_PLOT_KINDS[[1]]
      spec <- PLOT_KIND_FILTERS[[plot_kind]] %||% PLOT_KIND_FILTERS[[1]]
      df <- if (identical(spec$metrics, "bayesian")) l$data$bayes_simpson else l$data$freq
      metric_choices <- metric_choices_for_kind(plot_kind)
      metric_choices <- metric_choices[metric_choices %in% names(df)]

      shiny::tagList(
        shiny::selectInput(ns("f_case_study"), "Case study", choices = unique(df$case_study)),
        if (spec$method) {
          shiny::selectInput(ns("f_method"), "Method", choices = bexte_method_choices(unique(df$method)))
        },
        shiny::selectInput(ns("f_metric"), "Metric", choices = metric_choices),
        if (spec$sample_size) {
          shiny::selectInput(ns("f_sample_size"), "Target sample size per arm",
            choices = sort(unique(df$target_sample_size_per_arm)))
        }
      )
    })

    filtered_table <- shiny::reactive({
      l <- loaded()
      if (is.null(l)) {
        return(NULL)
      }
      plot_kind <- input$plot_kind %||% ANALYZE_PLOT_KINDS[[1]]
      spec <- PLOT_KIND_FILTERS[[plot_kind]] %||% PLOT_KIND_FILTERS[[1]]
      df <- if (identical(spec$metrics, "bayesian")) l$data$bayes_simpson else l$data$freq
      filter_results_table(
        df,
        case_study = input$f_case_study,
        method = input$f_method,
        sample_size = input$f_sample_size,
        metric = input$f_metric,
        filter_method = spec$method,
        filter_sample_size = spec$sample_size
      )
    })

    output$table <- DT::renderDataTable({
      df <- filtered_table()
      if (is.null(df)) {
        return(NULL)
      }
      DT::datatable(
        transform(df, method = if ("method" %in% names(df)) {
          vapply(method, bexte_method_label, character(1))
        } else NULL),
        style = "bootstrap5",
        class = "table table-sm",
        rownames = FALSE,
        colnames = unname(vapply(names(df), bexte_column_label, character(1))),
        options = list(scrollX = TRUE, pageLength = 15)
      )
    })

    output$download_table <- shiny::downloadHandler(
      filename = function() {
        shiny::req(loaded())
        paste0(loaded()$env, "_filtered.csv")
      },
      content = function(file) {
        shiny::req(filtered_table())
        readr::write_csv(filtered_table(), file)
      }
    )

    output$plot_gallery <- shiny::renderUI({
      plot_content()
    })

    output$plot_action <- shiny::renderUI({
      bexte_action_button(
        session$ns("make_plot"),
        if (plot_busy()) "Generating…" else "Generate plot",
        class = "btn-primary",
        disabled = is.null(loaded()) || plot_busy()
      )
    })

    output$download_action <- shiny::renderUI({
      bexte_download_button(
        session$ns("download_table"), "Download as CSV", class = "btn-default",
        disabled = is.null(loaded())
      )
    })

    shiny::observeEvent(
      list(input$plot_kind, input$f_case_study, input$f_method, input$f_metric, input$f_sample_size),
      {
        if (!plot_busy() && !is.null(loaded()) && has_plot()) {
          has_plot(FALSE)
          plot_content(bexte_empty("Controls changed. Generate the plot to update this view."))
        }
      },
      ignoreInit = TRUE
    )

    ## The plotly charts recolour themselves because renderPlotly() reads
    ## current_mode(). A forest plot is a static PNG, so following the toggle
    ## means drawing it again in the other scheme.
    shiny::observeEvent(current_mode(), {
      spec <- forest_spec()
      if (is.null(spec) || !has_plot()) {
        return(NULL)
      }
      tryCatch({
        shiny::withProgress(message = "Recolouring plot…", value = 0.5, {
          prepare_plot_globals_for_env(spec$env, figures_dir)
          image_path <- forest_image_for_mode(spec$kind, spec$df, spec$metric, current_mode())
          if (!is.null(image_path) && file.exists(image_path)) {
            output$forest_image <- shiny::renderImage(
              list(src = image_path, contentType = "image/png", width = "100%"),
              deleteFile = FALSE
            )
          }
        })
      }, error = function(e) {
        shiny::showNotification(paste("Plot error:", conditionMessage(e)), type = "error", duration = NULL)
      })
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$make_plot, {
      l <- loaded()
      if (is.null(l)) {
        shiny::showNotification("Load a results directory first.", type = "warning")
        return(NULL)
      }
      env <- l$env
      spec <- PLOT_KIND_FILTERS[[input$plot_kind]] %||% PLOT_KIND_FILTERS[[1]]
      case_study <- input$f_case_study
      method <- if (spec$method) input$f_method else NULL
      metric <- input$f_metric
      sample_size <- if (spec$sample_size) as.numeric(input$f_sample_size) else NA_real_
      shiny::req(case_study, metric)
      if (spec$method) shiny::req(method)
      if (spec$sample_size) shiny::req(sample_size)
      analysis_config <- yaml::read_yaml(system.file("conf/analysis_config.yml", package = "BExTE"))
      metric_labels <- metric_choices_for_kind(input$plot_kind)
      metric_label <- names(metric_labels)[match(metric, metric_labels)]
      if (length(metric_label) == 0 || is.na(metric_label)) metric_label <- metric
      context <- paste(
        c(case_study, if (!is.null(method)) bexte_method_label(method), metric_label),
        collapse = " · "
      )
      plot_busy(TRUE)
      on.exit(plot_busy(FALSE), add = TRUE)

      shiny::withProgress(message = "Generating plot…", value = 0.15, {
        if (input$plot_kind %in% ANALYZE_IMAGE_PLOT_KINDS) {
        tryCatch({
          prepare_plot_globals_for_env(env, figures_dir)
          shiny::incProgress(0.35)

          if (identical(input$plot_kind, "bayes_forest_plot") && is.null(l$data$bayes_simpson)) {
            stop("No results_bayesian_simpson.csv in this results directory.")
          }
          forest_df <- narrow_for_forest_plot(
            if (identical(input$plot_kind, "bayes_forest_plot")) l$data$bayes_simpson else l$data$freq,
            case_study, sample_size
          )
          ## Kept so the light/dark observer below can rebuild the PNG without
          ## re-deriving any of it.
          forest_spec(list(kind = input$plot_kind, df = forest_df, metric = metric, env = env))
          image_path <- forest_image_for_mode(input$plot_kind, forest_df, metric, current_mode())

          if (is.null(image_path) || !file.exists(image_path)) {
            shiny::showNotification("No plot produced for this combination of filters (often because too few values vary).", type = "warning")
            plot_content(bexte_empty("Nothing to plot for these filters. Try another method or metric."))
            forest_spec(NULL)
            has_plot(FALSE)
            return(NULL)
          }

          kind_label <- names(ANALYZE_PLOT_KINDS)[match(input$plot_kind, ANALYZE_PLOT_KINDS)]
          output$forest_image <- shiny::renderImage(
            list(src = image_path, contentType = "image/png", width = "100%"),
            deleteFile = FALSE
          )
          plot_content(
            shiny::tagList(
              bslib::card(
                style = "margin-bottom: 1rem;",
                bslib::card_header(
                  shiny::div(kind_label, shiny::span(class = "bexte-plot-context", context))
                ),
                bslib::card_body(shiny::imageOutput(ns("forest_image")))
              )
            )
          )
          has_plot(TRUE)
        }, error = function(e) {
          shiny::showNotification(paste("Plot error:", conditionMessage(e)), type = "error", duration = NULL)
          plot_content(bexte_empty("The plot could not be generated. Review the filters and try again."))
          forest_spec(NULL)
          has_plot(FALSE)
        })
        return(NULL)
      }

      tryCatch({
        prepare_plot_globals_for_env(env, figures_dir)
        shiny::incProgress(0.35)
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

        ## A single-plot function has nothing better to call itself than
        ## "plot", so fall back to the name of the chart the user picked.
        kind_label <- names(ANALYZE_PLOT_KINDS)[match(input$plot_kind, ANALYZE_PLOT_KINDS)]
        plot_ids <- paste0("plotly_", seq_along(plots))
        plot_names <- names(plots)
        plot_names[plot_names == "plot"] <- kind_label

        lapply(seq_along(plots), function(i) {
          local({
            ii <- i
            ## Reading the colour scheme here (rather than baking it in) is
            ## what makes the charts follow the light/dark toggle.
            output[[plot_ids[ii]]] <- plotly::renderPlotly(bexte_plotly(plots[[ii]], current_mode()))
          })
        })

        plot_content(
          if (length(plots) == 0) bexte_empty("Nothing to plot for these filters. Try another method or metric.") else shiny::tagList(
            lapply(seq_along(plots), function(i) {
              bslib::card(
                style = "margin-bottom: 1rem;",
                bslib::card_header(
                  shiny::div(plot_names[i], shiny::span(class = "bexte-plot-context", context))
                ),
                bslib::card_body(plotly::plotlyOutput(ns(plot_ids[i])))
              )
            })
          )
        )
        has_plot(length(plots) > 0)
      }, error = function(e) {
        shiny::showNotification(paste("Plot error:", conditionMessage(e)), type = "error", duration = NULL)
        plot_content(bexte_empty("The plot could not be generated. Review the filters and try again."))
        has_plot(FALSE)
      })
      })
    })

    invisible(NULL)
  })
}
