## Configure tab: build a case study and/or a full simulation environment
## (scenarios_config.yml / mcmc_config.yml / methods_config.R), saved under
## user_configs/ - see helpers.R for the save/read functions.
##
## The five sections are numbered because they are a sequence: each one feeds
## the next, and the last one writes the environment to disk.

mod_configure_ui <- function(id) {
  ns <- shiny::NS(id)
  bexte_page(
    shiny::tags$h1("Configure simulation", class = "visually-hidden"),
    shiny::p(
      class = "bexte-lede",
      "Define the trials to simulate, the borrowing methods to compare and the ",
      "grid of scenarios to run them over. Saving writes an environment under ",
      "user_configs/, ready to launch from the Run tab."
    ),

    bexte_step(
      1, "Case study",
      note = "Pick one of the case studies already available, or add your own. New case studies can use a binary or continuous endpoint.",
      shiny::radioButtons(
        ns("case_study_mode"), "Case study source",
        choices = c("Use existing" = "existing", "Create new" = "new"),
        inline = TRUE
      ),
      shiny::conditionalPanel(
        "input.case_study_mode == 'new'", ns = ns,
        bexte_fields(
          bexte_name_input(ns("cs_name"), "Name"),
          shiny::textInput(ns("cs_control_arm"), "Control arm name", value = "Placebo"),
          shiny::selectInput(ns("cs_endpoint"), "Endpoint", choices = c(
            "Binary" = "binary", "Continuous" = "continuous"
          ))
        ),
        bexte_fields(
          shiny::selectInput(ns("cs_null_space"), "Null hypothesis direction", choices = c(
            "Effect at or below boundary" = "left",
            "Effect at or above boundary" = "right"
          )),
          shiny::numericInput(ns("cs_theta_0"), "Null hypothesis boundary (θ₀)", value = 0)
        ),

        bexte_subhead("Target study"),
        bexte_fields(
          shiny::numericInput(ns("cs_target_control_n"), "Control sample size", value = 50, min = 1, step = 1),
          shiny::numericInput(ns("cs_target_treatment_n"), "Treatment sample size", value = 50, min = 1, step = 1)
        ),
        shiny::conditionalPanel(
          "input.cs_endpoint == 'binary'", ns = ns,
          bexte_fields(
            shiny::numericInput(ns("cs_target_control_resp"), "Control responses", value = 20, min = 0, max = 50, step = 1),
            shiny::numericInput(ns("cs_target_treatment_resp"), "Treatment responses", value = 30, min = 0, max = 50, step = 1)
          )
        ),
        shiny::conditionalPanel(
          "input.cs_endpoint == 'continuous'", ns = ns,
          bexte_fields(
            shiny::numericInput(ns("cs_target_effect"), "Treatment effect", value = 0.1),
            shiny::numericInput(ns("cs_target_se"), "Standard error", value = 0.1, min = 1e-12)
          )
        ),

        bexte_subhead("Source study"),
        bexte_fields(
          shiny::numericInput(ns("cs_source_control_n"), "Control sample size", value = 200, min = 1, step = 1),
          shiny::numericInput(ns("cs_source_treatment_n"), "Treatment sample size", value = 200, min = 1, step = 1)
        ),
        shiny::conditionalPanel(
          "input.cs_endpoint == 'binary'", ns = ns,
          bexte_fields(
            shiny::numericInput(ns("cs_source_control_resp"), "Control responses", value = 80, min = 0, max = 200, step = 1),
            shiny::numericInput(ns("cs_source_treatment_resp"), "Treatment responses", value = 110, min = 0, max = 200, step = 1)
          )
        ),
        shiny::conditionalPanel(
          "input.cs_endpoint == 'continuous'", ns = ns,
          bexte_fields(
            shiny::numericInput(ns("cs_source_effect"), "Treatment effect", value = 0.1),
            shiny::numericInput(ns("cs_source_se"), "Standard error", value = 0.07, min = 1e-12)
          )
        ),

        shiny::div(
          class = "bexte-actions",
          bexte_action_button(ns("save_case_study"), "Save case study", class = "btn-primary")
        ),
        shiny::uiOutput(ns("case_study_status"))
      )
    ),

    bexte_step(
      2, "Case studies and methods to compare",
      note = "Everything selected here is run against every scenario in the grid below.",
      bexte_split(
        shiny::uiOutput(ns("case_studies_picker")),
        shiny::checkboxGroupInput(ns("methods"), "Methods to compare", choices = NULL)
      ),
      shiny::uiOutput(ns("method_params_ui"))
    ),

    bexte_step(
      3, "Scenario grid",
      note = "The simulation runs every combination of these settings, for each case study and method.",
      bexte_fields(
        shiny::numericInput(ns("n_replicates"), "Monte Carlo replicates", value = 1000, min = 1, step = 1),
        shiny::numericInput(ns("ndrift"), "Number of drift points", value = 15, min = 1, step = 1),
        shiny::textInput(ns("sample_size_factors"), "Sample size factors", value = "1, 2, 4"),
        shiny::textInput(ns("denominator_change_factor"), "Denominator change factor", value = "1"),
        shiny::textInput(ns("target_to_source_std_ratio_range"), "Target/source SD ratio range", value = "1")
      ),
      shiny::p(class = "bexte-note", "Enter multiple factor or ratio values separated by commas."),
      shiny::uiOutput(ns("workload_preview")),
      shiny::checkboxInput(ns("parallelization"), "Run scenarios in parallel", value = FALSE)
    ),

    bexte_step(
      4, "MCMC settings",
      note = "Used only by the methods that need sampling: RMP, NPP, commensurate power prior and friends.",
      bexte_fields(
        shiny::numericInput(ns("num_chains"), "MCMC chains", value = 4, min = 1, step = 1),
        shiny::numericInput(ns("chain_length"), "Iterations per chain", value = 2000, min = 1, step = 1),
        shiny::numericInput(ns("tune"), "Warm-up iterations", value = 1000, min = 0, step = 1),
        shiny::numericInput(ns("target_ess"), "Target effective sample size", value = 2000, min = 1, step = 1),
        shiny::numericInput(ns("rhat_threshold"), "Rhat threshold", value = 1.1, min = 1)
      )
    ),

    bexte_step(
      5, "Save the environment",
      note = "Saved environments live in user_configs/ and appear in the Run tab.",
      bexte_fields(
        bexte_name_input(ns("env_name"), "Environment name")
      ),
      shiny::div(
        class = "bexte-actions",
        bexte_action_button(ns("save_env"), "Save environment", class = "btn-primary")
      ),
      shiny::uiOutput(ns("env_status"))
    )
  )
}

## on_env_saved: called (with no arguments) whenever an environment is
## saved, so the Run tab can refresh its environment picker without a
## manual "Refresh the list" button.
mod_configure_server <- function(id, on_env_saved = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    methods_template <- read_methods_template()

    refresh_case_study_choices <- function() {
      list_case_studies()
    }
    case_studies_rv <- shiny::reactiveVal(refresh_case_study_choices())

    output$case_studies_picker <- shiny::renderUI({
      cs <- case_studies_rv()
      choices <- cs$name
      names(choices) <- paste0(cs$name, " (", cs$source, ")")
      shiny::checkboxGroupInput(
        ns("case_studies"), "Case studies to include", choices = choices,
        selected = intersect(input$case_studies %||% character(), choices)
      )
    })

    shiny::observe({
      methods <- names(methods_template)
      shiny::updateCheckboxGroupInput(
        session, "methods", choices = bexte_method_choices(methods),
        selected = intersect(input$methods %||% character(), methods)
      )
    })

    shiny::observe({
      shiny::updateNumericInput(session, "cs_target_control_resp", max = input$cs_target_control_n)
      shiny::updateNumericInput(session, "cs_target_treatment_resp", max = input$cs_target_treatment_n)
      shiny::updateNumericInput(session, "cs_source_control_resp", max = input$cs_source_control_n)
      shiny::updateNumericInput(session, "cs_source_treatment_resp", max = input$cs_source_treatment_n)
    })

    validate_case_study_form <- function() {
      bexte_validate_name(input$cs_name, "case study")
      if (is.null(input$cs_control_arm) || !nzchar(trimws(input$cs_control_arm))) {
        stop("Control arm name cannot be empty.", call. = FALSE)
      }
      sample_sizes <- c(
        "Target control sample size" = input$cs_target_control_n,
        "Target treatment sample size" = input$cs_target_treatment_n,
        "Source control sample size" = input$cs_source_control_n,
        "Source treatment sample size" = input$cs_source_treatment_n
      )
      invisible(Map(function(value, label) {
        bexte_validate_number(value, label, min = 1, whole = TRUE)
      }, sample_sizes, names(sample_sizes)))
      bexte_validate_number(input$cs_theta_0, "Null hypothesis boundary")

      if (identical(input$cs_endpoint, "binary")) {
        response_specs <- list(
          list(input$cs_target_control_resp, "Target control responses", input$cs_target_control_n),
          list(input$cs_target_treatment_resp, "Target treatment responses", input$cs_target_treatment_n),
          list(input$cs_source_control_resp, "Source control responses", input$cs_source_control_n),
          list(input$cs_source_treatment_resp, "Source treatment responses", input$cs_source_treatment_n)
        )
        invisible(lapply(response_specs, function(spec) {
          bexte_validate_number(spec[[1]], spec[[2]], min = 0, max = spec[[3]], whole = TRUE)
        }))
      } else {
        bexte_validate_number(input$cs_target_effect, "Target treatment effect")
        bexte_validate_number(input$cs_source_effect, "Source treatment effect")
        bexte_validate_number(input$cs_target_se, "Target standard error", min = 0, strict_min = TRUE)
        bexte_validate_number(input$cs_source_se, "Source standard error", min = 0, strict_min = TRUE)
      }
      invisible(TRUE)
    }

    save_case_study <- function(overwrite = FALSE) {
      status <- tryCatch({
        validate_case_study_form()
        existing <- case_study_path(input$cs_name)
        if (!overwrite && nzchar(existing) && file.exists(existing)) {
          shiny::showModal(shiny::modalDialog(
            title = "Replace case study?",
            sprintf("A case study named '%s' already exists. Saving will replace it with these values.", input$cs_name),
            footer = shiny::tagList(
              shiny::modalButton("Keep existing"),
              bexte_action_button(ns("confirm_case_study_overwrite"), "Replace case study", class = "btn-danger")
            ),
            easyClose = FALSE
          ))
          return(NULL)
        }
        if (input$cs_endpoint == "binary") {
          build_and_save_case_study(
            name = input$cs_name, control_arm_name = input$cs_control_arm,
            endpoint = "binary", null_space = input$cs_null_space, theta_0 = input$cs_theta_0,
            target_control_n = input$cs_target_control_n, target_treatment_n = input$cs_target_treatment_n,
            source_control_n = input$cs_source_control_n, source_treatment_n = input$cs_source_treatment_n,
            target_control_responses = input$cs_target_control_resp, target_treatment_responses = input$cs_target_treatment_resp,
            source_control_responses = input$cs_source_control_resp, source_treatment_responses = input$cs_source_treatment_resp
          )
        } else {
          build_and_save_case_study(
            name = input$cs_name, control_arm_name = input$cs_control_arm,
            endpoint = "continuous", null_space = input$cs_null_space, theta_0 = input$cs_theta_0,
            target_control_n = input$cs_target_control_n, target_treatment_n = input$cs_target_treatment_n,
            source_control_n = input$cs_source_control_n, source_treatment_n = input$cs_source_treatment_n,
            target_treatment_effect = input$cs_target_effect, target_standard_error = input$cs_target_se,
            source_treatment_effect = input$cs_source_effect, source_standard_error = input$cs_source_se
          )
        }
        case_studies_rv(refresh_case_study_choices())
        selected <- unique(c(input$case_studies %||% character(), input$cs_name))
        session$onFlushed(function() {
          shiny::updateCheckboxGroupInput(session, "case_studies", selected = selected)
        }, once = TRUE)
        list(ok = TRUE, message = sprintf("Saved and selected '%s' for this environment.", input$cs_name))
      }, error = function(e) list(ok = FALSE, message = conditionMessage(e)))
      if (is.null(status)) return(invisible(NULL))
      output$case_study_status <- shiny::renderUI(bexte_status(status$message, ok = status$ok))
      invisible(NULL)
    }

    shiny::observeEvent(input$save_case_study, save_case_study())
    shiny::observeEvent(input$confirm_case_study_overwrite, {
      shiny::removeModal()
      save_case_study(overwrite = TRUE)
    })

    ## ---- Per-method parameter range editors --------------------------------

    format_range <- function(range) {
      paste(vapply(range, function(x) paste(x, collapse = ","), character(1)), collapse = "; ")
    }
    parse_range <- function(text, original_first_value, label, parameter_type = NULL,
                            parameter_id = NULL) {
      parameter_id <- parameter_id %||% ""
      parts <- trimws(strsplit(text, "[;,]")[[1]])
      parts <- parts[parts != ""]
      if (length(parts) == 0) {
        stop(sprintf("%s needs at least one value.", label), call. = FALSE)
      }
      as_one <- function(p) {
        if (is.logical(original_first_value)) {
          normalized <- toupper(p)
          if (!(normalized %in% c("TRUE", "FALSE"))) {
            stop(sprintf("%s accepts only TRUE or FALSE.", label), call. = FALSE)
          }
          return(identical(normalized, "TRUE"))
        }
        if (is.numeric(original_first_value)) {
          num <- suppressWarnings(as.numeric(p))
          if (is.na(num) || !is.finite(num)) {
            stop(sprintf("Every %s value must be numeric.", tolower(label)), call. = FALSE)
          }
          return(num)
        }
        p
      }
      values <- lapply(parts, as_one)
      numeric_values <- unlist(values)
      probability_parameters <- c(
        "prior_weight", "power_parameter", "power_parameter_mean",
        "desired_tie", "significance_level"
      )
      if (is.numeric(original_first_value) && parameter_id %in% probability_parameters &&
          any(numeric_values < 0 | numeric_values > 1)) {
        stop(sprintf("Every %s value must be between 0 and 1.", tolower(label)), call. = FALSE)
      }
      positive_parameters <- c(
        "power_parameter_std", "shape_parameter", "equivalence_margin",
        "tolerance", "n_iter"
      )
      if (is.numeric(original_first_value) && parameter_id %in% positive_parameters &&
          any(numeric_values <= 0)) {
        stop(sprintf("Every %s value must be greater than zero.", tolower(label)), call. = FALSE)
      }
      if (identical(parameter_type, "integer") && any(numeric_values != floor(numeric_values))) {
        stop(sprintf("Every %s value must be a whole number.", tolower(label)), call. = FALSE)
      }
      values
    }

    output$method_params_ui <- shiny::renderUI({
      selected <- input$methods
      if (is.null(selected) || length(selected) == 0) {
        return(NULL)
      }
      panels <- lapply(selected, function(method) {
        params <- methods_template[[method]]
        rows <- lapply(names(params), function(pname) {
          p <- params[[pname]]
          input_id <- ns(paste0("range_", method, "_", pname))
          if (identical(pname, "heterogeneity_prior")) {
            shiny::helpText(sprintf("%s: using the default set of priors (not editable here).", p$parameter_name %||% pname))
          } else {
            shiny::tagList(
              shiny::textInput(
                input_id,
                label = p$parameter_name %||% pname,
                value = format_range(p$range)
              ),
              shiny::helpText("Separate multiple values with commas.")
            )
          }
        })
        bslib::card(
          bslib::card_header(bexte_method_label(method)),
          bslib::card_body(rows)
        )
      })
      shiny::tagList(
        bexte_subhead("Parameter ranges to test"),
        shiny::div(class = "bexte-card-grid", panels)
      )
    })

    build_methods_dict_selected <- function() {
      selected <- input$methods
      out <- list()
      for (method in selected) {
        params <- methods_template[[method]]
        new_params <- params
        for (pname in names(params)) {
          if (identical(pname, "heterogeneity_prior")) {
            next
          }
          input_id <- paste0("range_", method, "_", pname)
          text <- input[[input_id]]
          if (!is.null(text)) {
            new_params[[pname]]$range <- parse_range(
              text,
              params[[pname]]$range[[1]],
              params[[pname]]$parameter_name %||% pname,
              params[[pname]]$type,
              pname
            )
          }
        }
        out[[method]] <- new_params
      }
      out
    }

    ## ---- Save environment ---------------------------------------------------

    build_environment_form <- function(require_name = TRUE) {
      if (require_name) {
        bexte_validate_name(input$env_name, "environment")
      }
      if (is.null(input$case_studies) || length(input$case_studies) == 0) {
        stop("Select at least one case study in step 2.", call. = FALSE)
      }
      if (is.null(input$methods) || length(input$methods) == 0) {
        stop("Select at least one method in step 2.", call. = FALSE)
      }

      bexte_validate_number(input$n_replicates, "Monte Carlo replicates", min = 1, whole = TRUE)
      bexte_validate_number(input$ndrift, "Number of drift points", min = 1, whole = TRUE)
      bexte_validate_number(input$num_chains, "MCMC chains", min = 1, whole = TRUE)
      bexte_validate_number(input$chain_length, "Iterations per chain", min = 1, whole = TRUE)
      bexte_validate_number(input$tune, "Warm-up iterations", min = 0, whole = TRUE)
      bexte_validate_number(input$target_ess, "Target effective sample size", min = 1, whole = TRUE)
      bexte_validate_number(input$rhat_threshold, "Rhat threshold", min = 1)

      sample_sizes <- bexte_parse_number_list(input$sample_size_factors, "Sample size factors")
      denominator_changes <- bexte_parse_number_list(
        input$denominator_change_factor, "Denominator change factor"
      )
      std_ratios <- bexte_parse_number_list(
        input$target_to_source_std_ratio_range, "Target/source SD ratio range"
      )
      methods <- build_methods_dict_selected()

      list(
        scenarios = list(
          n_replicates = input$n_replicates,
          ndrift = input$ndrift,
          denominator_change_factor = as.list(denominator_changes),
          sample_size_factors = as.list(sample_sizes),
          target_to_source_std_ratio_range = as.list(std_ratios),
          parallelization = isTRUE(input$parallelization),
          case_studies = as.list(input$case_studies),
          methods = as.list(input$methods)
        ),
        mcmc = list(
          num_chains = input$num_chains,
          parallel_chains = input$num_chains,
          tune = input$tune,
          target_accept = 0.8,
          chain_length = input$chain_length,
          max_chain_length = input$chain_length * 2,
          target_ess = input$target_ess,
          rhat_threshold = input$rhat_threshold,
          max_divergence_rate = 0.01
        ),
        methods = methods,
        parsed = list(
          sample_sizes = sample_sizes,
          denominator_changes = denominator_changes,
          std_ratios = std_ratios
        )
      )
    }

    output$workload_preview <- shiny::renderUI({
      if (length(input$case_studies %||% character()) == 0 ||
          length(input$methods %||% character()) == 0) {
        return(shiny::p(class = "bexte-note", "Select case studies and methods to preview the workload."))
      }
      tryCatch({
        values <- build_environment_form(require_name = FALSE)
        workload <- estimate_configured_workload(
          input$case_studies, values$methods, input$ndrift,
          values$parsed$sample_sizes, values$parsed$denominator_changes,
          values$parsed$std_ratios, input$n_replicates
        )
        shiny::div(
          class = "bexte-workload", role = "status", `aria-live` = "polite",
          shiny::strong(sprintf("About %s scenarios", format(workload$scenarios, big.mark = ","))),
          shiny::span(sprintf(" · %s Monte Carlo evaluations", format(workload$evaluations, big.mark = ",")))
        )
      }, error = function(e) bexte_status(conditionMessage(e), ok = FALSE))
    })

    save_environment_form <- function(overwrite = FALSE) {
      status <- tryCatch({
        values <- build_environment_form()
        existing <- input$env_name %in% list_environments()$name
        if (!overwrite && existing) {
          shiny::showModal(shiny::modalDialog(
            title = "Replace environment?",
            sprintf("An environment named '%s' already exists. Saving will replace its configuration.", input$env_name),
            footer = shiny::tagList(
              shiny::modalButton("Keep existing"),
              bexte_action_button(ns("confirm_env_overwrite"), "Replace environment", class = "btn-danger")
            ),
            easyClose = FALSE
          ))
          return(NULL)
        }

        save_environment(input$env_name, values$scenarios, values$mcmc, values$methods)
        if (!is.null(on_env_saved)) {
          on_env_saved()
        }
        list(ok = TRUE, message = sprintf("Saved '%s'. Launch it from the Run tab.", input$env_name))
      }, error = function(e) list(ok = FALSE, message = conditionMessage(e)))
      if (is.null(status)) return(invisible(NULL))
      output$env_status <- shiny::renderUI(bexte_status(status$message, ok = status$ok))
      invisible(NULL)
    }

    shiny::observeEvent(input$save_env, save_environment_form())
    shiny::observeEvent(input$confirm_env_overwrite, {
      shiny::removeModal()
      save_environment_form(overwrite = TRUE)
    })

    invisible(NULL)
  })
}
