## Configure tab: build a case study and/or a full simulation environment
## (scenarios_config.yml / mcmc_config.yml / methods_config.R), saved under
## user_configs/ - see helpers.R for the save/read functions.

mod_configure_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h4("1. Case study"),
    shiny::p("Pick from the existing case studies, or define a new one (binary or continuous endpoints only)."),
    shiny::wellPanel(
      shiny::radioButtons(ns("case_study_mode"), NULL, choices = c("Use existing" = "existing", "Create new" = "new"), inline = TRUE),
      shiny::conditionalPanel(
        "input.case_study_mode == 'new'", ns = ns,
        shiny::fluidRow(
          shiny::column(4, shiny::textInput(ns("cs_name"), "Name (lowercase, no spaces)")),
          shiny::column(4, shiny::textInput(ns("cs_control_arm"), "Control arm name", value = "Placebo")),
          shiny::column(4, shiny::selectInput(ns("cs_endpoint"), "Endpoint", choices = c("binary", "continuous")))
        ),
        shiny::fluidRow(
          shiny::column(4, shiny::selectInput(ns("cs_null_space"), "Null space", choices = c("left", "right"))),
          shiny::column(4, shiny::numericInput(ns("cs_theta_0"), "theta_0 (null hypothesis boundary)", value = 0))
        ),
        shiny::h5("Target study"),
        shiny::fluidRow(
          shiny::column(3, shiny::numericInput(ns("cs_target_control_n"), "Control n", value = 50, min = 1)),
          shiny::column(3, shiny::numericInput(ns("cs_target_treatment_n"), "Treatment n", value = 50, min = 1)),
          shiny::conditionalPanel(
            "input.cs_endpoint == 'binary'", ns = ns,
            shiny::column(3, shiny::numericInput(ns("cs_target_control_resp"), "Control responses", value = 20, min = 0)),
            shiny::column(3, shiny::numericInput(ns("cs_target_treatment_resp"), "Treatment responses", value = 30, min = 0))
          ),
          shiny::conditionalPanel(
            "input.cs_endpoint == 'continuous'", ns = ns,
            shiny::column(3, shiny::numericInput(ns("cs_target_effect"), "Treatment effect", value = 0.1)),
            shiny::column(3, shiny::numericInput(ns("cs_target_se"), "Standard error", value = 0.1, min = 0))
          )
        ),
        shiny::h5("Source study"),
        shiny::fluidRow(
          shiny::column(3, shiny::numericInput(ns("cs_source_control_n"), "Control n", value = 200, min = 1)),
          shiny::column(3, shiny::numericInput(ns("cs_source_treatment_n"), "Treatment n", value = 200, min = 1)),
          shiny::conditionalPanel(
            "input.cs_endpoint == 'binary'", ns = ns,
            shiny::column(3, shiny::numericInput(ns("cs_source_control_resp"), "Control responses", value = 80, min = 0)),
            shiny::column(3, shiny::numericInput(ns("cs_source_treatment_resp"), "Treatment responses", value = 110, min = 0))
          ),
          shiny::conditionalPanel(
            "input.cs_endpoint == 'continuous'", ns = ns,
            shiny::column(3, shiny::numericInput(ns("cs_source_effect"), "Treatment effect", value = 0.1)),
            shiny::column(3, shiny::numericInput(ns("cs_source_se"), "Standard error", value = 0.07, min = 0))
          )
        ),
        shiny::actionButton(ns("save_case_study"), "Save case study", class = "btn-primary"),
        shiny::textOutput(ns("case_study_status"))
      )
    ),

    shiny::h4("2. Case studies & methods for this environment"),
    shiny::fluidRow(
      shiny::column(
        6,
        shiny::uiOutput(ns("case_studies_picker"))
      ),
      shiny::column(
        6,
        shiny::checkboxGroupInput(ns("methods"), "Methods to compare", choices = NULL)
      )
    ),
    shiny::uiOutput(ns("method_params_ui")),

    shiny::h4("3. Scenario settings"),
    shiny::fluidRow(
      shiny::column(3, shiny::numericInput(ns("n_replicates"), "Monte Carlo replicates", value = 1000, min = 1)),
      shiny::column(3, shiny::numericInput(ns("ndrift"), "Number of drift points", value = 15, min = 1)),
      shiny::column(3, shiny::textInput(ns("sample_size_factors"), "Sample size factors", value = "1, 2, 4")),
      shiny::column(3, shiny::checkboxInput(ns("parallelization"), "Run in parallel", value = FALSE))
    ),
    shiny::fluidRow(
      shiny::column(3, shiny::textInput(ns("denominator_change_factor"), "Denominator change factor", value = "1")),
      shiny::column(3, shiny::textInput(ns("target_to_source_std_ratio_range"), "Target/source SD ratio range", value = "1"))
    ),

    shiny::h4("4. MCMC settings"),
    shiny::p("Only used by methods that need MCMC sampling (RMP, NPP, commensurate power prior, ...)."),
    shiny::fluidRow(
      shiny::column(2, shiny::numericInput(ns("num_chains"), "Chains", value = 4, min = 1)),
      shiny::column(2, shiny::numericInput(ns("chain_length"), "Chain length", value = 2000, min = 1)),
      shiny::column(2, shiny::numericInput(ns("tune"), "Tune", value = 1000, min = 0)),
      shiny::column(2, shiny::numericInput(ns("target_ess"), "Target ESS", value = 2000, min = 1)),
      shiny::column(2, shiny::numericInput(ns("rhat_threshold"), "Rhat threshold", value = 1.1, min = 1))
    ),

    shiny::h4("5. Save"),
    shiny::fluidRow(
      shiny::column(4, shiny::textInput(ns("env_name"), "Environment name (lowercase, no spaces)")),
      shiny::column(4, shiny::actionButton(ns("save_env"), "Save environment", class = "btn-primary", style = "margin-top: 25px;"))
    ),
    shiny::textOutput(ns("env_status"))
  )
}

mod_configure_server <- function(id) {
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
      shiny::checkboxGroupInput(ns("case_studies"), "Case studies to include", choices = choices)
    })

    shiny::observe({
      shiny::updateCheckboxGroupInput(session, "methods", choices = names(methods_template))
    })

    shiny::observeEvent(input$save_case_study, {
      status <- tryCatch({
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
        paste("Saved case study:", input$cs_name)
      }, error = function(e) paste("Error:", conditionMessage(e)))
      output$case_study_status <- shiny::renderText(status)
    })

    ## ---- Per-method parameter range editors --------------------------------

    format_range <- function(range) {
      paste(vapply(range, function(x) paste(x, collapse = ","), character(1)), collapse = "; ")
    }
    parse_range <- function(text, original_first_value) {
      parts <- trimws(strsplit(text, "[;,]")[[1]])
      parts <- parts[parts != ""]
      as_one <- function(p) {
        if (is.logical(original_first_value)) {
          return(as.logical(p))
        }
        num <- suppressWarnings(as.numeric(p))
        if (!is.na(num)) num else p
      }
      lapply(parts, as_one)
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
            shiny::textInput(
              input_id,
              label = sprintf("%s (%s)", p$parameter_name %||% pname, pname),
              value = format_range(p$range)
            )
          }
        })
        shiny::wellPanel(shiny::h5(method), rows)
      })
      shiny::tagList(shiny::h5("Parameter ranges to test"), panels)
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
          if (!is.null(text) && nzchar(text)) {
            new_params[[pname]]$range <- parse_range(text, params[[pname]]$range[[1]])
          }
        }
        out[[method]] <- new_params
      }
      out
    }

    ## ---- Save environment ---------------------------------------------------

    shiny::observeEvent(input$save_env, {
      status <- tryCatch({
        if (is.null(input$env_name) || !nzchar(input$env_name)) {
          stop("Give the environment a name.")
        }
        if (is.null(input$case_studies) || length(input$case_studies) == 0) {
          stop("Select at least one case study.")
        }
        if (is.null(input$methods) || length(input$methods) == 0) {
          stop("Select at least one method.")
        }

        parse_num_list <- function(text) as.numeric(trimws(strsplit(text, ",")[[1]]))

        scenarios_config <- list(
          n_replicates = input$n_replicates,
          ndrift = input$ndrift,
          denominator_change_factor = as.list(parse_num_list(input$denominator_change_factor)),
          sample_size_factors = as.list(parse_num_list(input$sample_size_factors)),
          target_to_source_std_ratio_range = as.list(parse_num_list(input$target_to_source_std_ratio_range)),
          parallelization = isTRUE(input$parallelization),
          case_studies = as.list(input$case_studies),
          methods = as.list(input$methods)
        )

        mcmc_config <- list(
          num_chains = input$num_chains,
          parallel_chains = input$num_chains,
          tune = input$tune,
          target_accept = 0.8,
          chain_length = input$chain_length,
          max_chain_length = input$chain_length * 2,
          target_ess = input$target_ess,
          rhat_threshold = input$rhat_threshold,
          max_divergence_rate = 0.01
        )

        save_environment(input$env_name, scenarios_config, mcmc_config, build_methods_dict_selected())
        paste("Saved environment:", input$env_name, "- go to the Run tab to launch it.")
      }, error = function(e) paste("Error:", conditionMessage(e)))
      output$env_status <- shiny::renderText(status)
    })

    invisible(NULL)
  })
}
