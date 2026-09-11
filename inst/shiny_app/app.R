## RBExT Shiny app: configure a simulation study, run it locally, then
## analyze and visualize the results.
##
## Launch with RBExT::run_rbext_app() - see R/shiny_app.R. The app works out
## of a workspace directory, exactly like inst/scripts/main.R works out of the
## repository root (it reads/writes "./results/<env>/", "./logs/<env>/",
## "./user_configs/<env>/" relative to it). run_rbext_app() picks the
## workspace - the checkout when there is one, a per-user directory otherwise.

## shiny::runApp() changes the process's working directory to this app's own
## directory (inst/shiny_app/) for as long as it runs. Every RBExT function
## (and this app's helpers) expects the working directory to be the
## workspace instead (relative paths like "./results/<env>/"), so
## run_rbext_app() records it before calling runApp() and we switch back to it
## here, right after sourcing our own files (which still need to be found
## relative to this app directory).
app_dir <- getwd()
source(file.path(app_dir, "helpers.R"))
source(file.path(app_dir, "theme.R"))
source(file.path(app_dir, "modules", "mod_configure.R"))
source(file.path(app_dir, "modules", "mod_run.R"))
source(file.path(app_dir, "modules", "mod_analyze.R"))
source(file.path(app_dir, "modules", "mod_replicate.R"))

## rbext_theme() compiles www/rbext.scss, so it has to run while the working
## directory still points at the app.
app_theme <- rbext_theme(app_dir)

workspace <- getOption("rbext.workspace", app_dir)
setwd(workspace)

ui <- shiny::tagList(
  ## Served straight out of www/; without it the browser's automatic probe
  ## for a favicon 404s on every page load.
  shiny::tags$head(
    shiny::tags$link(rel = "icon", type = "image/svg+xml", href = "favicon.svg")
  ),
  bslib::page_navbar(
    title = rbext_brand(),
    window_title = "RBExT",
    theme = app_theme,
    id = "main_nav",
    fillable = "Analyze",
    bslib::nav_panel("Configure", mod_configure_ui("configure")),
    bslib::nav_panel("Run", mod_run_ui("run")),
    bslib::nav_panel("Analyze", mod_analyze_ui("analyze")),
    bslib::nav_panel("Replicate paper", mod_replicate_ui("replicate")),
    bslib::nav_spacer(),
    bslib::nav_item(bslib::input_dark_mode(id = "color_mode"))
  )
)

server <- function(input, output, session) {
  setwd(workspace)
  just_finished_env <- shiny::reactiveVal(NULL)
  ## Bumped whenever an environment is saved, so the Run tab's environment
  ## picker can refresh itself instead of relying on a manual button.
  env_saved <- shiny::reactiveVal(0L)
  color_mode <- shiny::reactive(input$color_mode %||% "light")

  mod_configure_server("configure", on_env_saved = function() env_saved(env_saved() + 1L))
  mod_run_server("run", on_run_complete = function(env) just_finished_env(env), env_saved = env_saved)
  mod_analyze_server("analyze", just_finished_env = just_finished_env, color_mode = color_mode)
  mod_replicate_server("replicate", color_mode = color_mode)
}

shiny::shinyApp(ui, server)
