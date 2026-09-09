## RBExT Shiny app: configure a simulation study, run it locally, then
## analyze and visualize the results.
##
## Launch with RBExT::run_rbext_app() - see R/shiny_app.R. The app assumes
## the working directory is the repository root, exactly like
## inst/scripts/main.R (it reads/writes "./results/<env>/", "./logs/<env>/",
## "./user_configs/<env>/" relative to it).

## shiny::runApp() changes the process's working directory to this app's own
## directory (inst/shiny_app/) for as long as it runs. Every RBExT function
## (and this app's helpers) expects the working directory to be the
## repository root instead (relative paths like "./results/<env>/"), so
## run_rbext_app() records that root before calling runApp() and we switch
## back to it here, right after sourcing our own files (which still need to
## be found relative to this app directory).
app_dir <- getwd()
source(file.path(app_dir, "helpers.R"))
source(file.path(app_dir, "theme.R"))
source(file.path(app_dir, "modules", "mod_configure.R"))
source(file.path(app_dir, "modules", "mod_run.R"))
source(file.path(app_dir, "modules", "mod_analyze.R"))

## rbext_theme() compiles www/rbext.scss, so it has to run while the working
## directory still points at the app.
app_theme <- rbext_theme(app_dir)

repo_root <- getOption("rbext.repo_root", app_dir)
setwd(repo_root)

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
    bslib::nav_spacer(),
    bslib::nav_item(bslib::input_dark_mode(id = "color_mode"))
  )
)

server <- function(input, output, session) {
  setwd(repo_root)
  just_finished_env <- shiny::reactiveVal(NULL)
  ## Bumped whenever an environment is saved, so the Run tab's environment
  ## picker can refresh itself instead of relying on a manual button.
  env_saved <- shiny::reactiveVal(0L)
  color_mode <- shiny::reactive(input$color_mode %||% "light")

  mod_configure_server("configure", on_env_saved = function() env_saved(env_saved() + 1L))
  mod_run_server("run", on_run_complete = function(env) just_finished_env(env), env_saved = env_saved)
  mod_analyze_server("analyze", just_finished_env = just_finished_env, color_mode = color_mode)
}

shiny::shinyApp(ui, server)
