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
source(file.path(app_dir, "modules", "mod_configure.R"))
source(file.path(app_dir, "modules", "mod_run.R"))
source(file.path(app_dir, "modules", "mod_analyze.R"))

repo_root <- getOption("rbext.repo_root", app_dir)
setwd(repo_root)

ui <- shiny::navbarPage(
  title = "RBExT",
  shiny::tabPanel("Configure", mod_configure_ui("configure")),
  shiny::tabPanel("Run", mod_run_ui("run")),
  shiny::tabPanel("Analyze", mod_analyze_ui("analyze"))
)

server <- function(input, output, session) {
  setwd(repo_root)
  just_finished_env <- shiny::reactiveVal(NULL)

  mod_configure_server("configure")
  mod_run_server("run", on_run_complete = function(env) just_finished_env(env))
  mod_analyze_server("analyze", just_finished_env = just_finished_env)
}

shiny::shinyApp(ui, server)
