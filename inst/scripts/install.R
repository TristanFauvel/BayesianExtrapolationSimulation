## One-shot setup for BExTE.
##
##   Rscript install.R
##
## Run it either from a directory holding a release tarball
## (BExTE_<version>.tar.gz) or from a source checkout. It installs BExTE's R
## dependencies, BExTE itself, and CmdStan.
##
## CmdStan is the part that hand-rolled installs usually miss: cmdstanr is only
## the R interface, and the Stan models are compiled to native binaries at run
## time, so a C++ toolchain and a CmdStan installation both have to be present
## before any MCMC method will run.

if (getRversion() < "4.0.0") {
  stop("BExTE needs R 4.0.0 or later; this is ", getRversion(), ".")
}

## cmdstanr is not on CRAN. DESCRIPTION's Additional_repositories field is not
## consistently honoured by the resolvers, so set the repository explicitly.
options(repos = c(
  CRAN = "https://cloud.r-project.org",
  stan = "https://stan-dev.r-universe.dev"
))

if (!requireNamespace("pak", quietly = TRUE)) {
  install.packages("pak")
}

tarballs <- sort(Sys.glob("BExTE_*.tar.gz"), decreasing = TRUE)

if (length(tarballs) > 0) {
  message("Installing ", tarballs[[1]], " and its dependencies ...")
  pak::pkg_install(paste0("local::", tarballs[[1]]), ask = FALSE)
} else if (file.exists("DESCRIPTION")) {
  message("Installing dependencies for the checkout in ", getwd(), " ...")
  pak::local_install_deps(".", ask = FALSE)
} else {
  stop(
    "Found neither BExTE_<version>.tar.gz nor a DESCRIPTION in ", getwd(), ".\n",
    "Run this from the directory you downloaded the release into, or from a ",
    "source checkout."
  )
}

## cmdstanr ships no CmdStan of its own, and install_cmdstan() compiles it, so
## check the toolchain before spending the build on a failure.
cmdstan_installed <- !is.null(tryCatch(
  cmdstanr::cmdstan_path(),
  error = function(e) NULL
))

if (cmdstan_installed) {
  message("CmdStan already present at ", cmdstanr::cmdstan_path(), ".")
} else {
  message("Installing CmdStan (this compiles, and takes a few minutes) ...")
  cmdstanr::check_cmdstan_toolchain(fix = TRUE)
  cmdstanr::install_cmdstan()
}

## An application-menu entry, so the app can be started without an R session.
## Desktop entries are a freedesktop.org convention, so this is Linux only.
if (identical(tolower(Sys.info()[["sysname"]]), "linux")) {
  BExTE::create_bexte_shortcut()
  message("Added an \"BExTE\" entry to the application menu.")
} else {
  message(
    "No application-menu entry was added: BExTE::create_bexte_shortcut() ",
    "writes a freedesktop.org desktop entry, which only applies on Linux."
  )
}

message(
  "\nDone. Launch BExTE from the application menu, or from R with:\n",
  "  library(BExTE)\n",
  "  run_bexte_app()\n"
)
