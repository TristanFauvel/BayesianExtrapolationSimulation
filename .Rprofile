source("renv/activate.R")

# cmdstanr is not on CRAN; DESCRIPTION's Additional_repositories field
# alone is not consistently honored by pak (used by CI's
# r-lib/actions/setup-r-dependencies), which fails to resolve it without
# this repo also being set explicitly via options(repos = ...).
options(repos = c(getOption("repos"), stan = "https://stan-dev.r-universe.dev/"))
