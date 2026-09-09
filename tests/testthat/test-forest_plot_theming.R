## The Analyze tab renders forest plots as static PNGs (they are multi-panel
## gtables, so they cannot go through rbext_plotly() like every other chart).
## That path drew in ink that only works on white: geom_pointrange() had no
## colour, so the point estimates came out black and vanished against the dark
## surface. forest_plot()/forest_plot_bayesian() therefore take an optional
## palette; NULL keeps the publication figures exactly as they were.

## The plot_*() functions resolve their style constants and methods_dict as
## free variables out of .GlobalEnv (see inst/scripts/plots.R), so a test that
## drives them has to stand those up first.
setup_forest_plot_globals <- function(figures_dir) {
  source(system.file("conf/plots_config.R", package = "RBExT"))
  source(system.file("conf/methods_plots_config.R", package = "RBExT"))
  source(system.file("conf/metrics_config.R", package = "RBExT"))
  methods_env <- new.env()
  source(system.file("conf/full/methods_config.R", package = "RBExT"), local = methods_env)
  assign("methods_dict", methods_env$methods_dict, envir = .GlobalEnv)
  assign("figures_dir", figures_dir, envir = .GlobalEnv)
  assign("remake_figures", TRUE, envir = .GlobalEnv)
  invisible(NULL)
}

forest_plot_fixture <- function() {
  readRDS(testthat::test_path("fixtures", "forest_plot_freq.rds"))
}

test_that("rbext_palette supplies reference-line colours that differ by scheme", {
  source(system.file("shiny_app/theme.R", package = "RBExT"))

  light <- rbext_palette("light")
  dark <- rbext_palette("dark")

  expect_true(all(c("ref_target", "ref_source") %in% names(light)))
  expect_true(all(c("ref_target", "ref_source") %in% names(dark)))
  expect_false(identical(light$ref_target, dark$ref_target))
  expect_false(identical(light$ref_source, dark$ref_source))
})

test_that("forest_subplot draws point ranges in the palette ink when given a palette", {
  source(system.file("shiny_app/theme.R", package = "RBExT"))
  data <- forest_plot_fixture()
  data <- data[data$drift == data$drift[1], , drop = FALSE]
  data$rows <- seq_len(nrow(data))
  withr::with_tempdir({
    setup_forest_plot_globals(file.path(getwd(), ""))
    pal <- rbext_palette("dark")

    plt <- forest_subplot(
      data, "No effect",
      ylabel = TRUE,
      x_metric_name = rlang::sym("success_proba"),
      x_metric_uncertainty_lower = "conf_int_success_proba_lower",
      x_metric_uncertainty_upper = "conf_int_success_proba_upper",
      x_metric_label = "Success probability",
      methods_labels = methods_labels,
      legend = FALSE,
      sort_by = FALSE,
      palette = pal
    )

    pointrange <- Filter(
      function(l) inherits(l$geom, "GeomPointrange"),
      plt$layers
    )
    expect_length(pointrange, 1)
    expect_identical(pointrange[[1]]$aes_params$colour, pal$ink)
  })
})

test_that("forest_subplot leaves point ranges at the ggplot2 default without a palette", {
  data <- forest_plot_fixture()
  data <- data[data$drift == data$drift[1], , drop = FALSE]
  data$rows <- seq_len(nrow(data))
  withr::with_tempdir({
    setup_forest_plot_globals(file.path(getwd(), ""))

    plt <- forest_subplot(
      data, "No effect",
      ylabel = TRUE,
      x_metric_name = rlang::sym("success_proba"),
      x_metric_uncertainty_lower = "conf_int_success_proba_lower",
      x_metric_uncertainty_upper = "conf_int_success_proba_upper",
      x_metric_label = "Success probability",
      methods_labels = methods_labels,
      legend = FALSE,
      sort_by = FALSE
    )

    pointrange <- Filter(
      function(l) inherits(l$geom, "GeomPointrange"),
      plt$layers
    )
    expect_null(pointrange[[1]]$aes_params$colour)
  })
})

test_that("forest_plot writes a separate dark PNG and leaves the light figures untouched", {
  source(system.file("shiny_app/theme.R", package = "RBExT"))
  data <- forest_plot_fixture()
  withr::with_tempdir({
    figures_dir <- file.path(getwd(), "figures", "")
    dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
    setup_forest_plot_globals(figures_dir)

    light_path <- suppressWarnings(forest_plot(data, "success_proba"))
    expect_true(file.exists(light_path))
    light_pdf <- sub("\\.png$", ".pdf", light_path)
    expect_true(file.exists(light_pdf))
    light_png_before <- readBin(light_path, "raw", file.size(light_path))
    light_pdf_mtime <- file.mtime(light_pdf)

    dark_path <- suppressWarnings(
      forest_plot(data, "success_proba", palette = rbext_palette("dark"))
    )

    ## A distinct file, so the publication figure is never overwritten.
    expect_match(basename(dark_path), "_dark\\.png$")
    expect_true(file.exists(dark_path))
    expect_false(identical(normalizePath(dark_path), normalizePath(light_path)))

    ## The light PNG is byte-identical and no second PDF was written.
    expect_identical(readBin(light_path, "raw", file.size(light_path)), light_png_before)
    expect_identical(file.mtime(light_pdf), light_pdf_mtime)
    expect_false(file.exists(sub("\\.png$", ".pdf", dark_path)))

    ## The dark render is actually different ink, not a copy.
    expect_false(identical(
      readBin(dark_path, "raw", file.size(dark_path)),
      light_png_before
    ))
  })
})

## The app decides per render which palette to hand the plot builders. Light
## mode passes NULL, so what the user sees on screen is exactly the figure that
## goes into the paper; only dark mode asks for a recoloured copy.
test_that("rbext_plot_palette returns NULL for light mode and the scheme for dark", {
  source(system.file("shiny_app/theme.R", package = "RBExT"))

  expect_null(rbext_plot_palette("light"))
  expect_identical(rbext_plot_palette("dark"), rbext_palette("dark"))
})

test_that("forest_image_for_mode picks the dark file only in dark mode", {
  source(system.file("shiny_app/theme.R", package = "RBExT"))
  source(system.file("shiny_app/modules/mod_analyze.R", package = "RBExT"))
  data <- forest_plot_fixture()
  withr::with_tempdir({
    figures_dir <- file.path(getwd(), "figures", "")
    dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
    setup_forest_plot_globals(figures_dir)

    light <- suppressWarnings(
      forest_image_for_mode("forest_plot", data, "success_proba", "light")
    )
    dark <- suppressWarnings(
      forest_image_for_mode("forest_plot", data, "success_proba", "dark")
    )

    expect_false(grepl("_dark\\.png$", light))
    expect_match(dark, "_dark\\.png$")
    expect_true(file.exists(light))
    expect_true(file.exists(dark))
  })
})
