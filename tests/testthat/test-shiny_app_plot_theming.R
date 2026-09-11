## The Shiny app renders the package's plot_*() functions with plotly. Those
## functions label their axes with language objects and their titles with
## latex2exp::TeX() expressions (see plot_metric_vs_drift(), which does
## `labs(x = rlang::sym(xvar$label), title = format_title(...))`), and plotly
## rejects both: verify_attr() calls unique() on every scalar attribute, which
## errors on a symbol and warns on an expression. bexte_plotly() is what makes
## those plots renderable in the app.

test_that("bexte_plotly renders a plot whose axis label is a language object", {
  skip_if_not_installed("plotly")
  skip_if_not_installed("latex2exp")
  source(system.file("shiny_app/theme.R", package = "BExTE"))

  d <- data.frame(x = 1:5, y = 1:5)
  plt <- ggplot2::ggplot(d, ggplot2::aes(x, y)) +
    ggplot2::geom_line() +
    ggplot2::labs(
      x = rlang::sym("Drift in treatment effect"),
      y = "Pr(Study success)",
      title = latex2exp::TeX("$\\sigma_T/\\sigma_S = 1$")
    )

  expect_no_error(plotly::plotly_build(bexte_plotly(plt, "light")))
})

test_that("bexte_plotly renders a plot whose legend labels are TeX expressions", {
  skip_if_not_installed("plotly")
  skip_if_not_installed("latex2exp")
  source(system.file("shiny_app/theme.R", package = "BExTE"))

  d <- data.frame(x = rep(1:5, 2), y = 1:10, g = rep(c("a", "b"), each = 5))
  plt <- ggplot2::ggplot(d, ggplot2::aes(x, y, colour = g)) +
    ggplot2::geom_line() +
    ggplot2::scale_colour_discrete(labels = latex2exp::TeX(c("$w_0 = 0$", "$w_0 = 1$"))) +
    ggplot2::labs(colour = latex2exp::TeX("$w_0$"))

  expect_no_error(plotly::plotly_build(bexte_plotly(plt, "dark")))
})

test_that("bexte_plotly keeps the axis label readable", {
  skip_if_not_installed("plotly")
  source(system.file("shiny_app/theme.R", package = "BExTE"))

  d <- data.frame(x = 1:5, y = 1:5)
  plt <- ggplot2::ggplot(d, ggplot2::aes(x, y)) +
    ggplot2::geom_line() +
    ggplot2::labs(x = rlang::sym("Drift in treatment effect"))

  built <- bexte_plotly(plt, "light")
  expect_identical(built$x$layout$xaxis$title$text, "Drift in treatment effect")
})

## plotly draws its own SVG and understands a small HTML subset (<sub>, <sup>,
## <b>, <i>) but no plotmath and no LaTeX, so bexte_latex_html() rewrites the
## `$...$` the plot_*() functions produce into that subset. Everything below
## is a label this package actually builds - see format_title() and
## make_labels_from_parameters() in R/plots_utils.R.

test_that("bexte_latex_html renders a subscript as a plotly <sub>", {
  source(system.file("shiny_app/theme.R", package = "BExTE"))

  expect_identical(bexte_latex_html("$N_T/2$"), "N<sub>T</sub>/2")
})

test_that("bexte_latex_html renders a braced superscript as a plotly <sup>", {
  source(system.file("shiny_app/theme.R", package = "BExTE"))

  expect_identical(bexte_latex_html("$\\sigma^{2}$"), "σ<sup>2</sup>")
})

test_that("bexte_latex_html renders a Greek command as its Unicode letter", {
  source(system.file("shiny_app/theme.R", package = "BExTE"))

  expect_identical(
    bexte_latex_html("$\\sigma_T/\\sigma_S = $1"),
    "σ<sub>T</sub>/σ<sub>S</sub> = 1"
  )
})

test_that("bexte_latex_html subscripts a Greek letter", {
  source(system.file("shiny_app/theme.R", package = "BExTE"))

  expect_identical(bexte_latex_html("$\\xi_\\gamma$"), "ξ<sub>γ</sub>")
})

test_that("bexte_latex_html renders \\hat as a combining circumflex", {
  source(system.file("shiny_app/theme.R", package = "BExTE"))

  expect_identical(
    bexte_latex_html("$\\theta_0 - \\hat{\\theta}_0$"),
    "θ<sub>0</sub> - θ̂<sub>0</sub>"
  )
})

test_that("bexte_latex_html renders a relation command", {
  source(system.file("shiny_app/theme.R", package = "BExTE"))

  expect_identical(bexte_latex_html("$\\tau \\sim HN(0.5)$"), "τ ∼ HN(0.5)")
})

test_that("bexte_latex_html closes the gap left by a math-mode delimiter", {
  source(system.file("shiny_app/theme.R", package = "BExTE"))

  expect_identical(bexte_latex_html("$N_T/2 = $ 58"), "N<sub>T</sub>/2 = 58")
})

test_that("bexte_latex_html escapes HTML so a label cannot inject markup", {
  source(system.file("shiny_app/theme.R", package = "BExTE"))

  expect_identical(
    bexte_latex_html("Drift < 0 & p > 0.5"),
    "Drift &lt; 0 &amp; p &gt; 0.5"
  )
})

test_that("bexte_latex_html leaves a label with no LaTeX alone", {
  source(system.file("shiny_app/theme.R", package = "BExTE"))

  expect_identical(bexte_latex_html("Pr(Study success)"), "Pr(Study success)")
})

test_that("bexte_plotly gives plotly the title as markup, not as literal LaTeX", {
  skip_if_not_installed("plotly")
  skip_if_not_installed("latex2exp")
  source(system.file("shiny_app/theme.R", package = "BExTE"))

  d <- data.frame(x = 1:5, y = 1:5)
  plt <- ggplot2::ggplot(d, ggplot2::aes(x, y)) +
    ggplot2::geom_line() +
    ggplot2::labs(title = latex2exp::TeX("Botox, $N_T/2 = $ 58, $\\sigma_T/\\sigma_S = $1"))

  built <- bexte_plotly(plt, "light")
  expect_identical(
    built$x$layout$title$text,
    "Botox, N<sub>T</sub>/2 = 58, σ<sub>T</sub>/σ<sub>S</sub> = 1"
  )
})

## The plot_*() functions build their legend entries with
## process_method_parameters_label(..., as_latex = FALSE), so those arrive as
## plain strings that still carry their `$...$` rather than as TeX()
## expressions. Only what is inside a math span is LaTeX; a bare underscore
## elsewhere is part of the name.

test_that("bexte_latex_html converts the math spans of a mixed label", {
  source(system.file("shiny_app/theme.R", package = "BExTE"))

  expect_identical(
    bexte_latex_html("Gaussian RMP, $\\xi_\\gamma$ = 0.5, $\\sigma_\\gamma$ = 0.1"),
    "Gaussian RMP, ξ<sub>γ</sub> = 0.5, σ<sub>γ</sub> = 0.1"
  )
})

test_that("bexte_latex_html leaves an underscore outside a math span alone", {
  source(system.file("shiny_app/theme.R", package = "BExTE"))

  expect_identical(bexte_latex_html("power_prior"), "power_prior")
})

test_that("bexte_plotly renders a legend entry written as a LaTeX string", {
  skip_if_not_installed("plotly")
  source(system.file("shiny_app/theme.R", package = "BExTE"))

  d <- data.frame(
    x = rep(1:5, 2), y = 1:10,
    g = rep(c("Gaussian RMP, $\\xi_\\gamma$ = 0.5", "Pooling"), each = 5)
  )
  plt <- ggplot2::ggplot(d, ggplot2::aes(x, y, colour = g)) + ggplot2::geom_line()

  built <- bexte_plotly(plt, "light")
  expect_identical(built$x$data[[1]]$name, "Gaussian RMP, ξ<sub>γ</sub> = 0.5")
})

test_that("bexte_latex_html keeps a missing label missing", {
  source(system.file("shiny_app/theme.R", package = "BExTE"))

  expect_identical(bexte_latex_html(NA_character_), NA_character_)
})

test_that("dark plot traces replace black with readable foreground ink", {
  source(system.file("shiny_app/theme.R", package = "BExTE"))

  trace <- list(
    line = list(color = "rgba(0,0,0,1)"),
    marker = list(color = c("#000000", "#cc0000"))
  )
  contrasted <- bexte_trace_contrast(trace, "dark", "#E5E9EF")
  expect_identical(contrasted$line$color, "#E5E9EF")
  expect_identical(contrasted$marker$color, c("#E5E9EF", "#cc0000"))
  expect_identical(bexte_trace_contrast(trace, "light", "#1E293B"), trace)
})
