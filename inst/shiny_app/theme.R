## Look and feel for the BExTE app: the Bootstrap 5 theme object, plus the
## small UI constructors the three modules share. The colours themselves live
## in www/bexte.scss (both schemes), so anything here that needs a colour at
## render time - the plotly charts, mainly - reads it from bexte_palette().

#' Bootstrap 5 theme for the app. `app_dir` is the directory holding this
#' file, because the Sass has to be resolved before shiny::runApp() moves the
#' working directory back to the repository root.
bexte_theme <- function(app_dir) {
  bslib::bs_add_rules(
    bslib::bs_theme(
      version = 5,
      bg = "#F7F8FA",
      fg = "#1E293B",
      primary = "#0F766E",
      "border-radius" = "6px",
      "enable-shadows" = FALSE
    ),
    sass::sass_file(file.path(app_dir, "www", "bexte.scss"))
  )
}

#' The two colour schemes, mirroring the custom properties in bexte.scss.
#' Used for output that draws its own pixels (plotly) and so cannot inherit
#' the page's CSS.
bexte_palette <- function(mode = c("light", "dark")) {
  mode <- match.arg(mode)
  ## ref_target/ref_source are the forest plots' sample-size reference lines,
  ## plain "black"/"red" in the publication figures. Black is invisible on the
  ## dark surface and red goes muddy, so the scheme carries lightened
  ## stand-ins that keep the two lines telling apart.
  if (identical(mode, "dark")) {
    list(ink = "#E5E9EF", ink_muted = "#94A3B8", hairline = "#2A3138",
         accent = "#2DD4BF", surface = "#171C22",
         ref_target = "#E5E9EF", ref_source = "#FF8A8A")
  } else {
    list(ink = "#1E293B", ink_muted = "#64748B", hairline = "#E3E7EC",
         accent = "#0F766E", surface = "#FFFFFF",
         ref_target = "black", ref_source = "red")
  }
}

#' The palette to hand the forest-plot builders for a colour mode. Light mode
#' returns NULL on purpose: the builders then draw the publication figure
#' untouched, so what the app shows is the same image that goes into the paper.
#' Only dark mode needs a recoloured copy.
bexte_plot_palette <- function(mode) {
  if (identical(mode, "dark")) bexte_palette("dark") else NULL
}

#' Wordmark for the navbar: two overlapping densities, which is what the
#' package is about - borrowing a source posterior into a target trial.
bexte_brand <- function() {
  shiny::tags$span(
    class = "bexte-brand",
    shiny::tags$svg(
      class = "bexte-mark", width = "22", height = "16", viewBox = "0 0 22 16",
      fill = "none", stroke = "currentColor", `stroke-width` = "1.5",
      `stroke-linecap` = "round", `aria-hidden` = "true",
      shiny::tags$path(d = "M1 14c3.2 0 3.4-11 6.6-11S11 14 14.2 14", opacity = "0.45"),
      shiny::tags$path(d = "M7.8 14c3.2 0 3.4-9 6.6-9S21 14 21 14")
    ),
    "BExTE"
  )
}

## ---- Page furniture --------------------------------------------------------

#' The centred column each tab's content sits in.
bexte_page <- function(...) {
  shiny::div(class = "bexte-page", ...)
}

#' One numbered step of the Configure worksheet. The index is part of the
#' content - these five sections are a sequence ending in "Save environment" -
#' rather than decoration, so nothing else in the app is numbered.
bexte_step <- function(index, title, note = NULL, ...) {
  bslib::card(
    class = "bexte-step",
    fill = FALSE,
    bslib::card_header(
      shiny::div(
        class = "bexte-step-header",
        shiny::span(class = "bexte-step-index", index),
        shiny::tags$h2(class = "bexte-step-title", title)
      )
    ),
    bslib::card_body(
      fillable = FALSE,
      if (!is.null(note)) shiny::p(class = "bexte-note", note),
      ...
    )
  )
}

#' A row of inputs that reflows to the available width instead of being pinned
#' to a fixed 12-column grid.
bexte_fields <- function(...) {
  shiny::div(class = "bexte-field-grid", ...)
}

#' Two panels of comparable weight, side by side.
bexte_split <- function(...) {
  shiny::div(class = "bexte-split", ...)
}

bexte_subhead <- function(text) {
  shiny::tags$h3(class = "bexte-subhead", text)
}

#' Text input carrying the filename-safe convention as a real browser
#' constraint as well as explanatory placeholder text.
bexte_name_input <- function(input_id, label) {
  htmltools::tagQuery(
    shiny::textInput(input_id, label, placeholder = "lowercase letters, numbers, - or _")
  )$find("input")$addAttrs(
    pattern = "[a-z0-9][a-z0-9_-]*",
    autocomplete = "off",
    autocapitalize = "none",
    spellcheck = "false"
  )$allTags()
}

bexte_action_button <- function(input_id, label, ..., disabled = FALSE) {
  button <- shiny::actionButton(input_id, label, ...)
  if (isTRUE(disabled)) {
    button <- htmltools::tagAppendAttributes(
      button, disabled = "disabled", `aria-disabled` = "true"
    )
  }
  button
}

bexte_download_button <- function(output_id, label, ..., disabled = FALSE) {
  button <- shiny::downloadButton(output_id, label, ...)
  if (isTRUE(disabled)) {
    button <- htmltools::tagAppendAttributes(
      button, class = "disabled", `aria-disabled` = "true", tabindex = "-1"
    )
  }
  button
}

#' Result of an action the user just took. `ok` picks the colour; the text is
#' the message itself, so callers say what happened rather than passing a
#' severity around.
bexte_status <- function(message, ok = TRUE) {
  if (is.null(message) || !nzchar(message)) {
    return(NULL)
  }
  shiny::div(
    class = paste("bexte-status", if (ok) "bexte-status-ok" else "bexte-status-error"),
    role = if (ok) "status" else "alert",
    `aria-live` = if (ok) "polite" else "assertive",
    message
  )
}

#' A single figure in the Run tab's readout.
bexte_metric <- function(value, label) {
  shiny::div(
    class = "bexte-metric",
    shiny::div(class = "bexte-metric-value", value),
    shiny::div(class = "bexte-metric-label", label)
  )
}

#' Run state as a pill: idle, live, finished or failed.
bexte_state <- function(label, kind = c("idle", "live", "done", "failed")) {
  kind <- match.arg(kind)
  shiny::span(class = paste0("bexte-state bexte-state-", kind), label)
}

#' Placeholder for a panel that has nothing to show yet. Says what to do next,
#' never just "no data".
bexte_empty <- function(message) {
  shiny::div(class = "bexte-empty", role = "status", message)
}

## ---- Themed output ---------------------------------------------------------

#' The LaTeX the plot_*() labels are written in, and the character each
#' command stands for. Whole alphabet rather than only the letters in use
#' today, so a label added later renders instead of leaking its backslash.
bexte_latex_symbols <- c(
  alpha = "\u03b1", beta = "\u03b2", gamma = "\u03b3", delta = "\u03b4",
  epsilon = "\u03b5", zeta = "\u03b6", eta = "\u03b7", theta = "\u03b8",
  iota = "\u03b9", kappa = "\u03ba", lambda = "\u03bb", mu = "\u03bc",
  nu = "\u03bd", xi = "\u03be", pi = "\u03c0", rho = "\u03c1",
  sigma = "\u03c3", tau = "\u03c4", upsilon = "\u03c5", phi = "\u03c6",
  chi = "\u03c7", psi = "\u03c8", omega = "\u03c9",
  Gamma = "\u0393", Delta = "\u0394", Theta = "\u0398", Lambda = "\u039b",
  Xi = "\u039e", Pi = "\u03a0", Sigma = "\u03a3", Phi = "\u03a6",
  Psi = "\u03a8", Omega = "\u03a9",
  sim = "\u223c", times = "\u00d7", cdot = "\u00b7", pm = "\u00b1",
  leq = "\u2264", geq = "\u2265", neq = "\u2260", infty = "\u221e",
  ldots = "\u2026"
)

#' HTML-escape a label, so that a "<" in one reaches plotly as a "<" and not
#' as the start of a tag it should honour.
bexte_html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

#' Rewrite the inside of one math span as the HTML subset plotly understands.
#' plotly draws its own SVG and reads no LaTeX and no plotmath, but it does
#' honour <sub>, <sup>, <b> and <i> in any text attribute, which covers what
#' these labels need: subscripted sample sizes and standard deviations, Greek
#' parameters, the odd hat. A command with no character of its own keeps its
#' name, so an unhandled label degrades to its own text and never to a stray
#' backslash or brace.
bexte_latex_math_html <- function(x) {
  for (command in names(bexte_latex_symbols)) {
    x <- gsub(
      paste0("\\\\", command, "(?![A-Za-z])"),
      bexte_latex_symbols[[command]], x,
      perl = TRUE
    )
  }
  ## \hat{x} becomes x plus a combining circumflex, which every font that has
  ## the letter also composes.
  x <- gsub("\\\\hat\\{(.)\\}", "\\1\u0302", x, perl = TRUE)
  x <- gsub("\\\\([A-Za-z]+)", "\\1", x, perl = TRUE)

  x <- gsub("_\\{([^{}]*)\\}", "<sub>\\1</sub>", x, perl = TRUE)
  x <- gsub("\\^\\{([^{}]*)\\}", "<sup>\\1</sup>", x, perl = TRUE)
  x <- gsub("_([^{}[:space:]])", "<sub>\\1</sub>", x, perl = TRUE)
  x <- gsub("\\^([^{}[:space:]])", "<sup>\\1</sup>", x, perl = TRUE)

  gsub("[{}]", "", x, perl = TRUE)
}

#' Rewrite a label for plotly. Only what a `$...$` encloses is LaTeX: these
#' labels are half prose, and a method named "power_prior" is not a subscript.
#' The delimiters themselves only ever leave a gap behind them ("$N_T/2 = $
#' 58" has two spaces), which is why the spacing is closed up at the end.
#' Vectorised, because a scale's labels arrive as one character vector.
bexte_latex_html <- function(x) {
  vapply(x, function(label) {
    if (is.na(label)) {
      return(NA_character_)
    }
    parts <- bexte_html_escape(strsplit(label, "$", fixed = TRUE)[[1]])
    if (length(parts) == 0) {
      return("")
    }
    inside_math <- seq_along(parts) %% 2 == 0
    parts[inside_math] <- bexte_latex_math_html(parts[inside_math])
    trimws(gsub(" {2,}", " ", paste(parts, collapse = "")))
  }, character(1), USE.NAMES = FALSE)
}

#' The LaTeX a ggplot2 label is holding. `latex2exp::TeX()` keeps its source
#' on a `latex` attribute, which is the only place the LaTeX still exists once
#' TeX() has turned it into plotmath; a symbol
#' (`plot_metric_vs_drift()` does `labs(x = rlang::sym(xvar$label))`) is its
#' own name; anything else reads as the expression it is.
bexte_label_latex <- function(x) {
  if (is.expression(x)) {
    latex <- attr(x, "latex")
    if (is.character(latex) && length(latex) == length(x)) {
      return(latex)
    }
    return(vapply(as.list(x), function(e) paste(deparse(e), collapse = ""), character(1)))
  }
  if (is.symbol(x)) {
    return(as.character(x))
  }
  paste(deparse(x), collapse = "")
}

bexte_label_html <- function(x) {
  bexte_latex_html(bexte_label_latex(x))
}

#' Walk a layout and rewrite every label in it: the language objects and
#' expressions plotly cannot carry at all, and the `text` a title, an axis or
#' an annotation holds. Keyed on the name `text` rather than applied to every
#' string, so that the colours and font families alongside them are left as
#' they are. Deliberately limited to the layout and to trace names: the rest
#' of a plotly object holds quosures that it needs to keep.
bexte_plain_layout <- function(x, key = "") {
  if (is.expression(x) || is.language(x)) {
    return(bexte_label_html(x))
  }
  if (is.list(x)) {
    keys <- names(x)
    if (is.null(keys)) {
      keys <- rep("", length(x))
    }
    walked <- Map(bexte_plain_layout, x, keys)
    names(walked) <- names(x)
    return(walked)
  }
  if (identical(key, "text") && is.character(x)) {
    return(bexte_latex_html(x))
  }
  x
}

#' An axis title is either plain text or a list(text=, font=); normalising to
#' the list form lets bexte_plotly() merge a colour in without dropping the
#' text it is merging into.
bexte_title_list <- function(x) {
  if (is.null(x) || is.list(x)) {
    return(x)
  }
  list(text = x)
}

bexte_trace_contrast <- function(trace, mode, ink) {
  if (!identical(mode, "dark")) return(trace)
  replace_black <- function(value) {
    if (!is.character(value)) return(value)
    black <- grepl(
      "^(#000000|#000|black|rgb\\(0, ?0, ?0\\)|rgba\\(0, ?0, ?0, ?1\\))$",
      value, ignore.case = TRUE
    )
    value[black] <- ink
    value
  }
  for (part in c("line", "marker", "error_x", "error_y", "textfont")) {
    if (!is.null(trace[[part]]$color)) {
      trace[[part]]$color <- replace_black(trace[[part]]$color)
    }
  }
  trace
}

#' Hand a ggplot to plotly with the page's colours applied. plotly draws its
#' own SVG and cannot inherit the stylesheet, so the backgrounds are made
#' transparent (the card behind shows through) and the text, gridlines and
#' axes are set from the active scheme. Axis and legend titles need saying
#' twice: ggplotly copies those colours out of the ggplot theme, where they
#' are near-black whatever the page is doing.
bexte_plotly <- function(plot, mode = "light") {
  pal <- bexte_palette(if (identical(mode, "dark")) "dark" else "light")

  ## Unwrap the labels before the handover: ggplotly turns an expression label
  ## into the literal text "expression(w[0])" and passes a symbol through
  ## untouched, so neither survives it intact. They go over as their LaTeX
  ## rather than as finished markup, which leaves one place below where a
  ## label becomes HTML - whether it started as an expression or, as the
  ## legend entries do, as a plain string. Re-applied through labs() rather
  ## than assigned, because ggplot2 validates $labels.
  plotmath_labels <- Filter(
    function(label) is.expression(label) || is.language(label),
    as.list(plot$labels)
  )
  if (length(plotmath_labels) > 0) {
    plot <- plot + do.call(ggplot2::labs, lapply(plotmath_labels, bexte_label_latex))
  }

  fig <- plotly::ggplotly(plot)
  fig$x$layout <- bexte_plain_layout(fig$x$layout)
  for (axis_name in c("xaxis", "yaxis")) {
    if (!is.null(fig$x$layout[[axis_name]])) {
      fig$x$layout[[axis_name]]$title <- bexte_title_list(fig$x$layout[[axis_name]]$title)
    }
  }
  fig$x$data <- lapply(fig$x$data, function(trace) {
    if (is.expression(trace$name) || is.language(trace$name)) {
      trace$name <- bexte_label_html(trace$name)
    } else if (is.character(trace$name)) {
      trace$name <- bexte_latex_html(trace$name)
    }
    bexte_trace_contrast(trace, mode, pal$ink)
  })

  axis <- list(
    gridcolor = pal$hairline,
    zerolinecolor = pal$hairline,
    linecolor = pal$hairline,
    tickcolor = pal$hairline,
    tickfont = list(color = pal$ink_muted),
    title = list(font = list(color = pal$ink))
  )
  plotly::layout(
    fig,
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor = "rgba(0,0,0,0)",
    font = list(color = pal$ink),
    title = list(font = list(color = pal$ink)),
    xaxis = axis,
    yaxis = axis,
    legend = list(
      font = list(color = pal$ink),
      title = list(font = list(color = pal$ink)),
      bgcolor = "rgba(0,0,0,0)"
    ),
    hoverlabel = list(
      bgcolor = pal$surface,
      bordercolor = pal$hairline,
      font = list(color = pal$ink)
    ),
    modebar = list(
      bgcolor = "rgba(0,0,0,0)",
      color = pal$ink_muted,
      activecolor = pal$accent
    )
  )
}
