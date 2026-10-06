# Dashboard for the figure: change its settings and download the result.
# Run locally from the repository root with shiny::runApp("app").
# The plots come from R/figure.R, the same code that figure_example.R runs.

library(shiny)
library(bslib)

`%||%` <- function(x, y) if (is.null(x)) y else x # base R has it only from 4.4

# R/figure.R, data/ and images/ sit next to this file in the Shinylive bundle,
# and one level up in the repository
root <- if (file.exists("R/figure.R")) "." else ".."
source(file.path(root, "R", "figure.R"), local = TRUE)

# ggplotGrob() in the panel builders draws on the current device. Use a null pdf device:
# no Rplots.pdf, and no cairo device (cairo crashes in webR once magick is loaded).
pdf(NULL)

fig_data <- load_figure_data(file.path(root, "data"), file.path(root, "images"))
defaults <- figure_defaults()

# Settings each panel uses: a panel is rebuilt only when one of these changes
panel_settings <- list(
  A = character(),
  B = c("base_size", "group_colours", "sigma", "show_inset", "show_polygons"),
  C = c("base_size", "group_colours", "jitter_seed", "point_alpha"),
  D = c("base_size", "shear_constant", "width_line", "velocity_line"),
  E = c("base_size", "decay", "ribbon_width"),
  F = c("base_size", "viridis_option")
)
panel_builders <- list(A = panel_a, B = panel_b, C = panel_c, D = panel_d, E = panel_e, F = panel_f)
# Data sets each panel uses (panel A shows the repository's images, they can't be replaced)
panel_datasets <- list(A = character(), B = "B", C = "C", D = c("D1", "D2"), E = "E", F = "F")

# Data sets that can be replaced by an uploaded CSV, one tidy table each
dataset_choices <- c("B: neighbor numbers" = "B", "C: tracheal length" = "C",
                     "D: shear stress (red squares)" = "D1", "D: flow velocity (blue circles)" = "D2",
                     "E: concentration" = "E", "F: Hill coefficients" = "F")
dataset_notes <- list(
  B = "One row per number of neighbors n: fraction and error of the apical (1) and basal (2) side.",
  C = "One row per measurement: group (type, at most two groups), stage (dev_stage) and length in µm.",
  D1 = "Lumen width (µm) and shear stress (Pa), both positive.",
  D2 = "Lumen width (µm) and relative flow velocity.",
  E = paste("One row per time point and gene (at most three genes): concentration C with its error bars.",
            "pos_err_t and neg_err_t (horizontal error bars) are optional."),
  F = "One row per point: dissociation constant K (positive), Hill coefficient n, amplitude (point size) and duration (colour)."
)
dataset_label <- function(dataset) names(dataset_choices)[dataset_choices == dataset]

# "trachea_length 20 to 120; dev_stage E8.5, E9.5, E10.5, E11.5"
format_limits <- function(dataset) {
  limits <- axis_limits()[[dataset]]
  paste(vapply(names(limits), function(col) {
    range <- limits[[col]]
    paste(col, if (is.character(range)) paste(range, collapse = ", ") else paste(range, collapse = " to "))
  }, character(1)), collapse = "; ")
}
# Built panels, shared by all sessions. An explicit cache: Shinylive sets up no default app cache.
panel_cache <- cachem::cache_mem(max_size = 100 * 1024^2)

viridis_options <- c("magma (A)" = "A", "inferno (B)" = "B", "plasma (C)" = "C", "viridis (D)" = "D",
                     "cividis (E)" = "E", "rocket (F)" = "F", "mako (G)" = "G", "turbo (H)" = "H")

save_png <- function(plot, file, width, height, dpi) {
  ggsave(file, plot, width = width, height = height, dpi = dpi, bg = "white", device = ragg::agg_png)
}

# Draws one panel in its place in the 3 x 2 figure, with empty cells around it, and trims the
# white space. Drawn alone, the panel would not match the figure: patchwork aligns the panels,
# and the insets in B and D are placed for the layout of the whole figure.
save_panel_png <- function(panel, id, file, dpi) {
  spacer <- plot_spacer() + theme(aspect.ratio = 1) # all panels have aspect ratio 1
  cells <- rep(list(spacer), 6)
  cells[[match(id, LETTERS[1:6])]] <- panel + labs(tag = id)
  save_png(wrap_plots(cells, ncol = 3, nrow = 2), file, fig_width, fig_height, dpi)

  image <- magick::image_trim(magick::image_read(file))
  image <- magick::image_border(image, "white", geometry = paste0(dpi %/% 8, "x", dpi %/% 8))
  magick::image_write(image, file, format = "png")
}

# Colour input ----

# A native <input type="color"> with a small Shiny input binding (no extra package)
colour_input <- function(id, label, value) {
  div(class = "form-group shiny-input-container",
      tags$label(`for` = id, class = "control-label", label),
      tags$input(id = id, type = "color", value = value, class = "scifig-colour form-control form-control-color"))
}

colour_binding_js <- "
const colourBinding = new Shiny.InputBinding();
$.extend(colourBinding, {
  find: (scope) => $(scope).find('input.scifig-colour'),
  getValue: (el) => el.value,
  setValue: (el, value) => { el.value = value; },
  receiveMessage: (el, data) => { el.value = data.value; $(el).trigger('change'); },
  subscribe: (el, callback) => $(el).on('input.scifig change.scifig', () => callback(true)),
  unsubscribe: (el) => $(el).off('.scifig'),
  getRatePolicy: () => ({ policy: 'debounce', delay: 250 })
});
Shiny.inputBindings.register(colourBinding, 'scifig.colour');
"

# UI ----

settings_panel <- accordion(
  open = "Figure",
  accordion_panel(
    "Figure",
    sliderInput("base_size", "Base font size", min = 8, max = 16, step = 0.5, value = defaults$base_size),
    helpText("The insets in panels B and D are placed for the default size; other sizes can shift labels."),
    checkboxGroupInput("panels", "Panels", choices = LETTERS[1:6], selected = defaults$panels, inline = TRUE),
    # panel C colours its groups in alphabetical order (example data: mutant, wildtype)
    colour_input("colour1", "Colour 1 (B: apical side, C: first group A\u2013Z)", defaults$group_colours[1]),
    colour_input("colour2", "Colour 2 (B: basal side, C: second group)", defaults$group_colours[2])
  ),
  accordion_panel(
    "B: neighbor numbers",
    sliderInput("sigma", "Lognormal sigma", min = 0.05, max = 0.5, step = 0.01, value = defaults$sigma),
    input_switch("show_inset", "Interior angle inset", value = defaults$show_inset),
    input_switch("show_polygons", "Polygons below the axis", value = defaults$show_polygons)
  ),
  accordion_panel(
    "C: tracheal length",
    numericInput("jitter_seed", "Jitter seed", value = defaults$jitter_seed, min = 1, step = 1),
    sliderInput("point_alpha", "Point opacity", min = 0.05, max = 1, step = 0.05, value = defaults$point_alpha)
  ),
  accordion_panel(
    "D: shear stress",
    numericInput("shear_constant", "Shear stress constant", value = defaults$shear_constant, min = 0, step = 0.5),
    helpText("shear stress = constant / (π · 18 · width²)"),
    numericInput("width_line", "Dashed line: lumen width b (µm)", value = defaults$width_line,
                 min = 0.5, max = 50, step = 0.1),
    numericInput("velocity_line", "Dashed line: relative velocity", value = defaults$velocity_line,
                 min = 0, max = 1, step = 0.05)
  ),
  accordion_panel(
    "E: concentration",
    numericInput("decay1", "Decay constant, dashed curve (h)", value = defaults$decay[1], min = 1, step = 1),
    numericInput("decay2", "Decay constant, solid curve (h)", value = defaults$decay[2], min = 1, step = 1),
    numericInput("decay3", "Decay constant, dotted curve (h)", value = defaults$decay[3], min = 1, step = 1),
    sliderInput("ribbon_width", "Band around the solid curve (± %)", min = 0, max = 50, step = 1,
                value = defaults$ribbon_width * 100)
  ),
  accordion_panel(
    "F: Hill coefficients",
    selectInput("viridis_option", "Colour palette", choices = viridis_options, selected = defaults$viridis_option)
  )
)

ui <- page_sidebar(
  title = "Scientific figure dashboard",
  tags$head(tags$script(HTML(colour_binding_js))),
  sidebar = sidebar(
    width = 320,
    settings_panel,
    actionButton("reset", "Reset to defaults", icon = icon("rotate-left"))
  ),
  navset_card_underline(
    nav_panel(
      "Figure",
      imageOutput("figure_preview", height = "auto"),
      uiOutput("range_notes"),
      card_footer(
        div(class = "d-flex flex-wrap gap-2 align-items-center",
            selectInput("png_dpi", NULL, width = "180px",
                        choices = c("PNG, 96 dpi" = 96, "PNG, 150 dpi" = 150, "PNG, 300 dpi" = 300)),
            downloadButton("download_png", "Download PNG", class = "mb-3"),
            downloadButton("download_svg", "Download SVG", class = "mb-3"))
      )
    ),
    nav_panel(
      "Panel",
      radioButtons("single_panel", NULL, choices = LETTERS[1:6], selected = "B", inline = TRUE),
      imageOutput("panel_preview", height = "auto")
    ),
    nav_panel(
      "Data",
      layout_columns(
        col_widths = c(4, 8),
        div(
          selectInput("dataset", "Data set", choices = dataset_choices),
          uiOutput("dataset_help"),
          fileInput("upload", "Replace with a CSV file", accept = c(".csv", "text/csv")),
          uiOutput("upload_status"),
          div(class = "d-flex flex-wrap gap-2",
              downloadButton("download_example", "Example CSV"),
              actionButton("reset_data", "Use example data", icon = icon("rotate-left")))
        ),
        DT::DTOutput("data_table")
      )
    ),
    nav_panel(
      "About",
      div(class = "p-2", style = "max-width: 700px;",
        p("This page shows the example figure from",
          a("scifig_plot_examples_R", href = "https://github.com/macromeer/scifig_plot_examples_R",
            target = "_blank"),
          "and lets you change its settings and download the result as PNG or SVG."),
        p("The plots are made by the same R code as", code("figure_example.R"),
          "(", code("R/figure.R"), "). The app runs R in your browser (webR), so nothing is sent to a server,",
          "including data you upload in the Data tab."),
        p("The figure is drawn at a fixed size (1148 × 686 px at 96 dpi), because the insets in panels B",
          "and D are positioned for that size. The downloads use the same size; higher dpi only adds pixels.",
          "Fonts can differ slightly from the PNG/SVG/PDF that the script writes.")
      )
    )
  )
)

# Server ----

# Settings from the inputs. Inputs that are not set (yet) keep their default.
read_opts <- function(input) {
  opts <- defaults
  set <- function(name, value) if (!is.null(value)) opts[[name]] <<- value
  set_element <- function(name, i, value) if (!is.null(value)) opts[[name]][i] <<- value
  for (name in c("base_size", "sigma", "show_inset", "show_polygons", "jitter_seed", "point_alpha",
                 "shear_constant", "width_line", "velocity_line", "viridis_option")) {
    set(name, input[[name]])
  }
  for (i in 1:2) set_element("group_colours", i, input[[paste0("colour", i)]])
  for (i in 1:3) set_element("decay", i, input[[paste0("decay", i)]])
  if (!is.null(input$ribbon_width)) opts$ribbon_width <- input$ribbon_width / 100
  opts$panels <- input$panels %||% character() # unticking every box gives NULL
  opts
}

# Messages for settings that can't be drawn
check_opts <- function(opts) {
  numbers <- c(opts$jitter_seed, opts$shear_constant, opts$width_line, opts$velocity_line, opts$decay)
  c(
    if (anyNA(numbers)) "Fill in every number field.",
    if (length(opts$panels) == 0) "Select at least one panel.",
    if (isTRUE(opts$shear_constant <= 0)) "The shear stress constant must be positive.",
    if (isTRUE(opts$width_line < 0.5 || opts$width_line > 50)) "The lumen width line must be between 0.5 and 50 µm.",
    if (isTRUE(opts$velocity_line < 0 || opts$velocity_line > 1)) "The velocity line must be between 0 and 1.",
    if (isTRUE(any(opts$decay <= 0))) "Decay constants must be positive."
  )
}

server <- function(input, output, session) {
  opts <- reactive({
    opts <- read_opts(input)
    problems <- check_opts(opts)
    validate(need(length(problems) == 0, paste(problems, collapse = "\n")))
    opts
  }) |> debounce(500)

  # The data of this session: the repository's data until a CSV replaces a data set
  data <- reactiveVal(fig_data)
  upload_status <- reactiveVal(NULL)

  # One cached reactive per panel, keyed by the settings and data sets that panel uses
  panels <- lapply(setNames(nm = names(panel_builders)), function(id) {
    reactive(panel_builders[[id]](data(), opts())) |>
      bindCache(opts()[panel_settings[[id]]], data()[panel_datasets[[id]]], id, cache = panel_cache)
  })

  figure <- reactive({
    selected <- opts()$panels
    build_figure(lapply(setNames(nm = selected), function(id) panels[[id]]()))
  })

  output$figure_preview <- renderImage({
    file <- tempfile(fileext = ".png")
    save_png(figure(), file, fig_width, fig_height, dpi = 96)
    list(src = file, contentType = "image/png", alt = "Figure preview",
         style = "width: 100%; max-width: 1148px; height: auto;")
  }, deleteFile = TRUE)

  output$panel_preview <- renderImage({
    file <- tempfile(fileext = ".png")
    # same size in inches as in the figure, more pixels: same layout, just bigger
    save_panel_png(panels[[input$single_panel]](), input$single_panel, file, dpi = 240)
    list(src = file, contentType = "image/png", alt = paste("Panel", input$single_panel),
         style = "width: 100%; max-width: 760px; height: auto;")
  }, deleteFile = TRUE)

  output$download_png <- downloadHandler(
    filename = "figure.png",
    content = function(file) save_png(figure(), file, fig_width, fig_height, dpi = as.numeric(input$png_dpi %||% 96))
  )
  output$download_svg <- downloadHandler(
    filename = "figure.svg",
    content = function(file) {
      ggsave(file, figure(), width = fig_width, height = fig_height, bg = "white", device = svglite::svglite)
    }
  )

  # Data ----

  observeEvent(input$upload, {
    dataset <- input$dataset
    file <- input$upload
    checked <- tryCatch(
      check_figure_data(read.csv(file$datapath, check.names = FALSE, stringsAsFactors = FALSE), dataset),
      error = function(e) e
    )
    if (inherits(checked, "error")) {
      upload_status(list(ok = FALSE, text = paste0(file$name, ": ", conditionMessage(checked),
                                                   " The previous data are kept.")))
    } else {
      all_data <- data()
      all_data[[dataset]] <- checked
      data(all_data)
      upload_status(list(ok = TRUE, text = sprintf("%s: %d rows replace %s.", file$name, nrow(checked),
                                                   dataset_label(dataset))))
    }
  })

  observeEvent(input$reset_data, {
    all_data <- data()
    all_data[[input$dataset]] <- fig_data[[input$dataset]]
    data(all_data)
    upload_status(list(ok = TRUE, text = paste(dataset_label(input$dataset), "uses the example data again.")))
  })

  # a message about an upload belongs to the data set it was for
  observeEvent(input$dataset, upload_status(NULL), ignoreInit = TRUE)

  output$dataset_help <- renderUI({
    dataset <- input$dataset %||% "B"
    tagList(
      p(class = "small", dataset_notes[[dataset]]),
      p(class = "small", "Columns: ", code(paste(names(data_schemas()[[dataset]]), collapse = ", "))),
      p(class = "small text-muted", "Axis range: ", format_limits(dataset))
    )
  })

  output$upload_status <- renderUI({
    status <- upload_status()
    if (is.null(status)) return(NULL)
    p(class = paste("small", if (status$ok) "text-success" else "text-danger"), status$text)
  })

  output$download_example <- downloadHandler(
    filename = function() paste0("data_", input$dataset %||% "B", "_example.csv"),
    content = function(file) write.csv(fig_data[[input$dataset %||% "B"]], file, row.names = FALSE)
  )

  output$data_table <- DT::renderDT({
    table <- data()[[input$dataset %||% "B"]]
    numeric_columns <- names(table)[vapply(table, is.numeric, logical(1))]
    DT::datatable(table, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE)) |>
      DT::formatSignif(numeric_columns, digits = 4)
  })

  # Rows outside the fixed axis ranges, for the panels in the figure
  output$range_notes <- renderUI({
    shown <- unlist(panel_datasets[opts()$panels])
    counts <- vapply(shown, function(dataset) count_outside_axes(data()[[dataset]], dataset), numeric(1))
    counts <- counts[counts > 0]
    if (length(counts) == 0) return(NULL)
    div(class = "small text-warning",
        lapply(names(counts), function(dataset) {
          p(class = "mb-1", sprintf("%s: %d row(s) outside the axis range (%s).",
                                    dataset_label(dataset), counts[[dataset]], format_limits(dataset)))
        }))
  })

  # Settings ----

  observeEvent(input$reset, {
    for (id in c("base_size", "sigma", "point_alpha")) updateSliderInput(session, id, value = defaults[[id]])
    updateSliderInput(session, "ribbon_width", value = defaults$ribbon_width * 100)
    for (id in c("jitter_seed", "shear_constant", "width_line", "velocity_line")) {
      updateNumericInput(session, id, value = defaults[[id]])
    }
    for (i in 1:3) updateNumericInput(session, paste0("decay", i), value = defaults$decay[i])
    for (id in c("show_inset", "show_polygons")) update_switch(id, value = defaults[[id]], session = session)
    updateCheckboxGroupInput(session, "panels", selected = defaults$panels)
    updateSelectInput(session, "viridis_option", selected = defaults$viridis_option)
    session$sendInputMessage("colour1", list(value = defaults$group_colours[1]))
    session$sendInputMessage("colour2", list(value = defaults$group_colours[2]))
  })
}

shinyApp(ui, server)
