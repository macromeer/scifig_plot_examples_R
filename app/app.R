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
    colour_input("colour1", "Colour 1 (B: apical side, C: mutant)", defaults$group_colours[1]),
    colour_input("colour2", "Colour 2 (B: basal side, C: wildtype)", defaults$group_colours[2])
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
      "About",
      div(class = "p-2", style = "max-width: 700px;",
        p("This page shows the example figure from",
          a("scifig_plot_examples_R", href = "https://github.com/macromeer/scifig_plot_examples_R",
            target = "_blank"),
          "and lets you change its settings and download the result as PNG or SVG."),
        p("The plots are made by the same R code as", code("figure_example.R"),
          "(", code("R/figure.R"), "). The app runs R in your browser (webR), so nothing is sent to a server."),
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

  # One cached reactive per panel, keyed by the settings that panel uses
  panels <- lapply(setNames(nm = names(panel_builders)), function(id) {
    reactive(panel_builders[[id]](fig_data, opts())) |>
      bindCache(opts()[panel_settings[[id]]], id, cache = panel_cache)
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
