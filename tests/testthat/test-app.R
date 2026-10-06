# Server logic of the dashboard (app/app.R), without a browser

skip_if_not_installed("shiny")
skip_if_not_installed("bslib")
skip_if_not_installed("ragg")

app <- shiny::shinyAppDir(file.path(repo_root, "app"))

# testServer() doesn't run the UI, so inputs start unset (= defaults), except the
# panel checkboxes: unset is the same as none ticked
start <- function(session, ...) {
  session$setInputs(panels = LETTERS[1:6], ...)
  session$elapse(600) # settings are debounced
}

test_that("the app starts from the script's default settings", {
  shiny::testServer(app, {
    start(session)
    expect_equal(opts(), fig$figure_defaults())
    expect_s3_class(figure(), "patchwork")
    expect_match(output$figure_preview$src, "^data:image/png;base64,")
  })
})

test_that("settings reach the panels", {
  shiny::testServer(app, {
    start(session, viridis_option = "B", ribbon_width = 30, colour1 = "#1b9e77", decay1 = 24,
          show_inset = FALSE)
    expect_equal(opts()$viridis_option, "B")
    expect_equal(opts()$ribbon_width, 0.3)
    expect_equal(opts()$group_colours, c("#1b9e77", fig$figure_defaults()$group_colours[2]))
    expect_equal(opts()$decay, c(24, 60, 72))
    expect_false(opts()$show_inset)
    for (id in names(panels)) expect_no_warning(ggplotGrob(panels[[id]]()), message = id)
  })
})

test_that("only the selected panels are in the figure", {
  shiny::testServer(app, {
    session$setInputs(panels = c("B", "E"))
    session$elapse(600)
    expect_equal(opts()$panels, c("B", "E"))
    expect_no_warning(ggplotGrob(figure()))
  })
})

test_that("settings that can't be drawn give a message instead of a figure", {
  shiny::testServer(app, {
    start(session, shear_constant = -1)
    expect_error(opts(), "shear stress constant must be positive")
    session$setInputs(shear_constant = 33.28, decay2 = NA)
    session$elapse(600)
    expect_error(opts(), "Fill in every number field")
    session$setInputs(decay2 = 60, panels = NULL)
    session$elapse(600)
    expect_error(opts(), "Select at least one panel")
  })
})

test_that("the panel tab shows the chosen panel", {
  shiny::testServer(app, {
    start(session, single_panel = "D")
    expect_match(output$panel_preview$src, "^data:image/png;base64,")
  })
})

test_that("the downloads write a PNG at the chosen resolution and an SVG", {
  shiny::testServer(app, {
    start(session, png_dpi = "150")
    png <- magick::image_info(magick::image_read(output$download_png))
    expect_equal(c(png$width, png$height), round(c(1148, 686) * 150 / 96), tolerance = 1)

    svg <- readLines(output$download_svg, warn = FALSE)
    expect_match(svg[1], "^<\\?xml")
    expect_true(any(grepl("<svg", svg)))
  })
})
