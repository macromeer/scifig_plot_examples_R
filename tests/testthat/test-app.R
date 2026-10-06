# Server logic of the dashboard (app/app.R), without a browser

skip_if_not_installed("shiny")
skip_if_not_installed("bslib")
skip_if_not_installed("DT")
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

# The repository's data, as the app loads it
example_data <- fig$load_figure_data(file.path(repo_root, "data"), file.path(repo_root, "images"))

# A CSV file as fileInput() reports it
upload <- function(df, name = "upload.csv") {
  path <- tempfile(fileext = ".csv")
  write.csv(df, path, row.names = FALSE)
  data.frame(name = name, size = file.size(path), type = "text/csv", datapath = path)
}

test_that("an uploaded CSV replaces one data set", {
  shiny::testServer(app, {
    start(session, dataset = "F")
    new_F <- example_data$F[1:50, ]
    new_F$n <- new_F$n / 2
    session$setInputs(upload = upload(new_F))
    expect_equal(data()$F, new_F, ignore_attr = TRUE)
    expect_equal(data()$C, example_data$C) # the other data sets stay
    expect_true(upload_status()$ok)
    expect_equal(nrow(ggplot_build(panels$F())$data[[1]]), 50)
    expect_match(output$figure_preview$src, "^data:image/png;base64,")
  })
})

test_that("an invalid upload keeps the previous data and says what is wrong", {
  shiny::testServer(app, {
    start(session, dataset = "C")
    three_groups <- example_data$C
    three_groups$type[1] <- "heterozygous"
    session$setInputs(upload = upload(three_groups))
    expect_equal(data()$C, example_data$C)
    expect_false(upload_status()$ok)
    expect_match(upload_status()$text, "at most two groups")
  })
})

test_that("rows outside the axis range are reported", {
  shiny::testServer(app, {
    start(session, dataset = "D2")
    expect_null(output$range_notes)
    fast <- example_data$D2
    fast$velocity[1:3] <- 1.5
    # drawing the preview warns that ggplot removed these rows; the note is the app's report of it
    suppressWarnings(session$setInputs(upload = upload(fast)))
    expect_match(output$range_notes$html, "3 row\\(s\\) outside the axis range")
  })
})

test_that("example CSVs read back as the example data, and reset restores it", {
  shiny::testServer(app, {
    start(session, dataset = "E")
    example <- read.csv(output$download_example, stringsAsFactors = FALSE)
    expect_equal(fig$check_figure_data(example, "E"), example_data$E, ignore_attr = TRUE)

    session$setInputs(upload = upload(example_data$E[1:8, ]))
    expect_equal(nrow(data()$E), 8)
    session$setInputs(reset_data = 1)
    expect_equal(data()$E, example_data$E)
  })
})
