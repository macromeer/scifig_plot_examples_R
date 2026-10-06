# Run the full script once, writing the figure files to a temporary directory
out_dir <- withr::local_tempdir(.local_envir = teardown_env())
exported <- source_figure(scifig.out_dir = out_dir)
out_file <- function(ext) file.path(out_dir, paste0("figure_example.", ext))

test_that("the script writes PNG, SVG and PDF without warnings", {
  expect_equal(exported$warnings, character())
  for (ext in c("png", "svg", "pdf")) {
    expect_true(file.exists(out_file(ext)), label = ext)
    expect_gt(file.size(out_file(ext)), 0)
  }
})

test_that("the PNG has the intended pixel size", {
  png <- magick::image_info(magick::image_read(out_file("png")))
  expect_equal(c(png$width, png$height), c(1148, 686))
})

test_that("the SVG uses the font fallback list everywhere", {
  svg <- readLines(out_file("svg"))
  families <- unique(regmatches(svg, regexpr("font-family: [^;']+", svg)))
  expect_equal(families, 'font-family: Arial, "Liberation Sans", Helvetica, sans-serif')
})

test_that("the PDF embeds outline fonts only, no Type 3 bitmap fonts", {
  skip_if(Sys.which("pdffonts") == "", "pdffonts (poppler-utils) is not installed")
  fonts <- system2("pdffonts", shQuote(out_file("pdf")), stdout = TRUE)
  fonts <- fonts[-(1:2)] # header lines
  expect_gt(length(fonts), 0)
  expect_false(any(grepl("Type 3", fonts)), label = paste(fonts, collapse = "\n"))
})
