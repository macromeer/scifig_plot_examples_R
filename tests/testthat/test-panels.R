test_that("the script runs without warnings or deprecations", {
  expect_equal(fig$warnings, character())
})

for (panel in paste0("panel_", LETTERS[1:6])) {
  test_that(paste(panel, "renders without warnings"), {
    # catches rows dropped by scale limits and NaN positions
    expect_no_warning(ggplotGrob(fig[[panel]]))
  })
}

layer_geoms <- function(plot) vapply(plot$layers, function(l) class(l$geom)[1], character(1))

test_that("panel A labels each image with its file format", {
  text <- Filter(function(l) inherits(l$geom, "GeomText"), fig$panel_A$layers)
  # annotate() keeps positions in the layer data and the label in aes_params
  labels <- data.frame(x = vapply(text, function(l) l$data$x, numeric(1)),
                       label = vapply(text, function(l) as.character(l$aes_params$label), character(1)))
  # image1.jpg is on the left, image2.png on the right
  expect_lt(labels$x[labels$label == "JPEG"], 0.5)
  expect_gt(labels$x[labels$label == "PNG"], 0.5)
})

test_that("panel B keeps the edge bars and draws the fitted curve once", {
  built <- ggplot_build(fig$panel_B)
  bars <- built$data[[which(layer_geoms(fig$panel_B) == "GeomBar")]]
  curve <- built$data[[which(layer_geoms(fig$panel_B) == "GeomFunction")]]

  expect_equal(nrow(bars), nrow(fig$data_B)) # data_B is long: one row per n and side
  expect_false(anyNA(bars[c("xmin", "xmax", "ymax")]))
  expect_length(unique(curve$group), 1)
  expect_false(grepl("%", get_labs(fig$panel_B)$y, fixed = TRUE)) # y values are fractions
})

test_that("panel C shows every point once, at the same positions on every build", {
  points1 <- ggplot_build(fig$panel_C)$data[[2]]
  points2 <- ggplot_build(fig$panel_C)$data[[2]]

  expect_equal(nrow(points1), nrow(fig$data_C))
  expect_equal(points1[c("x", "y")], points2[c("x", "y")])
  # outliers are part of the jittered points; the boxplot must not draw them again
  expect_true(is.na(fig$panel_C$layers[[1]]$geom_params$outlier_gp$shape))
})

test_that("panel D places the shear-stress overlay at finite x bounds", {
  # the default -Inf/Inf become NaN on the log10 x scale and the overlay disappears
  overlay <- fig$panel_D$layers[[which(layer_geoms(fig$panel_D) == "GeomCustomAnn")]]
  bounds <- unlist(overlay$geom_params[c("xmin", "xmax")])
  expect_true(all(is.finite(bounds)))
  expect_equal(bounds, c(xmin = 0.5, xmax = 50))
})

test_that("panel E keeps error bars that extend past the axis limits", {
  built <- ggplot_build(fig$panel_E)
  bars <- built$data[which(layer_geoms(fig$panel_E) == "GeomErrorbar")]
  vertical <- bars[[1]]
  horizontal <- bars[[2]]

  expect_equal(nrow(vertical), nrow(fig$data_E))
  expect_false(anyNA(vertical[c("ymin", "ymax")]))
  expect_equal(nrow(horizontal), sum(!is.na(fig$data_E$pos_err_t)))
  expect_false(anyNA(horizontal[c("xmin", "xmax")]))
})
