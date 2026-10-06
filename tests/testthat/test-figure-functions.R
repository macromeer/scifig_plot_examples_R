# The functions in R/figure.R, called with other inputs than figure_example.R uses

data <- fig$load_figure_data(file.path(repo_root, "data"), file.path(repo_root, "images"))

test_that("loaded data has the columns and types of data_schemas()", {
  schemas <- fig$data_schemas()
  expect_setequal(names(schemas), setdiff(names(data), "images"))
  for (panel in names(schemas)) {
    types <- vapply(data[[panel]], function(x) if (is.numeric(x) || all(is.na(x))) "numeric" else class(x)[1],
                    character(1))
    expect_equal(types, schemas[[panel]], label = panel)
  }
})

test_that("panels build without warnings for non-default settings", {
  opts <- modifyList(fig$figure_defaults(), list(
    base_size = 9, group_colours = c("grey30", "skyblue"), sigma = 0.2,
    jitter_seed = 2, point_alpha = 0.8, shear_constant = 50, width_line = 2,
    velocity_line = 0.5, decay = c(30, 40, 50), ribbon_width = 0.3, viridis_option = "B"
  ))
  panels <- fig$build_panels(data, opts)
  expect_named(panels, LETTERS[1:6])
  for (panel in names(panels)) {
    expect_no_warning(ggplotGrob(panels[[panel]]), message = panel)
  }
  expect_no_warning(ggplotGrob(fig$build_figure(panels)))
})

test_that("opts$panels selects the panels of the figure", {
  opts <- modifyList(fig$figure_defaults(), list(panels = c("B", "E")))
  panels <- fig$build_panels(data, opts)
  expect_named(panels, c("B", "E"))
  expect_no_warning(ggplotGrob(fig$build_figure(panels)))
})

test_that("panel B leaves out the inset and polygons when switched off", {
  n_custom <- function(plot) sum(vapply(plot$layers, function(l) inherits(l$geom, "GeomCustomAnn"), logical(1)))
  opts <- fig$figure_defaults()
  expect_equal(n_custom(fig$panel_b(data, opts)), 2)
  opts$show_inset <- FALSE
  expect_equal(n_custom(fig$panel_b(data, opts)), 1)
  opts$show_polygons <- FALSE
  expect_equal(n_custom(fig$panel_b(data, opts)), 0)
})
