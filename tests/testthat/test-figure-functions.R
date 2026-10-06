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

test_that("the repository's data passes check_figure_data() unchanged", {
  for (dataset in names(fig$data_schemas())) {
    expect_equal(fig$check_figure_data(data[[dataset]], dataset), data[[dataset]],
                 ignore_attr = TRUE, label = dataset)
  }
})

test_that("check_figure_data() reads data as it comes from a CSV file", {
  file <- withr::local_tempfile(fileext = ".csv")
  write.csv(data$E[data$E$gene != "gene c", c("gene", "t", "C", "pos_err", "neg_err")], file, row.names = FALSE)
  checked <- fig$check_figure_data(read.csv(file, stringsAsFactors = FALSE), "E")
  # the horizontal error columns are optional
  expect_named(checked, names(fig$data_schemas()$E))
  expect_true(all(is.na(checked$pos_err_t)))
})

test_that("check_figure_data() names the column and row of a problem", {
  bad_C <- data$C
  bad_C$trachea_length <- as.character(bad_C$trachea_length)
  bad_C$trachea_length[5] <- "n/a"
  expect_error(fig$check_figure_data(bad_C, "C"), "Column trachea_length, data row 5: 'n/a' is not a number")

  bad_C <- data$C
  bad_C$type[3] <- ""
  expect_error(fig$check_figure_data(bad_C, "C"), "Column type, data row 3: value missing")

  expect_error(fig$check_figure_data(data$D1["width"], "D1"), "Missing column\\(s\\): shear_stress")
  expect_error(fig$check_figure_data(data$F[0, ], "F"), "no data rows")
})

test_that("check_figure_data() rejects data the panels can't draw", {
  three_groups <- data$C
  three_groups$type[1] <- "heterozygous"
  expect_error(fig$check_figure_data(three_groups, "C"), "at most two groups \\(found 3\\)")

  four_genes <- data$E
  four_genes$gene[1] <- "gene d"
  expect_error(fig$check_figure_data(four_genes, "E"), "at most three genes \\(found 4\\)")

  zero_width <- data$D2
  zero_width$width[2] <- 0
  expect_error(fig$check_figure_data(zero_width, "D2"), "Column width, data row 2: must be positive")

  one_sided <- data$E
  one_sided$neg_err_t[which(!is.na(one_sided$pos_err_t))[1]] <- NA
  expect_error(fig$check_figure_data(one_sided, "E"), "give both or neither")
})

test_that("count_outside_axes() counts rows outside the fixed axis ranges", {
  for (dataset in names(fig$axis_limits())) {
    expect_equal(fig$count_outside_axes(data[[dataset]], dataset), 0, label = dataset)
  }
  shifted <- data$C
  shifted$trachea_length[1:2] <- 150
  shifted$dev_stage[3] <- "E12.5"
  expect_equal(fig$count_outside_axes(shifted, "C"), 3)
})
