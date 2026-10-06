test_that("tracheal length files are read without losing the first value", {
  # the data_C files have no header row
  files <- list.files(repo_root, pattern = "^data_C(wt|mu)_E[0-9.]+\\.csv$", full.names = TRUE)
  expect_length(files, 8)
  n_values <- sum(vapply(files, function(f) length(readLines(f)), integer(1)))

  expect_equal(nrow(fig$data_C), n_values)
  expect_true(is.numeric(fig$data_C$trachea_length))
  expect_false(anyNA(fig$data_C$trachea_length))
})

test_that("panel C has both genotypes at every stage", {
  groups <- unique(fig$data_C[c("type", "dev_stage")])
  expect_equal(nrow(groups), 8)
  expect_setequal(groups$type, c("wildtype", "mutant"))
  expect_setequal(groups$dev_stage, c("E8.5", "E9.5", "E10.5", "E11.5"))
})

test_that("panel E has values and error bars for every gene", {
  expect_equal(as.vector(table(fig$data_E$gene)), c(8, 8, 8))
  expect_false(anyNA(fig$data_E[c("t", "C", "pos_err", "neg_err")]))
  # horizontal error bars exist only for gene c
  expect_equal(unique(fig$data_E$gene[!is.na(fig$data_E$pos_err_t)]), "gene c")
})
