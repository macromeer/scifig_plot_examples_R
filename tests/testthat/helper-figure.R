# Run from the repository root with:
#   Rscript -e 'testthat::test_dir("tests/testthat")'

repo_root <- normalizePath(test_path("..", ".."))

# Not a CRAN package: always run the vdiffr snapshot tests (they skip "on CRAN" otherwise)
withr::local_envvar(NOT_CRAN = "true", .local_envir = teardown_env())

# Source figure_example.R into a fresh environment, from the repository root,
# and record every warning raised (soft deprecations included).
# Options are passed on, e.g. scifig.save = FALSE or scifig.out_dir = <dir>.
source_figure <- function(...) {
  withr::local_options(lifecycle_verbosity = "warning", ...)
  env <- new.env(parent = globalenv())
  warnings <- character()
  withCallingHandlers(
    source(file.path(repo_root, "figure_example.R"), local = env, chdir = TRUE),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  env$warnings <- warnings
  env
}

# Panels and data shared by all tests; no figure files are written
fig <- source_figure(scifig.save = FALSE)
