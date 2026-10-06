# scifig_plot_examples_R
Scientific publication figure plotting examples with R. 

![figure_example](figure_example.png)

## Usage

Requires R and ggplot2 >= 3.5.0. Install the packages once:

```r
install.packages(c("ggplot2", "patchwork", "scales", "magick", "ggforce", "latex2exp", "svglite"))
```

Then, from the repository root:

```sh
Rscript figure_example.R
```

This writes `figure_example.png`, `figure_example.svg` and `figure_example.pdf`.

## Tests

The tests check the data loading, that every panel renders without warnings, the exported files, and how each panel looks (visual snapshots with vdiffr). They run on GitHub Actions for every push and pull request. To run them locally:

```r
install.packages(c("testthat", "vdiffr", "withr"))
```

```sh
Rscript -e 'testthat::test_dir("tests/testthat")'
```

If you change a panel on purpose, the snapshot test for it fails. Review the new version and accept it with:

```r
testthat::snapshot_review("snapshots", path = "tests/testthat")
```
