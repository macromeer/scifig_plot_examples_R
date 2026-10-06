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

To change font size, colours or model parameters, edit `opts` in `figure_example.R` (all settings and their defaults are listed in `figure_defaults()` in `R/figure.R`).

## Dashboard

`app/` is a Shiny app for the figure: change font size, colours, model parameters and which panels to show, and download the result as PNG or SVG. It uses the same plotting code (`R/figure.R`). To run it locally:

```r
install.packages(c("shiny", "bslib", "ragg"))
shiny::runApp("app")
```

## Layout

| Path | Contents |
|---|---|
| `figure_example.R` | Script that builds and saves the figure |
| `R/figure.R` | Functions for the figure: data loading, one function per panel, saving |
| `app/` | Dashboard app (Shiny) |
| `data/` | CSV data for panels B–F |
| `images/` | Microscopy images for panel A |
| `tests/` | Tests (see below) |

## Tests

The tests check the data loading, that every panel renders without warnings (also with non-default settings), the exported files, the dashboard's server logic, and how each panel looks (visual snapshots with vdiffr). They run on GitHub Actions for every push and pull request. To run them locally:

```r
install.packages(c("testthat", "vdiffr", "withr", "shiny", "bslib", "ragg"))
```

```sh
Rscript -e 'testthat::test_dir("tests/testthat")'
```

If you change a panel on purpose, the snapshot test for it fails. Review the new version and accept it with:

```r
testthat::snapshot_review("snapshots", path = "tests/testthat")
```
