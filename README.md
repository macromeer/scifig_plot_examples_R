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
