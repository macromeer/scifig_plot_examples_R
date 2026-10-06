
# Run from the repository root: reads R/, data/ and images/ via relative paths

# The plotting code is in R/figure.R, one function per panel.
# Settings (font size, colours, model parameters) are in figure_defaults() there.
source("R/figure.R", local = TRUE) # local: into the same environment as this script

# with Rscript: use a null device so ggplotGrob() doesn't leave an Rplots.pdf behind
if (!interactive()) pdf(NULL)

opts <- figure_defaults()
# e.g. opts$base_size <- 10 or opts$panels <- c("B", "C")

data <- load_figure_data("data", "images")
data_B <- long_data_B(data$B)
data_C <- data$C
data_E <- data$E

panels <- build_panels(data, opts)
panel_A <- panels$A
panel_B <- panels$B
panel_C <- panels$C
panel_D <- panels$D
panel_E <- panels$E
panel_F <- panels$F

figure <- build_figure(panels)

if (interactive()) print(figure)

# The tests set options(scifig.save = FALSE) to build the panels without writing files,
# and options(scifig.out_dir = ...) to write the files somewhere else.
if (getOption("scifig.save", TRUE)) {
  save_figure(figure, out_dir = getOption("scifig.out_dir", "."))
}
