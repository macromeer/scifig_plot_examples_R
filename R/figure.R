# Functions that build the figure. Used by figure_example.R (and the dashboard app):
#   data   <- load_figure_data("data", "images")
#   panels <- build_panels(data, figure_defaults())
#   figure <- build_figure(panels)
#   save_figure(figure, ".")

# Libraries/packages

library(ggplot2) # Grammar of graphics (>= 3.5.0 for legend.position.inside)
library(patchwork) # Arranging multiple plots into a grid
library(scales)  # Generic plot scaling methods
library(grid)    # A rewrite of the graphics layout capabilities
library(magick)  # graphics and image processing
library(ggforce) # Collection of additional ggplot stats + geoms
library(latex2exp) # Use LaTeX Expressions in Plots
# also needs the svglite package for ggsave(..., ".svg") in save_figure()

# Save at a fixed size (1148 x 686 px at 96 dpi) so the layout is the same on every run.
# Positions of the insets in panels B and D are tuned to this size.
fig_width  = 1148/96 # inches
fig_height = 686/96

# Settings ----

# Every value that can be changed without touching the plotting code
figure_defaults <- function() {
  list(
    base_size = 12,                         # global font size
    group_colours = c('#f2a340','#998fc2'), # panels B and C, custom colors in hex code
    sigma = 0.14,                           # B: width of the lognormal distribution
    show_inset = TRUE,                      # B: interior angle plot
    show_polygons = TRUE,                   # B: regular polygons below the x axis
    jitter_seed = 1,                        # C: same jitter on every run
    point_alpha = 0.4,                      # C: transparency of the points
    shear_constant = 33.28,                 # D: shear stress = constant/(pi*18*width^2)
    width_line = 1.1,                       # D: dashed vertical line (lumen width b)
    velocity_line = 0.9,                    # D: dashed horizontal line (relative velocity)
    decay = c(48, 60, 72),                  # E: decay constants (h) of the three curves
    ribbon_width = 0.15,                    # E: band around the middle curve (fraction)
    viridis_option = "D",                   # F: viridis palette, "A" to "H"
    panels = LETTERS[1:6]                   # panels in the figure
  )
}

# Data ----

# Reads the CSV files and images. Returns one tidy data frame per panel
# (D has two: D1 and D2) and the two images for panel A.
load_figure_data <- function(data_dir = "data", image_dir = "images") {
  data_file <- function(name) file.path(data_dir, name)

  # these files have no header row: one file per genotype and stage
  read_C <- function(type, file_type, stage) {
    values <- read.csv(data_file(paste0("data_C", file_type, "_", stage, ".csv")), header = FALSE)
    data.frame("type" = rep(type, nrow(values)),
               "dev_stage" = rep(stage, nrow(values)),
               "trachea_length" = values[, 1])
  }
  stages <- c("E8.5", "E9.5", "E10.5", "E11.5")
  data_C <- do.call(rbind, c(lapply(stages, function(s) read_C("wildtype", "wt", s)),
                             lapply(stages, function(s) read_C("mutant", "mu", s))))

  # the three gene files name their error columns differently
  data_Ea = read.csv(data_file("data_Ea.csv"))
  data_Eb = read.csv(data_file("data_Eb.csv"))
  data_Ec = read.csv(data_file("data_Ec.csv"))
  data_E = data.frame("gene"=c(rep("gene a",nrow(data_Ea)),
                               rep("gene b",nrow(data_Eb)),
                               rep("gene c",nrow(data_Ec))),
                      "t"=c(data_Ea$t,data_Eb$t,data_Ec$t),
                      "C"=c(data_Ea$C,data_Eb$C,data_Ec$C),
                      "pos_err"=c(data_Ea$err,data_Eb$pos_err,data_Ec$pos_err_C),
                      "neg_err"=c(data_Ea$err,data_Eb$neg_err,data_Ec$neg_err_C),
                      "pos_err_t"=c(rep(NA, nrow(data_Ea)),rep(NA, nrow(data_Eb)),data_Ec$pos_err_t),
                      "neg_err_t"=c(rep(NA, nrow(data_Ea)),rep(NA, nrow(data_Eb)),data_Ec$neg_err_t)
  )

  list(
    B = read.csv(data_file("data_B.csv")),
    C = data_C,
    D1 = read.csv(data_file("data_D1.csv")),
    D2 = read.csv(data_file("data_D2.csv")),
    E = data_E,
    F = read.csv(data_file("data_F.csv")),
    images = list(magick::image_read(file.path(image_dir, "image1.jpg")),
                  magick::image_read(file.path(image_dir, "image2.png")))
  )
}

# Expected columns and their types for each data frame returned by load_figure_data()
data_schemas <- function() {
  list(
    B = c(n = "numeric", fraction1 = "numeric", fraction2 = "numeric", err1 = "numeric", err2 = "numeric"),
    C = c(type = "character", dev_stage = "character", trachea_length = "numeric"),
    D1 = c(width = "numeric", shear_stress = "numeric"),
    D2 = c(width = "numeric", velocity = "numeric"),
    E = c(gene = "character", t = "numeric", C = "numeric", pos_err = "numeric", neg_err = "numeric",
          pos_err_t = "numeric", neg_err_t = "numeric"),
    F = c(K = "numeric", n = "numeric", amplitude = "numeric", duration = "numeric")
  )
}

# Fixed axis ranges, by data set and column. Values outside them are not shown
# (panel E draws them past the axis). Panel B's x axis follows the data.
axis_limits <- function() {
  list(
    B = list(fraction1 = c(0, 0.6), fraction2 = c(0, 0.6)),
    C = list(trachea_length = c(20, 120), dev_stage = c("E8.5", "E9.5", "E10.5", "E11.5")),
    D1 = list(width = c(0.5, 50), shear_stress = c(0.001, 1)),
    D2 = list(width = c(0.5, 50), velocity = c(0, 1)),
    E = list(t = c(0, 96), C = c(0, 0.4)),
    F = list(K = c(1, 100), n = c(0, 4))
  )
}

# Checks a data set (e.g. an uploaded CSV) against data_schemas() and returns it with the
# columns in schema order and the right types. Stops with a message that names the column
# and data row of the first problem.
check_figure_data <- function(df, dataset) {
  schema <- data_schemas()[[dataset]]
  optional <- if (dataset == "E") c("pos_err_t", "neg_err_t") else character() # horizontal error bars
  names(df) <- trimws(names(df))

  missing <- setdiff(names(schema), c(names(df), optional))
  if (length(missing) > 0) {
    stop("Missing column(s): ", paste(missing, collapse = ", "),
         ". Expected: ", paste(names(schema), collapse = ", "), ".", call. = FALSE)
  }
  for (col in setdiff(optional, names(df))) df[[col]] <- NA_real_
  df <- df[names(schema)]
  if (nrow(df) == 0) stop("The file has no data rows.", call. = FALSE)

  problem <- function(col, row, what) {
    stop(sprintf("Column %s, data row %d: %s", col, row, what), call. = FALSE)
  }
  for (col in names(schema)) {
    x <- df[[col]]
    if (is.character(x)) x <- trimws(x)
    blank <- is.na(x) | (is.character(x) & x %in% "")
    if (schema[[col]] == "numeric") {
      number <- suppressWarnings(as.numeric(x))
      not_number <- which(!blank & is.na(number))
      if (length(not_number) > 0) problem(col, not_number[1], sprintf("'%s' is not a number.", x[not_number[1]]))
      df[[col]] <- number
    } else {
      df[[col]] <- as.character(x)
    }
    if (!col %in% optional && any(blank)) problem(col, which(blank)[1], "value missing.")
  }

  positive <- list(D1 = c("width", "shear_stress"), D2 = "width", F = "K")[[dataset]] # log axes
  for (col in positive) {
    if (any(df[[col]] <= 0)) problem(col, which(df[[col]] <= 0)[1], "must be positive (log axis).")
  }
  if (dataset == "C" && length(unique(df$type)) > 2) {
    stop("Column type: panel C has two colours, so at most two groups (found ",
         length(unique(df$type)), ").", call. = FALSE)
  }
  if (dataset == "E") {
    if (length(unique(df$gene)) > 3) {
      stop("Column gene: panel E has three point shapes, so at most three genes (found ",
           length(unique(df$gene)), ").", call. = FALSE)
    }
    one_sided <- which(is.na(df$pos_err_t) != is.na(df$neg_err_t))
    if (length(one_sided) > 0) problem("pos_err_t/neg_err_t", one_sided[1], "give both or neither.")
  }
  df
}

# Number of rows of a data set with a value outside the fixed axis ranges
count_outside_axes <- function(df, dataset) {
  limits <- axis_limits()[[dataset]]
  outside <- rep(FALSE, nrow(df))
  for (col in names(limits)) {
    x <- df[[col]]
    range <- limits[[col]]
    outside <- outside | if (is.character(range)) !x %in% range else (!is.na(x) & (x < range[1] | x > range[2]))
  }
  sum(outside)
}

# format data_B to ggplot's liking: one row per n and side
long_data_B <- function(data_B) {
  data.frame("n"=c(data_B$n,data_B$n),
             "sample"=c(rep("apical side",nrow(data_B)),
                        rep("basal side",nrow(data_B))),
             "fraction"=c(data_B$fraction1,data_B$fraction2),
             "err"=c(data_B$err1,data_B$err2)
  )
}

# Theme ----

# Manual theme for most panels
# documentation: https://ggplot2.tidyverse.org/reference/theme.html
my_theme <-  function(base_size = 12) {
  theme(
    aspect.ratio = 1,
    axis.line =element_line(colour = "black"),

    # shift axis text closer to axis bc ticks are facing inwards
    axis.text.x = element_text(size = base_size*0.8, color = "black",
                               lineheight = 0.9,
                               margin=margin(0.3,0.3,0.3,0.3, unit = "cm")),
    axis.text.y = element_text(size = base_size*0.8, color = "black",
                               lineheight = 0.9,
                               margin=margin(0.3,0.3,0.3,0.3, unit = "cm")),

    axis.ticks = element_line(color = "black", linewidth  =  0.2),
    axis.title.x = element_text(size = base_size,
                                color = "black",
                                margin = margin(t = -5)),
    # t (top), r (right), b (bottom), l (left)
    axis.title.y = element_text(size = base_size,
                                color = "black", angle = 90,
                                margin = margin(r = -5)),
    axis.ticks.length = unit(-0.3, "lines"),
    legend.background = element_rect(color = NA,
                                     fill = NA),
    legend.key = element_rect(color = "black",
                              fill = "white"),
    legend.key.size = unit(0.5, "lines"),
    legend.key.height =NULL,
    legend.key.width = NULL,
    legend.text = element_text(size = 0.6*base_size,
                               color = "black"),
    legend.title = element_text(size = 0.6*base_size,
                                face = "bold",
                                hjust = 0,
                                color = "black"),
    legend.direction = "vertical",
    legend.box = NULL,
    panel.background = element_rect(fill = "white",
                                    color  =  NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = base_size,
                              color = "black"),
  )


}

# Panel A ----

panel_a <- function(data, opts = figure_defaults()) {
  img1 <- magick::image_flip(data$images[[1]])
  img2 <- magick::image_flip(data$images[[2]])

  ggplot() +
    annotation_custom(rasterGrob(image =  img1,
                                 x=0.27,
                                 y=0.49,
                                 width = unit(0.45,"npc"),
                                 height = unit(0.87,"npc")),
                      -Inf, Inf, -Inf, Inf) +
    annotation_custom(rasterGrob(image = img2,
                                 x=0.73,
                                 y=0.49,
                                 width = unit(0.45,"npc"),
                                 height = unit(0.87,"npc")),
                      -Inf, Inf, -Inf, Inf) +

    geom_ellipse(aes(x0 = 0.25,
                     y0 = 0.3,
                     a = 0.1,
                     b = 0.04,
                     angle = 0),
                 color="yellow",
                 linewidth=1)+
    scale_x_continuous(limits = c(0,1))+
    scale_y_continuous(limits=c(0,1)) +
    geom_segment(aes(x=0.15,
                     xend=0.2,
                     y=0.75,
                     yend=0.7),
                 arrow = arrow(length=unit(0.30,"cm"),
                               ends="last",
                               type = "closed"),
                 linewidth = 1,
                 color="white") +
    geom_segment(aes(x=0.3,
                     xend=0.9,
                     y=0.7,
                     yend=0.7),
                 arrow = arrow(length=unit(0.30,"cm"),
                               ends="both",
                               type = "closed"),
                 linewidth = 1,
                 color="red") +


    annotate("text", x = 0.25, y = 0.5, label = "JPEG",color="white") + # image1.jpg
    annotate("text", x = 0.75, y = 0.5, label = "PNG",color="white") +  # image2.png
    annotate("text", x = 0.25, y = 1, label = "image 1",color="black") +
    annotate("text", x = 0.75, y = 1, label = "image 2",color="black") +
    annotate("text", x = 0.39, y = 0.07, label = "20~mu*m",color="white",parse=T) +
    annotate("text", x = 0.89, y = 0.07, label = "20~mu*m",color="white",parse=T) +
    geom_segment(aes(x=0.33,xend=0.45,y=0.03,yend=0.03), linewidth = 2,color="white") +
    geom_segment(aes(x=0.83,xend=0.95,y=0.03,yend=0.03),linewidth = 2,color="white")  +
    theme_void() +# blank plot w/o axes etc.
    theme(plot.margin = unit(c(0,0,1,0), "cm"),
          aspect.ratio = 1)
}

# Panel B ----

panel_b <- function(data, opts = figure_defaults()) {
  base_size <- opts$base_size
  data_B <- long_data_B(data$B)

  # define lognormal distribution to be called via ggplot2::geom_function
  sigma = opts$sigma
  logn_dist <- function(n) exp(-(log(n)-log(6))^2/(2*sigma^2))/(sqrt(2*pi)*sigma*n)

  panel_B <-
    ggplot(data=data_B,(aes(x=n, # aes: aesthetics
                            y=fraction,
                            fill=sample)))+
    geom_bar(stat = "identity",
             position=position_dodge()) + # dodge overlapping objects side-to-side
    geom_function(fun=logn_dist,
                  linetype="solid",
                  inherit.aes = FALSE) + # otherwise drawn once per fill group
    geom_errorbar(aes(ymin=fraction-err,
                      ymax=fraction+err),
                  width=.2,
                  position=position_dodge(.9)) +
    scale_fill_manual(values=opts$group_colours)+
    scale_x_continuous(expand = c(0, 0), # prevent gap between origin and first tick
                       breaks=c(seq(from =  min(data_B$n),
                                    to = max(data_B$n),
                                    by = 1)),
                       limits = c(min(data_B$n),max(data_B$n)),
                       oob = oob_keep) + # keep the edge bars (n = 3, 10), which extend past the limits
    scale_y_continuous(expand = c(0, 0),
                       breaks=c(seq(0,0.6,0.1)),
                       limits = axis_limits()$B$fraction1
    )+
    my_theme(base_size) +
    # some extra theme tweaking
    theme(legend.position = "inside",
          legend.position.inside = c(0.18,0.95),
          legend.title = element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.x = element_text(vjust=-0.5),
          axis.title.x = element_text(vjust=-0.5),
          legend.key.size = unit(0.4, "lines"),
          plot.margin = unit(c(0,1,0,0), "cm"),
          aspect.ratio = 1
    ) +
    xlab(expression(paste("number of neighbors ",italic("n")))) +
    ylab("fraction of cells")


  inset_curve <- function(n) 180*(n-2)/n

  # now comes the inset plot

  inset <-
    ggplot() + geom_function(fun = inset_curve) +
      annotate("text",
               x=6.5,y=160,
               label=TeX("$180^\\circ(\\textit{n}-2)/\\textit{n}$"),
               parse=TRUE,size=2) +
    scale_x_continuous(expand = c(0, 0),
                       breaks=c(seq(3,10,1)),
                       limits = c(3,10)) +
    scale_y_continuous(expand = c(0, 0),
                       breaks=c(seq(60,180,20)),
                       limits = c(60,180)
                      ) +
    my_theme(base_size) +
    xlab(expression(italic(n)))+
    # expression doc: https://stat.ethz.ch/R-manual/R-devel/library/grDevices/html/plotmath.html
    ylab(expression(interior~angle~italic(theta[n])(degree))) +
    # some extra theme tweaking
    theme(
      panel.background = element_blank(),
      plot.background = element_blank(),
      axis.ticks.length = unit(-0.1, "lines"), # make ticks a bit shorter
      axis.title.x = element_text(size = 0.5*base_size,
                                  color = "black",
                                  margin = margin(t = -4)),
      # top (t), right (r), bottom (b), left (l)
      axis.title.y = element_text(size = 0.5*base_size,
                                  color = "black", angle = 90,
                                  margin = margin(r = -4)),
      axis.text.x = element_text(size = base_size*0.5,
                                 color = "black",
                                 lineheight = 0.9,
                                 margin=margin(0.1,0.1,0.1,0.1, unit = "cm")),
      axis.text.y = element_text(size = base_size*0.5, color = "black",
                                 lineheight = 0.9,
                                 margin=margin(0.1,0.1,0.1,0.1, unit = "cm"))
    )


  # now for some regular polygon drawing
  polygons <-
    ggplot() +
      ggforce::geom_regon(aes(x0=c(seq(3,10,1)),
                              y0=rep(-0.3,8),
                              sides = c(seq(3,10,1)),
                              angle=0, r=0.3),
                          fill=NA,
                          color="black") +
    theme_void() + coord_fixed()

  # finally, we can combine everything to plot panel B
  # (ggplotGrob: create grob (grid graphical object) from the inset plot)

  if (opts$show_inset) {
    panel_B <- panel_B +
      annotation_custom(grob = ggplotGrob(x = inset),
                        xmin=6.6, xmax=10.1, ymin=0.1, ymax=0.6)
  }
  if (opts$show_polygons) {
    panel_B <- panel_B +
      annotation_custom(grob = ggplotGrob(polygons),
                        xmin = 2.36, xmax = 10.68, ymin = -0.192, ymax = 0.085)
  }
  panel_B
}

# Panel C ----

panel_c <- function(data, opts = figure_defaults()) {
  ggplot(data=data$C,
         aes(x=dev_stage,
             y=trachea_length,
             fill=type))+
    geom_boxplot(outlier.shape = NA) + # outliers are already shown by the jittered points
    geom_point(position=position_jitterdodge(seed = opts$jitter_seed), # jitter for h-dist, dodge for grouped dists
               pch=21,
               alpha=opts$point_alpha) +  # transparency
    scale_x_discrete(limits=axis_limits()$C$dev_stage) +
    scale_fill_manual(values=opts$group_colours)+
    my_theme(opts$base_size) +
    theme(legend.position = "inside",
          legend.position.inside = c(0.18,0.95),
          legend.title = element_blank(),
          legend.key = element_blank()
    ) +
    scale_y_continuous(expand = c(0, 0),
                       breaks=c(seq(20,120,by=20)),
                       limits = axis_limits()$C$trachea_length
    )+
    xlab("developmental stage (days)") +
    ylab(TeX("tracheal length ($\\mu$m)"))
}

# Panel D ----

panel_d <- function(data, opts = figure_defaults()) {
  data_D1 = data$D1
  data_D2 = data$D2

  curve_D1 = data.frame(width=data_D1$width,
                        shear_stress=opts$shear_constant/(pi*18*data_D1$width^2))


  panel_D1 <- ggplot(data=data_D1,
                     aes(x=width,
                         y=shear_stress)) +
    geom_point(fill="red",
               size=3,
               pch=22) +
    geom_line(data=curve_D1) +
    theme_void()+
    scale_x_log10(expand=c(0,0), # prevent gap between origin and first tick
                  breaks=c(0.5,1,2,5,10,20,50),
                  labels=c(0.5,1,2,5,10,20,50),
                  limits=axis_limits()$D1$width) +
    scale_y_log10( expand = c(0, 0),
                   # using trans_format from the scales package, but one can also use expressions
                   labels = trans_format('log10', math_format(10^.x)),
                   breaks=c(0.001,0.01,0.1,1),
                   limits = axis_limits()$D1$shear_stress
    ) +
    annotation_logticks(sides = "l") +
    theme(
      line = element_blank(),
      # exclude everything outside axes bc it messes with positioning of grob in panel_D
      text = element_blank(),
      title = element_blank(),
      axis.line.y = element_line(colour = "black"),
      aspect.ratio = 1
    ) +
    ylab("shear stress (Pa)")



  ggplot(data=data_D2,
         aes(x=width,
             y=velocity))+
    # add plot of first dataset as grob as a trick to introduce two y-axes with different scalings
    # finite x bounds (data units) needed: the default -Inf/Inf turn into NaN on a log10 scale
    annotation_custom(ggplotGrob(panel_D1), xmin = 0.5, xmax = 50) +
    geom_point(fill="blue",
               size=3,
               pch=21) +
    my_theme(opts$base_size) +
    geom_line(color="blue") +
    geom_vline(xintercept = opts$width_line,
               linetype="dashed") +
    geom_hline(yintercept = opts$velocity_line,
               linetype="dashed") +
    scale_x_log10(expand=c(0,0),
                  breaks=c(0.5,1,2,5,10,20,50),
                  labels=c(0.5,1,2,5,10,20,50),
                  limits=axis_limits()$D2$width) +
    annotation_logticks(sides = "b") +
    scale_y_continuous(expand = c(0,0),
                       breaks = seq(0,1,0.1),
                       limits = axis_limits()$D2$velocity,
                       # putting the y axis of the second plot to the right
                       position = "right",
                       # now the secondary axis becomes the left axis
                       # we need the axis text+title for panel_D1
                       # They were excluded in panel_D1 bc they were messing with the positioning
                       sec.axis = sec_axis(~.,
                                           name = "shear stress (Pa)",
                                           # rescale breaks bc sec_axis inherits scale from primary y axis
                                           breaks=rescale(c(-3,-2,-1,0),
                                                          to = c(0,1)),
                                           labels = c(expression("10"^"-3",
                                                                 "10"^"-2",
                                                                 "10"^"-1",
                                                                 "10"^"0")))

    )  +
    # some extra theme tweaking
    theme(
      aspect.ratio = 1,
      plot.margin = unit(c(0.1,0,0,0.5), "cm"), # to match other panels
      axis.title.y = element_text(margin = margin(r=1)),
      axis.text.y = element_text(margin = margin(r=6)),
      axis.text.y.right = element_text(margin = margin(l=7)),
      axis.title.y.right = element_text(angle = 90)
    ) +
    #xlab(expression(lumen~width~(mu*m))) +
    xlab(TeX("lumen width ($\\mu$m)")) +
    ylab("relative flow velocity") +
    annotate(geom = "text",x =6 ,y =0.85 ,label = "italic(tau) == 0.5~Pa",parse=T) +
    annotate(geom = "text",x =1.4 ,y =0.4 ,label = paste0("italic(b) == ", opts$width_line, "*mu*m"),parse=T,angle=90) +
    annotate(geom = "text",x =6.5 ,y =0.6 ,label = "italic(tau) == frac(4*italic(mu*Q),pi*italic(a*b)^2)",parse=T)
}

# Panel E ----

panel_e <- function(data, opts = figure_defaults()) {
  data_E = data$E

  decay = opts$decay
  f1 = function(t) 0.2*exp(-t/decay[1])
  f2 = function(t) 0.3*exp(-t/decay[2])
  f3 = function(t) 0.4*exp(-t/decay[3])
  t_grid = seq(0,96,1)

  ribbon = data.frame(
    "f2" = f2(t_grid),
    "t" = t_grid
  )

  manual_pch =c(15,16,17) # available pch: type ?pch

  ggplot(data=data_E) +
    geom_point(aes(x=t,y=C,pch=factor(gene)),size=2) +
    geom_ribbon(data=ribbon, aes(x=t,ymin=(1-opts$ribbon_width)*f2,ymax=(1+opts$ribbon_width)*f2),
                fill="black",alpha=0.1) +
    geom_function(fun=f1, linetype="dashed") +
    geom_function(fun=f2) +
    geom_function(fun=f3, linetype="dotted") +
    geom_errorbar(aes(x=t,ymin=C-neg_err, ymax=C+pos_err),
                  width=2) +
    # horizontal error bars only exist for gene c
    geom_errorbar(data=subset(data_E, !is.na(pos_err_t)),
                  aes(y=C,xmin=t-neg_err_t,xmax=t+pos_err_t),
                  orientation="y", width=0.005)+
    scale_shape_manual(values=manual_pch) +
    my_theme(opts$base_size) + theme(legend.title=element_blank())+
    theme(legend.position = "inside",
          legend.position.inside = c(0.9,0.95)) +
    # oob_keep: error bars that reach past the axis limits are drawn, not dropped
    scale_x_continuous(expand = c(0, 0),
                       breaks = c(seq(0,96,12)),
                       limits = axis_limits()$E$t,
                       oob = oob_keep
    ) +
    scale_y_continuous(expand = c(0, 0),
                       breaks = c(seq(0,0.4,0.05)),
                       limits = axis_limits()$E$C,
                       oob = oob_keep
    ) +
    theme(
      panel.grid.major = element_line("gray95", linewidth = 0.1),
      # putting label closer to axis bc exponent makes it bigger
      axis.title.y = element_text(margin = margin(r = -9))
    ) +
    xlab("time (h)") +
    ylab(expression(paste("concentration (mmol",~cm^-3,")"))) +
    coord_cartesian(clip = "off") # to allow for plotting outside axes
}

# Panel F ----

panel_f <- function(data, opts = figure_defaults()) {
  ggplot(data=data$F,
         aes(x=K,y=n,
             size=amplitude,
             fill=duration))+
    geom_point(pch=21) +
    my_theme(opts$base_size) +

    scale_x_log10(expand = c(0, 0),
                  labels=c(1,10,100),
                  breaks=c(1,10,100),
                  limits = axis_limits()$F$K) +
    scale_y_continuous(expand = c(0, 0),
                       breaks=c(seq(0,4,by=0.5)),
                       limits = axis_limits()$F$n) +
    annotation_logticks(sides='b') +
    scale_size(range = c(1, 3)) +
    scale_fill_viridis_c(option=opts$viridis_option) + # viridis color palette, built into ggplot2
    theme(legend.position = "inside",
          legend.position.inside = c(0.9,0.35)) +
    coord_cartesian(clip = "off") +
    xlab(expression(paste("dissociation constant",~~italic("K")," (M)"))) +
    ylab("Hill coefficient n")
}

# Plotting ----

# Named list of the panels in opts$panels, e.g. list(A = <ggplot>, B = <ggplot>, ...)
build_panels <- function(data, opts = figure_defaults()) {
  builders <- list(A = panel_a, B = panel_b, C = panel_c, D = panel_d, E = panel_e, F = panel_f)
  lapply(builders[opts$panels], function(build) build(data, opts))
}

build_figure <- function(panels) {
  wrap_plots(panels) +
    plot_annotation(tag_levels = 'A')
}

# Writes figure_example.png/.svg/.pdf to out_dir at the fixed size
save_figure <- function(figure, out_dir = ".", formats = c("png", "svg", "pdf")) {
  # Name an outline font explicitly: cairo devices map the default "sans" to "Helvetica",
  # which on some Linux systems resolves to a bitmap font (pixelated, missing rotated glyphs).
  # On Linux, fontconfig maps "Arial" to the metric-compatible Liberation Sans.
  fig_font = "Arial"
  out_file = function(ext) file.path(out_dir, paste0("figure_example.", ext))

  if ("png" %in% formats) {
    # device: ggsave() switches to ragg::agg_png when ragg is installed, which has no family argument
    ggsave(out_file("png"), figure, width = fig_width, height = fig_height, dpi = 96, bg = "white",
           device = grDevices::png, family = fig_font)
  }

  if ("svg" %in% formats) {
    # svglite writes the name of the installed font it matched (e.g. "Liberation Sans" on Linux).
    # Replace it with a CSS fallback list so the SVG renders the same on any system.
    svg_font = systemfonts::font_info(fig_font)$family
    ggsave(out_file("svg"), figure, width = fig_width, height = fig_height, bg = "white",
           system_fonts = list(sans = svg_font, symbol = svg_font)) # symbol: Greek letters in plotmath
    svg = readLines(out_file("svg"))
    svg = gsub('font-family: "[^"]*"', 'font-family: Arial, "Liberation Sans", Helvetica, sans-serif', svg)
    writeLines(svg, out_file("svg"))
  }

  if ("pdf" %in% formats) {
    ggsave(out_file("pdf"), figure, width = fig_width, height = fig_height, device = cairo_pdf, # cairo: Unicode glyphs (mu)
           family = fig_font)
  }

  invisible(out_file(formats))
}
