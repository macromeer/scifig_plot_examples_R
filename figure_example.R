
# Run from the repository root: data and image files are read via relative paths

# Libraries/packages

library(ggplot2) # Grammar of graphics (>= 3.5.0 for legend.position.inside)
library(patchwork) # Arranging multiple plots into a grid
library(scales)  # Generic plot scaling methods
library(grid)    # A rewrite of the graphics layout capabilities
library(magick)  # graphics and image processing
library(ggforce) # Collection of additional ggplot stats + geoms
# also needs the svglite package for ggsave(..., ".svg") at the end

# with Rscript: use a null device so ggplotGrob() doesn't leave an Rplots.pdf behind
if (!interactive()) pdf(NULL)
library(latex2exp) # Use LaTeX Expressions in Plots

# global font size
base_size=12 

# Manual theme for most panels
# documentation: https://ggplot2.tidyverse.org/reference/theme.html
my_theme <-  function() {
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

img1 <- magick::image_flip(magick::image_read("./image1.jpg"))
img2 <-  magick::image_flip(magick::image_read("./image2.png"))

panel_A <- ggplot() +
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


# Panel B ----

data_B = read.csv("./data_B.csv")

# format data to ggplot's liking
data_B = data.frame("n"=c(data_B$n,data_B$n),
                    "sample"=c(rep("apical side",nrow(data_B)),
                               rep("basal side",nrow(data_B))),
                    "fraction"=c(data_B$fraction1,data_B$fraction2),
                    "err"=c(data_B$err1,data_B$err2)
)

# define lognormal distribution to be called via ggplot2::geom_function
sigma = 0.14
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
  scale_fill_manual(values=c('#f2a340','#998fc2'))+
  scale_x_continuous(expand = c(0, 0), # prevent gap between origin and first tick
                     breaks=c(seq(from =  min(data_B$n), 
                                  to = max(data_B$n),
                                  by = 1)),
                     limits = c(min(data_B$n),max(data_B$n)),
                     oob = oob_keep) + # keep the edge bars (n = 3, 10), which extend past the limits
  scale_y_continuous(expand = c(0, 0),
                     breaks=c(seq(0,0.6,0.1)),
                     limits = c(0,0.6)
  )+
  my_theme() +
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
  my_theme() +
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


# create  grob (grid graphical object) from the inset plot
panel_B_inset <- ggplotGrob(x = inset) 


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

panel_B <- 
  panel_B + 
  annotation_custom(grob = panel_B_inset, 
                    xmin=6.6, xmax=10.1, ymin=0.1, ymax=0.6) +
  annotation_custom(grob = ggplotGrob(polygons), 
                    xmin = 2.36, xmax = 10.68, ymin = -0.192, ymax = 0.085) 


# Panel C ----

# these files have no header row
data_Cwt_E8.5  = read.csv( "./data_Cwt_E8.5.csv", header = FALSE)
data_Cwt_E9.5  = read.csv( "./data_Cwt_E9.5.csv", header = FALSE)
data_Cwt_E10.5 = read.csv("./data_Cwt_E10.5.csv", header = FALSE)
data_Cwt_E11.5 = read.csv("./data_Cwt_E11.5.csv", header = FALSE)
data_Cmu_E8.5  = read.csv( "./data_Cmu_E8.5.csv", header = FALSE)
data_Cmu_E9.5  = read.csv( "./data_Cmu_E9.5.csv", header = FALSE)
data_Cmu_E10.5 = read.csv("./data_Cmu_E10.5.csv", header = FALSE)
data_Cmu_E11.5 = read.csv("./data_Cmu_E11.5.csv", header = FALSE)

# format data to ggplot's liking
data_C = data.frame(
  "type" = c(
    rep(
      "wildtype",
      nrow(data_Cwt_E8.5) +
        nrow(data_Cwt_E9.5) +
        nrow(data_Cwt_E10.5) +
        nrow(data_Cwt_E11.5)
    ),
    rep(
      "mutant",
      nrow(data_Cmu_E8.5) +
        nrow(data_Cmu_E9.5) +
        nrow(data_Cmu_E10.5) +
        nrow(data_Cmu_E11.5)
    )
  ),
  "dev_stage" = c(
    rep("E8.5", nrow(data_Cwt_E8.5)),
    rep("E9.5", nrow(data_Cwt_E9.5)),
    rep("E10.5", nrow(data_Cwt_E10.5)),
    rep("E11.5", nrow(data_Cwt_E11.5)),
    rep("E8.5", nrow(data_Cmu_E8.5)),
    rep("E9.5", nrow(data_Cmu_E9.5)),
    rep("E10.5", nrow(data_Cmu_E10.5)),
    rep("E11.5", nrow(data_Cmu_E11.5))
  ),
  "trachea_length" = c(
    data_Cwt_E8.5[, 1] ,
    data_Cwt_E9.5[, 1] ,
    data_Cwt_E10.5[, 1] ,
    data_Cwt_E11.5[, 1] ,
    data_Cmu_E8.5[, 1] ,
    data_Cmu_E9.5[, 1] ,
    data_Cmu_E10.5[, 1] ,
    data_Cmu_E11.5[, 1]
  )
)

panel_C <- 
  ggplot(data=data_C,
         aes(x=dev_stage,
             y=trachea_length,
             fill=type))+
  geom_boxplot(outlier.shape = NA) + # outliers are already shown by the jittered points
  geom_point(position=position_jitterdodge(seed = 1), # jitter for h-dist, dodge for grouped dists
             pch=21,
             alpha=0.4) +  # transparency
  scale_x_discrete(limits=c("E8.5","E9.5","E10.5","E11.5")) +
  scale_fill_manual(values=c('#f2a340','#998fc2'))+ # custom colors in hex code
  my_theme() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.18,0.95),
        legend.title = element_blank(),
        legend.key = element_blank()
  ) +
  scale_y_continuous(expand = c(0, 0),
                     breaks=c(seq(20,120,by=20)),
                     limits = c(20,120)
  )+
  xlab("developmental stage (days)") + 
  ylab(TeX("tracheal length ($\\mu$m)"))

# Panel D ----

data_D1 = read.csv("./data_D1.csv")
data_D2 = read.csv("./data_D2.csv")

curve_D1 = data.frame(width=data_D1$width,
                      shear_stress=33.28/(pi*18*data_D1$width^2))


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
                limits=c(0.5,50)) +
  scale_y_log10( expand = c(0, 0),
                 # using trans_format from the scales package, but one can also use expressions
                 labels = trans_format('log10', math_format(10^.x)), 
                 breaks=c(0.001,0.01,0.1,1),
                 limits = c(0.001,1)
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



panel_D <- 
  ggplot(data=data_D2, 
         aes(x=width,
             y=velocity))+ 
  # add plot of first dataset as grob as a trick to introduce two y-axes with different scalings
  # finite x bounds (data units) needed: the default -Inf/Inf turn into NaN on a log10 scale
  annotation_custom(ggplotGrob(panel_D1), xmin = 0.5, xmax = 50) +
  geom_point(fill="blue",
             size=3,
             pch=21) + 
  my_theme() + 
  geom_line(color="blue") +
  geom_vline(xintercept = 1.1,
             linetype="dashed") +
  geom_hline(yintercept = 0.9,
             linetype="dashed") +
  scale_x_log10(expand=c(0,0),
                breaks=c(0.5,1,2,5,10,20,50),
                labels=c(0.5,1,2,5,10,20,50), 
                limits=c(0.5,50)) + 
  annotation_logticks(sides = "b") + 
  scale_y_continuous(expand = c(0,0),
                     breaks = seq(0,1,0.1),
                     limits = c(0,1), 
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
  annotate(geom = "text",x =1.4 ,y =0.4 ,label = "italic(b) == 1.1*mu*m",parse=T,angle=90) +
  annotate(geom = "text",x =6.5 ,y =0.6 ,label = "italic(tau) == frac(4*italic(mu*Q),pi*italic(a*b)^2)",parse=T) 
  


# Panel E ----

data_Ea = read.csv("./data_Ea.csv")
data_Eb = read.csv("./data_Eb.csv")
data_Ec = read.csv("./data_Ec.csv")
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

f1 = function(t) 0.2*exp(-t/48)
f2 = function(t) 0.3*exp(-t/60)
f3 = function(t) 0.4*exp(-t/72)
t_grid = seq(0,96,1)

ribbon = data.frame(
  "f2" = f2(t_grid),
  "t" = t_grid
)

manual_pch =c(15,16,17) # available pch: type ?pch 

panel_E <- ggplot(data=data_E) +
  geom_point(aes(x=t,y=C,pch=factor(gene)),size=2) +
  geom_ribbon(data=ribbon, aes(x=t,ymin=0.85*f2,ymax=1.15*f2),
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
  my_theme() + theme(legend.title=element_blank())+
  theme(legend.position = "inside",
        legend.position.inside = c(0.9,0.95)) +
  # oob_keep: error bars that reach past the axis limits are drawn, not dropped
  scale_x_continuous(expand = c(0, 0),
                     breaks = c(seq(0,96,12)),
                     limits = c(0,96),
                     oob = oob_keep
  ) +
  scale_y_continuous(expand = c(0, 0),
                     breaks = c(seq(0,0.4,0.05)),
                     limits = c(0,0.4),
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


# Panel F ----

data_F = read.csv("./data_F.csv")

panel_F <- ggplot(data=data_F,
                  aes(x=K,y=n,
                      size=amplitude,
                      fill=duration))+
  geom_point(pch=21) +
  my_theme() +

  scale_x_log10(expand = c(0, 0),
                labels=c(1,10,100),
                breaks=c(1,10,100),
                limits = c(1,100)) +
  scale_y_continuous(expand = c(0, 0),
                     breaks=c(seq(0,4,by=0.5)),
                     limits = c(0,4)) +
  annotation_logticks(sides='b') +
  scale_size(range = c(1, 3)) +
  scale_fill_viridis_c(option="D") + # viridis color palette, built into ggplot2
  theme(legend.position = "inside",
        legend.position.inside = c(0.9,0.35)) +
  coord_cartesian(clip = "off") +
  xlab(expression(paste("dissociation constant",~~italic("K")," (M)"))) +
  ylab("Hill coefficient n")



# Plotting ----

figure <- wrap_plots(panel_A,panel_B,panel_C,panel_D,panel_E,panel_F) +
  plot_annotation(tag_levels = 'A')

if (interactive()) print(figure)

# Save at a fixed size (1148 x 686 px at 96 dpi) so the layout is the same on every run.
# Positions of the insets in panels B and D are tuned to this size.
fig_width  = 1148/96 # inches
fig_height = 686/96
# Name an outline font explicitly: cairo devices map the default "sans" to "Helvetica",
# which on some Linux systems resolves to a bitmap font (pixelated, missing rotated glyphs).
# On Linux, fontconfig maps "Arial" to the metric-compatible Liberation Sans.
fig_font = "Arial"

# The tests set options(scifig.save = FALSE) to build the panels without writing files,
# and options(scifig.out_dir = ...) to write the files somewhere else.
if (getOption("scifig.save", TRUE)) {
  out_dir = getOption("scifig.out_dir", ".")
  out_file = function(ext) file.path(out_dir, paste0("figure_example.", ext))

  ggsave(out_file("png"), figure, width = fig_width, height = fig_height, dpi = 96, bg = "white",
         family = fig_font)

  # svglite writes the name of the installed font it matched (e.g. "Liberation Sans" on Linux).
  # Replace it with a CSS fallback list so the SVG renders the same on any system.
  svg_font = systemfonts::font_info(fig_font)$family
  ggsave(out_file("svg"), figure, width = fig_width, height = fig_height, bg = "white",
         system_fonts = list(sans = svg_font, symbol = svg_font)) # symbol: Greek letters in plotmath
  svg = readLines(out_file("svg"))
  svg = gsub('font-family: "[^"]*"', 'font-family: Arial, "Liberation Sans", Helvetica, sans-serif', svg)
  writeLines(svg, out_file("svg"))

  ggsave(out_file("pdf"), figure, width = fig_width, height = fig_height, device = cairo_pdf, # cairo: Unicode glyphs (mu)
         family = fig_font)
}

