
## preliminaries ---------------------------------------------------------------------------------

# packages
pacman::p_load(ggplot2,ggthemes,plyr,reshape2,grid,lubridate,stringr,showtext,png,grid,cowplot,hrbrthemes,magick)

# general sample of theme colours
pal_worlddataverse1 <- c("#613FC2","#000245","#A08CDA","#D848C4","#FCDC00","#FEF1D3","#00A046","#D6D6D6","#F5F5F5")

# generate purple gradient colors
pal_worlddataverse2 <- c("#191919","#120D25","#202020","#362870","#483595","#5B44BB","#7B68C7","#9C8DD5","#BDB3E3","#DED9F1")

# get fonts
font_add_google("Work Sans","worksans")
showtext_auto()

# get logo for branding
img <- readPNG(source = "/Volumes/GoogleDrive/Shared drives/DATA_WDL/worlddataverse/wdl_logo.png")

# generating sample scatter plot to test theme
base_scatter <- ggplot(mpg, aes(cty, hwy, color = factor(cyl))) +
  geom_jitter() + 
  geom_abline(colour = "grey50", size = 2)

# generating sample bar plot to test theme
base_bar <- ggplot(mpg, aes(x = class, fill = class)) +
  geom_bar() 

# generating sample gradient plot to test theme
base_gradient = ggplot(faithfuld, aes(waiting, eruptions)) +
  geom_raster(aes(fill = density))
base_gradient

## theme function ---------------------------------------------------------------------------------

# add to override default "fill" colour palette with a WDL one (use for bar plots)
scale_fill_worlddataverse <- function(){
  structure(list(
    scale_fill_manual(values=pal_worlddataverse1)
  ))
}

# add to override default colour palette of dot plots with a WDL one (applicable when factor() is used in the aes())
scale_colour_discrete_worlddataverse <- function (){
  structure(list(
    scale_color_manual(values = pal_worlddataverse1)
  ))
}

# overrides default blue gradient (when one is needed) with a WDL purple gradient. Add to any graph that uses gradient scales (e.g. geom_point or geom_raster).
scale_color_continuous_worlddataverse <- function(){
  structure(list(
    scale_fill_gradientn(colours = pal_worlddataverse2)
  ))
}

theme_wdl <- function (base_size = 8, base_font = NA){
  theme_ipsum(base_size=8,base_family="worksans") + 
    theme (
      text = element_text (colour = "#000245", face = "plain"), 
      plot.title = element_text(size = 18, face = "bold", hjust = 0, colour = "#000245", margin = margin(0,0,5,0)), 
      axis.title = element_text(size = 14, face = "italic", colour = "#202020"),
      panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "#F5F5F5"), 
      panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "#F5F5F5"),
      legend.text = element_text (size = 10, colour = "#000245", face = "plain"),
      legend.title = element_text (colour = "#000245", face = "bold")
    )
}

# logic: to any ggplot, apply theme_wdl function + one of the three colour functions
# depends on the type of ggplot which one of the three colour functions gets applied

## testing theme ---------------------------------------------------------------------------------

base_scatter_styled = base_scatter + 
  labs (title = "Test Title", subtitle = "test subtitle for this graph", x = "test x", y = "test y") + 
  theme_ipsum(base_size=8,base_family="worksans") + 
  theme (
    text = element_text (colour = "#000245", face = "plain"), 
    plot.title = element_text(size = 18, face = "bold", hjust = 0, colour = "#000245", margin = margin(0,0,5,0)), 
    axis.title = element_text(size = 14, face = "italic", colour = "#202020"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "#F5F5F5"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "#F5F5F5"),
    legend.text = element_text (size = 10, colour = "#000245", face = "plain"),
    legend.title = element_text (colour = "#000245", face = "bold")
  ) + 
  scale_colour_discrete_worlddataverse() # chosen because this is a scatter plot.

base_scatter_styled
grid::grid.raster(img, x = 0.07, y = 0.03, just = c('left', 'bottom'), width = unit(1.3, 'inches'))


theme_wdl <- function (base_size = 8, base_font = NA){
  theme_ipsum(base_size=8,base_family="worksans") + 
    theme (
      text = element_text (colour = "#000245", face = "plain"), 
      plot.title = element_text(size = 18, face = "bold", hjust = 0, colour = "#000245", margin = margin(0,0,8,0)), 
      axis.title = element_text(size = 14, face = "italic", colour = "#202020"),
      panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "#F5F5F5"), 
      panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "#F5F5F5"),
      legend.text = element_text (size = 10, colour = "#000245", face = "plain"),
      legend.title = element_text (colour = "#000245", face = "bold")
    ) 
} 













scatter_styled = base_scatter +
  labs (title = "hello", subtitle = "Test subtitle for this graph", x = "test x", y = "test y") + 
  theme_wdl()





base_bar
bar_styled = base_bar + 
  labs (title = "Test Title", subtitle = "test subtitle for this graph", x = "test x", y = "test y") + 
  theme_ipsum(base_size=8,base_family="worksans") + 
  theme (
    text = element_text (colour = "#000245", face = "plain"), 
    plot.title = element_text(size = 18, face = "bold", hjust = 0, colour = "#000245", margin = margin(0,0,5,0)), 
    axis.title = element_text(size = 14, face = "italic", colour = "#202020"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "#F5F5F5"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "#F5F5F5"),
    legend.text = element_text (size = 10, colour = "#000245", face = "plain"),
    legend.title = element_text (colour = "#000245", face = "bold")
  ) +
  scale_fill_worlddataverse() # chosen because this is a bar plot

base_bar_styled
grid::grid.raster(img, x = 0.07, y = 0.03, just = c('left', 'bottom'), width = unit(1.3, 'inches'))

gradient_styled = base_gradient + 
  labs (title = "Test Title", subtitle = "test subtitle for this graph", x = "test x", y = "test y") + 
  theme_ipsum(base_size=8,base_family="worksans") + 
  theme (
    text = element_text (colour = "#000245", face = "plain"), 
    plot.title = element_text(size = 18, face = "bold", hjust = 0, colour = "#000245", margin = margin(0,0,5,0)), 
    axis.title = element_text(size = 14, face = "italic", colour = "#202020"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "#F5F5F5"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "#F5F5F5"),
    legend.text = element_text (size = 10, colour = "#000245", face = "plain"),
    legend.title = element_text (colour = "#000245", face = "bold")
  ) + 
  scale_color_continuous_worlddataverse()
gradient_styled
grid::grid.raster(img, x = 0.07, y = 0.03, just = c('left', 'bottom'), width = unit(1.3, 'inches'))

ggplot(faithfuld, aes(waiting, eruptions)) +
  geom_raster(aes(fill = density))

