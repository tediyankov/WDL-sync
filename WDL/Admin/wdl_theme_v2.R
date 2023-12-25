
theme_wdl <- function (base_size = 8, base_font = NA){
  
  # route to Google Drive
  base_path <- worlddataverse::get_wdl_path()
  
  # for German users
  get_drive_path <- function(){
    dplyr::case_when(
      dir.exists("G:/Geteilte Ablagen") ~ file.path("G:",
                                                    "Geteilte Ablagen",
                                                    "DATA_WDL"),
      dir.exists("G:/Shared Drives") ~ file.path("G:",
                                                 "Shared Drives",
                                                 "DATA_WDL"),
      dir.exists("/Volumes/GoogleDrive/Geteilte Ablagen") ~
        file.path("/Volumes",
                  "GoogleDrive",
                  "Geteilte Ablagen",
                  "DATA_WDL"))
  }
  
  if(is.na(base_path)) {
    base_path = get_drive_path()
  }
  
  # load WDL logo image 
  require ("png")
  img = readPNG (file.path (base_path, "worlddataverse", "wdl_logo.png"))
  
  # load fonts
  require ("showtext")
  font_add_google("Work Sans", "worksans")
  showtext_auto()
  
  ## color palettes
  
  # general sample of theme colours
  pal_worlddataverse1 <- c("#613FC2","#000245","#A08CDA","#D848C4","#FCDC00","#FEF1D3","#00A046","#D6D6D6","#F5F5F5")
  
  # generate purple gradient colors
  pal_worlddataverse2 <- c("#191919","#120D25","#202020","#362870","#483595","#5B44BB","#7B68C7","#9C8DD5","#BDB3E3","#DED9F1")
  
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
  
  # ggplot2 format specs (color, text)
  
  theme_ipsum(base_size=8,base_family="worksans") + 
    theme (
      text = element_text (colour = "#000245", face = "plain"), 
      plot.title = element_text(size = 18, face = "bold", hjust = 0, colour = "#000245", margin = margin(0,0,8,0)), 
      axis.title = element_text(size = 14, face = "italic", colour = "#202020"),
      panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "#F5F5F5"), 
      panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "#F5F5F5"),
      legend.text = element_text (size = 10, colour = "#000245", face = "plain"),
      legend.title = element_text (colour = "#000245", face = "bold")
    ) + 
    scale_colour_discrete_worlddataverse() + 
    scale_color_continuous_worlddataverse() + 
    scale_fill_worlddataverse()
  
}







theme_wdl <- function (a,c){
  
  b = a + theme_ipsum(base_size=8,base_family="worksans") +
    theme (
      text = element_text (colour = "#000245", face = "plain"), 
      plot.title = element_text(size = 18, face = "bold", hjust = 0, colour = "#000245", margin = margin(0,0,8,0)), 
      axis.title = element_text(size = 14, face = "italic", colour = "#202020"),
      panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "#F5F5F5"), 
      panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "#F5F5F5"),
      legend.text = element_text (size = 10, colour = "#000245", face = "plain"),
      legend.title = element_text (colour = "#000245", face = "bold")
    )
  
  c = b + scale_colour_discrete_worlddataverse()
  
  return (c)
  
}

all_plot_sectors = ggplot (data = wec_dat_totals, aes (gdp.pc, emissions.pc, colour = factor(sector))) +
  geom_point(color = pal_worlddataverse2[4]) + 
  geom_smooth (method = "lm",
               formula = y ~ x + I(x^2), 
               size = 1,
               fill = NA) +
  labs (title = "GDP per capita vs. Emissions per capita", subtitle = "All sectors, data from 2020", x = "\nGDP per capita (log scale)", y = "Emissions per capita (log scale)\n") + 
  theme_ipsum(base_size=8,base_family="worksans") + 
  theme_wdl() + 
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.01),
    trans='log') +
  scale_x_continuous(
    labels = scales::number_format(accuracy = 0.01), 
    trans='log')

all_plot_sectors
grid::grid.raster(img, x = 0.07, y = 0.03, 
                  just = c('left', 'bottom'), 
                  width = unit(1.3, 'inches'))








