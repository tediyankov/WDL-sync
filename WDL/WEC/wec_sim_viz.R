
## Plot aesthetics -------------------------------------------------------------

# aesthetics

# packages
pacman::p_load(tibble,tidyverse,ggplot2,ggthemes,plyr,reshape2,grid,lubridate,stringr,showtext,png,grid,cowplot,hrbrthemes,magick)

# general sample of theme colours
pal_worlddataverse1 <- c("#613FC2","#000245","#A08CDA","#D848C4","#FCDC00","#FEF1D3","#00A046","#D6D6D6","#F5F5F5")

# generate purple gradient colors
pal_worlddataverse2 <- c("#191919","#120D25","#202020","#362870","#483595","#5B44BB","#7B68C7","#9C8DD5","#BDB3E3","#DED9F1")

# get fonts
font_add_google("Work Sans", "worksans")
showtext_auto()

# get logo for branding

# for iMac
img <- readPNG(source = file.path("/Volumes/",
                                  "GoogleDrive-115239951252043738907",
                                  "Shared drives",
                                  "DATA_WDL",
                                  "worlddataverse",
                                  "wdl_logo.png"))

# for laptop 
img <- readPNG(source = "/Volumes/GoogleDrive/Shared drives/DATA_WDL/worlddataverse/wdl_logo.png")

# add to override default colour palette of dot plots with a WDL one (applicable when factor() is used in the aes())
scale_colour_discrete_worlddataverse <- function (){
  structure(list(
    scale_color_manual(values = pal_worlddataverse1)
  ))
}

scale_color_continuous_worlddataverse <- function(){
  structure(list(
    scale_fill_gradientn(colours = pal_worlddataverse2)
  ))
}

theme_wdl <- function (base_size = 8, base_font = NA){
  theme_ipsum(base_size=8,base_family="worksans") +
    theme (
      text = element_text (colour = "#000245", face = "plain"),
      plot.title = element_text(size = 18, face = "bold", hjust = 0, colour = "#000245", margin = margin(0,0,8,0)),
      axis.title = element_text(size = 14, face = "italic", colour = "#202020"),
      axis.text.x = element_text (angle = 90, vjust = 0.5, hjust = 1),
      panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "#F5F5F5"),
      panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "#F5F5F5"),
      legend.text = element_text (size = 10, colour = "#000245", face = "plain"),
      legend.title = element_text (colour = "#000245", face = "bold")
    )
}

## line chart (three ranks) ------------------------------------------------------------------

# reformatting data

input_data = em_pc_totals_future %>%
  mutate (rank = c(1,2,3)) %>%
  relocate (rank, .before = 1) %>%
  pivot_longer (2:30, names_to = "year", values_to = "total_em_pc")

# chart

input_data_1 = input_data [input_data$rank == 1,]

line_plot = ggplot (input_data, aes (x = year, y = total_em_pc, group = rank, color = factor (rank))) +
  geom_line () +
  labs (title = "High Wealth Low Emissions: \nchanges in emissions per capita over time",
        subtitle = "2022 - 2050",
        x = "year",
        y = "total yearly emissions per capita (all sectors), tonnes") +
  theme_ipsum(base_size=8,base_family="worksans") +
  theme_wdl() +
  scale_colour_discrete_worlddataverse()

line_plot

## line chart (best case, worst case, average)

# data 




