
## Prelims =====================================================================

# packages 
library (tidyverse)
library (worlddataverse)

# building function to load WEC data
load_wec_dat = function (){
  
  # loading paths
  base_path <- worlddataverse::get_wdl_path()
  if (is.na (base_path)) {base_path = file.path("/Volumes/",
                                                "GoogleDrive-115239951252043738907",
                                                "Shared drives",
                                                "DATA_WDL")}
  
  # load binary (Rda) file
  load (file.path (base_path,
                   "world_emissions_clock",
                   "01_data",
                   "WEC_data_binary_new.Rda"))
  
  # rename data file
  wec_dat = WDL_IIASA_data_consolidated_ind_essd
  
  # changing all 0s to NAs
  wec_dat [wec_dat == 0] <- NA
  
  .GlobalEnv$wec_dat = wec_dat
  
}

# loading WEC data
load_wec_dat()

# setting system font
worlddataverse::font_wdl()

## Chart time! =================================================================

IIASA_WDL_global_sector_em <- wec_dat %>% 
  group_by(year, sector) %>% 
  summarise(base = sum(base, na.rm = TRUE),
            ndc = sum(ndc, na.rm = TRUE),
            o_1p5c = sum(o_1p5c, na.rm = TRUE), 
            h_cpol = sum(h_cpol, na.rm = TRUE)) %>% 
  arrange(sector, year)

# Lukas's chart 
p_sectors_global_agg_pie <- ggplot(IIASA_WDL_global_sector_em %>% filter(year == 2022), 
                                   aes(x = "", y = base/10^9, fill = sector)) + 
  geom_bar(stat = "identity", color = "black") + 
  coord_polar("y", start=0) +
  ggrepel::geom_label_repel(aes(label = paste0(round(base/10^9, 1), " GT"), x = 1.25), 
                            position = position_stack(vjust = 0.4), size=10, 
                            show.legend = FALSE, color = c(1, 1, 1, "white", "white")) +
  theme_void() + 
  labs(title = expression(Global~GHG~emissions~"in"~2022~by~sector)) +
  theme(plot.title = element_text(size = 24, face = "bold"),
        plot.caption = element_text(hjust = 0, size = 16), 
        legend.title = element_blank(),
        legend.position = "bottom", 
        legend.text = element_text(size = 20), 
        axis.text = element_blank()) +
  guides(fill = guide_legend(reverse = FALSE)) + 
  scale_fill_viridis_d()

p_sectors_global_agg_pie

# my chart
pie_input <- IIASA_WDL_global_sector_em %>%
  
  filter (year == 2022) %>%
  dplyr::select (year, sector, base) %>%
  
  # compute percentages 
  mutate (fraction = base / sum (base)) %>% 
  
  # compute cumulative percentages 
  mutate (ymax = cumsum (fraction)) %>% 
  
  # compute bottom of each rectangle
  mutate (ymin = c(0, head (ymax, n = -1))) %>%
  
  # compute central label position
  mutate (labelPosition = c(0.1049087, 0.23, 0.46, 0.74, 0.93)) %>%
  
  # compute labels 
  mutate (label = c ("Agriculture: \n12.1 GT", "Building: \n3.2 GT", "Energy: \n20.6 GT", "Industry: \n13.7 GT", "Transport: \n8.2 GT"))

wec_donut = ggplot (pie_input, aes (ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = sector)) + 
  geom_rect () + 
  geom_text (x = 2, aes (y = labelPosition, label = label, color = sector), size = 5) + 
  worlddataverse::scale_fill_wdl2() + 
  worlddataverse::scale_color_wdl2() +
  coord_polar (theta = "y") + 
  xlim (c(-1, 4)) +
  worlddataverse::theme_wdl() + 
  theme_void () + 
  theme (legend.position = "none")
  
wec_donut

  
  
  
  
  












