
# raw data 
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

# data input
fun_input_wec8bn_plot2_old <- function (a,b) {
  
  fun_internal <- function (year) {
    
    year = year
    world_avg = wec_dat [wec_dat$year == year,] %>%
      group_by (year, iso3c, sector) %>%
      summarise (base = sum (base, na.rm = T),
                 ndc = sum (ndc, na.rm = T),
                 o_1p5c = sum (o_1p5c, na.rm = T),
                 pop = mean (pop, na.rm = T)) %>%
      ungroup () %>%
      group_by (year, sector) %>%
      summarise (base = sum (base, na.rm = T),
                 ndc = sum (ndc, na.rm = T),
                 o_1p5c = sum (o_1p5c, na.rm = T),
                 pop = sum (pop, na.rm = T)) %>%
      mutate (pop = replace (pop, sector == "Transport", pop [sector == "Transport"] / 2),
              base_pc = base / pop,
              ndc_pc = ndc / pop,
              o_1p5c_pc = o_1p5c / pop) %>%
      ungroup () %>%
      dplyr::select (year, sector, base_pc, ndc_pc, o_1p5c_pc) %>%
      ungroup () %>%
      group_by (year) %>%
      summarise (base_pc = sum (base_pc, na.rm = T), 
                 ndc_pc = sum (ndc_pc, na.rm = T), 
                 o_1p5c_pc = sum (o_1p5c_pc, na.rm = T))
    
  }
  
  years = seq (from = a, to = b, by = 1)
  datalist = vector("list", length = length (unique (years)))
  
  for (i in years) {datalist [[i]] <- fun_internal (i)}
  
  input_wec8bn_plot2 = do.call (rbind, datalist)
  
  .GlobalEnv$input_wec8bn_plot2 = input_wec8bn_plot2
  
}
fun_input_wec8bn_plot2 (2010,2050)

## this is the right one
fun_input_wec8bn_plot2_new <- function (a,b) {
  
  fun_internal <- function (year) {
    
    year = year
    world_avg = wec_dat [wec_dat$year == year,] %>%
      group_by (year, sector) %>%
      summarise (base = sum (base, na.rm = T),
                 ndc = sum (ndc, na.rm = T),
                 o_1p5c = sum (o_1p5c, na.rm = T)) %>%
      ungroup () %>%
      dplyr::select (year, sector, base, ndc, o_1p5c) %>%
      ungroup () %>%
      group_by (year) %>%
      summarise (base = sum (base, na.rm = T), 
                 ndc = sum (ndc, na.rm = T), 
                 o_1p5c = sum (o_1p5c, na.rm = T))
    
  }
  
  years = seq (from = a, to = b, by = 1)
  datalist = vector("list", length = length (unique (years)))
  
  for (i in years) {datalist [[i]] <- fun_internal (i)}
  
  input_wec8bn_plot2 = do.call (rbind, datalist)
  
  .GlobalEnv$pathways_sectors = input_wec8bn_plot2
  
}
pathways_total <- fun_input_wec8bn_plot2_new (2010,2050)

fun_pathways_sector <- function (a,b) {
  
  fun_internal <- function (year) {
    
    year = year
    
    world_avg = wec_dat [wec_dat$year == year,] %>%
      group_by (year, iso3c, sector) %>%
      summarise (base = sum (base, na.rm = T),
                 ndc = sum (ndc, na.rm = T),
                 o_1p5c = sum (o_1p5c, na.rm = T)) %>%
      ungroup () %>%
      group_by (year, sector) %>%
      summarise (base = sum (base, na.rm = T),
                 ndc = sum (ndc, na.rm = T),
                 o_1p5c = sum (o_1p5c, na.rm = T)) %>%
      ungroup () %>%
      dplyr::select (year, sector, base, ndc, o_1p5c) %>%
      ungroup ()
    
  }
  
  years = seq (from = a, to = b, by = 1)
  datalist = vector("list", length = length (unique (years)))
  
  for (i in years) {datalist [[i]] <- fun_internal (i)}
  
  input_wec8bn_plot2 = do.call (rbind, datalist)
  
  .GlobalEnv$pathways_sector = input_wec8bn_plot2
  
}
pathways_sector <- fun_pathways_sector (2010, 2050)

pathways_total = pathways_total %>%
  mutate (sector = "Total") %>%
  relocate (sector, .before = base)

pathways_dat = rbind (pathways_total, pathways_sector)%>%
  arrange (year)

pathways_countries_2030 = wec_dat [wec_dat$year == 2030,] %>%
  group_by (year, iso3c) %>%
  summarise (base = sum (base, na.rm = T),
             ndc = sum (ndc, na.rm = T),
             o_1p5c = sum (o_1p5c, na.rm = T)) %>%
  dplyr::select (year, iso3c, base, ndc, o_1p5c) %>%
  ungroup () %>%
  filter (iso3c %in% c("USA", "CHN", "IND", "EGY")) %>%
  mutate (base = base / 1000000000,
          ndc = ndc / 1000000000, 
          o_1p5c = o_1p5c / 1000000000)
  


wec_pathway_dat_fun <- function (a,b) {
  
  fun_internal <- function (year) {
    
    year = year
    
    world_avg = wec_dat [wec_dat$year == year,] %>%
      group_by (year, iso3c, sector) %>%
      summarise (base = sum (base, na.rm = T),
                 ndc = sum (ndc, na.rm = T),
                 o_1p5c = sum (o_1p5c, na.rm = T),
                 pop = mean (pop, na.rm = T)) %>%
      ungroup () %>%
      group_by (year, sector) %>%
      summarise (base = sum (base, na.rm = T),
                 ndc = sum (ndc, na.rm = T),
                 o_1p5c = sum (o_1p5c, na.rm = T),
                 pop = sum (pop, na.rm = T)) %>%
      mutate (pop = replace (pop, sector == "Transport", pop [sector == "Transport"] / 2),
              base_pc = base / pop,
              ndc_pc = ndc / pop,
              o_1p5c_pc = o_1p5c / pop) %>%
      ungroup () %>%
      dplyr::select (year, sector, base_pc, ndc_pc, o_1p5c_pc) %>%
      ungroup ()
    
  }
  
  years = seq (from = a, to = b, by = 1)
  datalist = vector("list", length = length (unique (years)))
  
  for (i in years) {datalist [[i]] <- fun_internal (i)}
  
  input_wec8bn_plot2 = do.call (rbind, datalist)
  
  .GlobalEnv$wec_pathway_dat_fun = input_wec8bn_plot2
  
}
wec_pathway_dat <- wec_pathway_dat_fun (2010,2050)

input_wec8bn_plot2 = input_wec8bn_plot2 %>%
  mutate (sector = "Total") %>%
  relocate (sector, .before = base_pc)

wec_pathway_dat_wtotal = rbind (wec_pathway_dat, input_wec8bn_plot2) %>%
  arrange (year)


pathways_dat = pathways_dat %>%
  mutate (sector = as.factor (sector)) %>%
  mutate (sector = factor (sector, levels = c("Total", "Agriculture", "Building", "Energy", "Industry", "Transport"))) %>%
  mutate (base = base / 1000000000, 
          ndc = ndc / 1000000000, 
          o_1p5c = o_1p5c / 1000000000)


# creating plot 
wec8bn_plot2 = ggplot (pathways_dat, 
                       aes (x = year, 
                            y = base)) + 
  geom_line (aes (col = "#0dcdc0")) +
  geom_line (aes (y = ndc, col = "#264653")) +
  geom_line (aes (y = o_1p5c, col = "#e9c46a")) +
  geom_ribbon (aes (ymax = base, ymin = ndc), fill = "#64D9B3", alpha = .2) + 
  geom_ribbon (aes (ymax = ndc, ymin = o_1p5c), fill = "#E76E51", alpha = .2) + 
  labs (title = "Global sectoral GHG emission scenario pathways", 
        subtitle = "2010-2050",
        x = "year", 
        y = "GHG Total Emissions / GTonnes") + 
  theme_wdl () + 
  theme (legend.position = "none") + 
  scale_colour_discrete_worlddataverse () + 
  facet_wrap (~ sector, scales = "free_y") 
  #scale_fill_identity(name = '', guide = 'legend', labels = c('Implementation Gap', 'Ambition Gap')) + 
  #scale_colour_manual(name = '', 
                      #values = c('#64D9B3'='#64D9B3','#264653'='#264653','#E9C46A'='#E9C46A'), 
                      #labels = c('Business as Usual','Do as we promised','Achieve the goal'))


wec8bn_plot2
grid::grid.raster(img, x = 0.07, y = 0.03, just = c('left', 'bottom'), width = unit(1.3, 'inches'))



wec8bn_plot3 = ggplot (wec_pathway_dat_wtotal, 
                       aes (x = year, 
                            y = base_pc)) + 
  geom_line (aes (col = "#0dcdc0")) +
  geom_line (aes (y = ndc_pc, col = "#264653")) +
  geom_line (aes (y = o_1p5c_pc, col = "#e9c46a")) +
  geom_ribbon (aes (ymax = base_pc, ymin = ndc_pc), fill = "#f4a261", alpha = .2) + 
  geom_ribbon (aes (ymax = ndc_pc, ymin = o_1p5c_pc), fill = "#e76f51", alpha = .2) + 
  labs (title = "Global sectoral GHG emission scenario pathways", 
        subtitle = "2010-2050",
        x = "year", 
        y = "GHG Emissions per capita / Tonnes") + 
  theme_wdl () + 
  theme (legend.position = "none") + 
  scale_colour_discrete_worlddataverse () + 
  facet_wrap (~ sector, scales = "free_y") 

wec8bn_plot3





