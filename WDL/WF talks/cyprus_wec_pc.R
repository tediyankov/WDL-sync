

load_wec_dat = function (){
  
  require (worlddataverse)
  
  # loading paths
  base_path <- worlddataverse::get_wdl_path()
  if (is.na (base_path)) {base_path = file.path("/Users",
                                                "teddyyankov",
                                                "Library",
                                                "CloudStorage",
                                                "GoogleDrive-teodor.yankov@worlddata.io",
                                                "Shared drives", 
                                                "DATA_WDL")}
  
  # load binary (Rda) file
  load (file.path (base_path,
                   "world_emissions_clock",
                   "01_data",
                   "WEC_data_binary_20230425.Rda"))
  
  # rename data file
  wec_dat = WDL_IIASA_data_consolidated_ind_essd
  
  # changing all 0s to NAs
  wec_dat [wec_dat == 0] <- NA
  
  .GlobalEnv$wec_dat = wec_dat
  .GlobalEnv$base_path = base_path
  
} 
load_wec_dat()

load_wec_dat()

wec_dat_cyprus = wec_dat %>%
  filter (iso3c == "CYP") %>%
  dplyr::group_by (iso3c, year, pop) %>%
  dplyr::summarise (em = sum(base, na.rm = T)) %>%
  mutate (em_pc = em / pop) 

ggplot (data = wec_dat_cyprus,
        aes (x = year, y = em_pc)) +
  geom_line (linewidth = 1) +
  theme_wdl () + 
  labs (title = 'Cyprus GHG emissions per capita over time',
        subtitle = "2010-2050",
        y = 'GHG emissions per capita, T CO2eq\n',
        x = '\nYear')
