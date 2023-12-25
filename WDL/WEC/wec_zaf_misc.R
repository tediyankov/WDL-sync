
rm (list = ls())

## paths
base_path <- worlddataverse::get_wdl_path()

## load data file
load (file = file.path (base_path,
                        "world_emissions_clock",
                        "ZAF_subnational",
                        "02_output",
                        "zaf_emis_subnational_27.02.rda"))

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
                   "WEC_data_binary_new.Rda"))
  
  # rename data file
  wec_dat = WDL_IIASA_data_consolidated_ind_essd
  
  # changing all 0s to NAs
  wec_dat [wec_dat == 0] <- NA
  
  .GlobalEnv$wec_dat = wec_dat
  .GlobalEnv$base_path = base_path
  
}

# loading WEC data
load_wec_dat()

wec_dat_zaf_sec = wec_dat %>% ungroup () %>%
  
  # first aggregation
  group_by (year, iso3c, sector) %>%
  summarise (ndc = sum(ndc, na.rm = T), 
             o_1p5c = sum(o_1p5c, na.rm = T), 
             base = sum(base, na.rm = T)) %>%
  
  # filtering for ZAF 
  filter (iso3c == "ZAF")

## computing ZAF totals
wec_dat_zaf_totals = wec_dat_zaf_sec %>% 
  group_by (year, iso3c) %>%
  summarise(ndc = sum(ndc, na.rm = T), 
            o_1p5c = sum(o_1p5c, na.rm = T), 
            base = sum(base, na.rm = T)) %>% 
  mutate (sector = "Total", .after = iso3c)

## joining to make complete ZAf df
wec_dat_zaf = rbind (wec_dat_zaf_sec, wec_dat_zaf_totals) %>%
  
  # renaming vars
  rename_with (
    ~ case_when (
      . == "sector" ~ "sec",
      . == "ndc" ~ "ndc_tot",
      . == "o_1p5c" ~ "o_1p5c_tot",
      . == "base" ~ "base_tot",
      TRUE ~ .
    )
  )


wec_dat_zaf_prov_wtot = emis_sub_nat_sec %>%
  
  # executing join
  left_join (wec_dat_zaf, by = c("year", "sec")) %>%
  
  # computing shares 
  mutate (ndc_share = ndc / ndc_tot,
          o_1p5c_share = o_1p5c / o_1p5c_tot,
          base_share = base / base_tot) %>%
  
  # keeping only req vars
  dplyr::select (year, prov, sec, ndc_share, o_1p5c_share, base_share)

## new chart
library (scales)

## cleaning data 
zaf_pie_dat = wec_dat_zaf_prov_wtot %>%
  
  # keeping only 2023
  filter (year == 2023) %>%
  
  # keeping only base_share
  dplyr::select (-c(ndc_share, o_1p5c_share)) %>%
  
  # adding percentage variable 
  mutate (labels = scales::percent (round (base_share, 2))) %>%
  
  # creating chart 
  ggplot (aes (x = "", y = base_share, fill = prov)) + 
  geom_col (color = "black") +
  geom_label (aes (label = labels),
              color = "white",
              position = position_stack(vjust = 0.5),
              show.legend = F) +
  coord_polar (theta = "y") + 
  facet_wrap (~sec) +
  labs (title = "Province contribtuion to sectoral GHG Emissions", 
        subtitle = "Showing data for 2023") + 
  theme_void () +
  #worlddataverse::theme_wdl() +
  worlddataverse::scale_fill_wdl()

zaf_pie_dat

tlibrary(ggrepel)


zaf_pie_dat2 = wec_dat_zaf_prov_wtot %>%
  
  # keeping only 2023
  filter (year == 2023) %>%
  
  # keeping only base_share
  dplyr::select (-c(ndc_share, o_1p5c_share)) %>%
  
  # adding percentage variable 
  mutate (labels = scales::percent (round (base_share, 2))) %>%
  
  mutate(csum = rev(cumsum(rev(base_share))), 
         pos = base_share/2 + lead(csum, 1),
         pos = if_else(is.na(pos), base_share/2, pos))



  
  ggplot (data = zaf_pie_dat2, aes (x = "", y = base_share, fill = prov)) + 
  geom_col (width = 1, color = 1) +
  geom_label_repel (data = zaf_pie_dat2,
                    aes(y = pos, label = labels),
                    size = 4.5, nudge_x = 1, show.legend = FALSE) +
  coord_polar (theta = "y") + 
  facet_wrap (~sec) +
  labs (title = "Province contribtuion to sectoral GHG Emissions", 
        subtitle = "Showing data for 2023") + 
  theme_void () +
  #worlddataverse::theme_wdl() +
  worlddataverse::scale_fill_wdl()






