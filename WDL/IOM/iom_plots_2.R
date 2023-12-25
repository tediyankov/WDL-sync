
## IOM paper II dependency ratio graph =========================================

## preliminaries ---------------------------------------------------------------

## packages
install.packages ('devtools')
devtools::install_github ("WorldDataLab/wdl_tools_legacy/worlddataverse"
                         ,ref="main"
                         ,auth_token = "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"
                         ,force = TRUE)

pacman::p_load (tidyverse, data.table, worlddataverse, countrycode)

## paths
base_path <- worlddataverse::get_wdl_path()
wdp_path <- file.path(base_path,'Mpro_2.0')
currentwdp_path <- file.path(wdp_path,'01_Data','R_2022_10_11','2022_10_12_second_2017ppp','03_outputs')
input_path <- file.path (base_path, "IOM", "2nd_paper", "input_data")

## data

# sample economies
focus_countries = c("AUS","BEL","CAN","CHE","CZE","DEU","FRA","HUN","IRL","ISR","NLD","POL","USA")

# pulling WDP data
wdp_raw = readRDS (file.path (currentwdp_path,"05_003_merge_standard_ages_R_2022_10_11_2017ppp_1_USD.rds"))
wdp = wdp_raw %>% worlddataverse::rds_to_mp (ages = F, inc = T, inc_vec = c(0, 12, Inf))
wdp_clean = wdp %>%
  group_by (ccode, year, age_group) %>%
  summarise (hc.pdf.m = sum (hc.pdf.m, na.rm = T)) %>% ungroup () %>%
  filter (ccode %in% focus_countries & year %in% c(2000:2030)) %>%
  mutate (age_group = ifelse (age_group %in% c("[00,05)","[05,10)","[10,15)"), "0_15", 
                              ifelse (age_group %in% c("[65,70)","[70,75)","[75,INF)"), "65_plus", "16_64"))) %>%
  group_by (ccode, year, age_group) %>%
  summarise (hc.pdf.m = sum (hc.pdf.m, na.rm = T)) %>% ungroup ()
  
wdp_dep = wdp_clean %>%
  group_by (ccode, year) %>%
  summarise (dep_ratio = ((hc.pdf.m[age_group == "0_15"] + hc.pdf.m[age_group == "65_plus"]) / hc.pdf.m[age_group == "16_64"]) * 100)

wdp_input <- wdp_clean %>%
  group_by (ccode, year) %>%
  summarise (pop = sum (hc.pdf.m, na.rm = T)) %>%
  left_join (wdp_dep, by = c("ccode", "year")) %>%
  mutate (pop = pop / 1000000) %>%
  filter (year %in% 2020:2030)

## chart with population
dep_ratio_plot = ggplot (wdp_input, 
                         aes (x = year)) + 
  geom_line (aes (y = dep_ratio), size = 1.5, col = "#163C65") + 
  geom_line (aes (y = pop), size = 1.5, col = "#4B7BBC") + 
  scale_y_continuous (name = "Dependence Ratio",
                      sec.axis = sec_axis (~., name = "Population (in Millions)")) +
  theme (
    axis.title.y = element_text (colour = "#163C65", size = 9),
    axis.title.y.right = element_text (colour = "#4B7BBC", size= 9),
    plot.margin = margin(t = 2,  # Top margin
                         r = 2,  # Right margin
                         b = 2,  # Bottom margin
                         l = 2,  # Left margin
                         unit = "cm")
  ) +
  facet_wrap (~ ccode, scales = "free") + 
  ggtitle ("Dependency Ratio vs Population Growth")
  
dep_ratio_plot

## chart with fertility

# load fertility data
# fertility 2000 - 2021
fertility_present <- read.csv (file.path (input_path, "fertility_present.csv")) %>%
  dplyr::select (6, 11:12) %>%
  rename_with (
    ~ case_when (
      . == "ISO3.Alpha.code" ~ "ISO3",
      . == "Year" ~ "year",
      . == "Total.Fertility.Rate..live.births.per.woman." ~ "fertility",
      TRUE ~ .
    )
  ) %>%
  mutate (ccode = countrycode (sourcevar = ISO3,
                               origin = "iso3c",
                               destination = "iso3c",
                               warn = TRUE,
                               nomatch = NA)) %>%
  filter (!(is.na (ccode)) & year %in% 2000:2021) %>%
  filter (ccode %in% c("AUS","BEL","CAN","CHE","CZE","DEU","FRA","HUN","IRL","ISR","NLD","POL","USA")) %>%
  dplyr::select (ccode, year, fertility) %>%
  mutate (fertility = as.numeric (fertility))

# fertility 2022 - 2030
fertility_future <- read.csv (file.path (input_path, "fertility_future.csv")) %>%
  dplyr::select (6, 11:12) %>%
  rename_with (
    ~ case_when (
      . == "ISO3.Alpha.code" ~ "ISO3",
      . == "Year" ~ "year",
      . == "Total.Fertility.Rate..live.births.per.woman." ~ "fertility",
      TRUE ~ .
    )
  ) %>%
  mutate (ccode = countrycode (sourcevar = ISO3,
                               origin = "iso3c",
                               destination = "iso3c",
                               warn = TRUE,
                               nomatch = NA)) %>%
  filter (!(is.na (ccode)) & year <= 2030) %>%
  filter (ccode %in% c("AUS","BEL","CAN","CHE","CZE","DEU","FRA","HUN","IRL","ISR","NLD","POL","USA")) %>%
  dplyr::select (ccode, year, fertility) %>%
  mutate (fertility = as.numeric (fertility))

# fertility 2000 - 2030 + subset for countries of interest
fertility = rbind (fertility_present, fertility_future) %>%
  arrange (ccode, year)

# input data with fertility and dep ratio
wdp_input_2 = wdp_dep %>% left_join (fertility, by = c("ccode", "year"))

## chart
dep_ratio_plot_fert = ggplot (wdp_input_2, 
                         aes (x = year)) + 
  geom_line (aes (y = dep_ratio), size = 1.5, col = "#163C65") + 
  geom_line (aes (y = fertility *20), size = 1.5, col = "#4B7BBC") + 
  scale_y_continuous (name = "Dependence Ratio",
                      sec.axis = sec_axis (~./20, name = "Fertility")) +
  theme (
    axis.title.y = element_text (colour = "#163C65", size = 9),
    axis.title.y.right = element_text (colour = "#4B7BBC", size= 9),
    plot.margin = margin(t = 2,  # Top margin
                         r = 2,  # Right margin
                         b = 2,  # Bottom margin
                         l = 2,  # Left margin
                         unit = "cm")
    ) +
  facet_wrap (~ ccode, scales = "free") + 
  ggtitle ("Dependency Ratio vs Fertility")
dep_ratio_plot_fert









