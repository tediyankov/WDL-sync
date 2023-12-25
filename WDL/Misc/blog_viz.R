
wdp_raw <- readRDS (file.path (currentwdp_path,"05_003_merge_standard_ages_R_2023_04_11_2017ppp_1_USD.rds"))
wdp = wdp_raw %>% worlddataverse::rds_to_wdpro (ages = F, inc = T, inc_vec = c(0, 12, Inf))

EU = c("Austria", "Belgium", "Bulgaria", "Croatia", "Republic of Cyprus",
       "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Germany",
       "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania",
       "Luxembourg", "Malta", "Netherlands", "Poland", "Portugal", "Romania",
       "Slovakia", "Slovenia", "Spain", "Sweden")

EU_ISO3 <- countrycode (sourcevar = EU,
                        origin = "country.name",
                        destination = "iso3c",
                        warn = TRUE,
                        nomatch = NA)

wdp_eu_us = wdp %>%
  
  # filtering for country
  filter (ccode %in% c(EU_ISO3, "USA", "CHN", "IND")) %>%
  
  # recoding into EU and USA
  mutate (ccode = ifelse (ccode %in% EU_ISO3, "EU", ccode)) %>%
  
  # aggregating total 
  dplyr::group_by (ccode, year) %>%
  dplyr::summarise (pop = sum (hc.pdf.m, na.rm = T),
                    spend = sum (exp.pdf.m, na.rm = T)) %>%
  
  # computing per capita spend
  mutate (spend_pc = spend / pop) %>%
  
  # creating plot
  ggplot (aes (x = year, y = spend_pc, group = ccode, col = ccode)) + 
  geom_line (linewidth = 1) + 
  worlddataverse::theme_wdl() + 
  labs (title = "Growth of Per Capita Spending in EU, USA, China and India",
        subtitle = "2000 - 2050",
        x = "\nYear", 
        y = "Per Capita Spending, USD 2017 PPP\n") + 
  theme (axis.text.x = element_text (angle = 360))

wdp_eu_us


pop_growth = wdp %>%
  
  # filtering for country
  filter (ccode %in% c(EU_ISO3, "USA", "CHN", "IND")) %>%
  
  # recoding into EU and USA
  mutate (ccode = ifelse (ccode %in% EU_ISO3, "EU", ccode)) %>%
  
  # aggregating total 
  dplyr::group_by (ccode, year) %>%
  dplyr::summarise (pop = sum (hc.pdf.m, na.rm = T)) %>%
  
  # converting pop to billions
  mutate (pop = pop / 10^9) %>%
  
  # building line plot
  ggplot (aes (x = year, y = pop, group = ccode, col = ccode)) +
  geom_line () +
  geom_line (linewidth = 1) + 
  worlddataverse::theme_wdl() + 
  labs (title = "Population in EU, USA, China and India",
        subtitle = "2000 - 2050",
        x = "\nYear", 
        y = "Population, Billions") + 
  theme (axis.text.x = element_text (angle = 360))

pop_growth

# getting USA CAGR
pop_cagr = wdp %>%
  
  # filtering for country
  filter (ccode %in% c(EU_ISO3, "USA", "CHN", "IND")) %>%
  
  # recoding into EU and USA
  mutate (ccode = ifelse (ccode %in% EU_ISO3, "EU", ccode)) %>%
  
  # aggregating total 
  dplyr::group_by (ccode, year) %>%
  dplyr::summarise (pop = sum (hc.pdf.m, na.rm = T)) %>%
  
  # getting CAGR
  dplyr::group_by (ccode) %>%
  dplyr::mutate (CAGR = CAGR_fun (pop) * 100) %>% ungroup () %>%
  
  mutate (pop = pop / 10^9)
  
  
  
  
  
  
  
