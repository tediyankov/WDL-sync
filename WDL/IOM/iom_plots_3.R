

## IOM paper 2 data viz ========================================================

## Prelims

# packages
pacman::p_load (tidyverse, data.table, worlddataverse, countrycode)

## paths
base_path <- worlddataverse::get_wdl_path()
wdp_path <- file.path(base_path,'Mpro_2.0')
currentwdp_path <- file.path(wdp_path,'01_Data','R_2022_10_11','2022_10_12_second_2017ppp','03_outputs')
input_path <- file.path (base_path, "IOM", "2nd_paper", "input_data")
output_path <- file.path (base_path, "IOM", "2nd_paper", "output")

worlddataverse::font_wdl()

## population grid plot (w and w/out migration) --------------------------------

## loading data

# flows pred by edu_age breakdown + filtered for countries of interest
flows <- read.csv (file.path (output_path, "flows_edu_age.csv")) %>%
  filter (dest %in% c("AUS","BEL","CAN","CHE","CZE","DEU","FRA","HUN","IRL","ISR","NLD","POL","USA"))

# flows pred by edu_age breakdown but summed by destination
flows_sums <- read.csv (file.path (output_path, "flows_edu_age_sums.csv")) %>%
  filter (dest %in% c("AUS","BEL","CAN","CHE","CZE","DEU","FRA","HUN","IRL","ISR","NLD","POL","USA")) %>%
  dplyr::select (2:3, 11) %>%
  rename_with (
    ~ case_when (
      . == "dest" ~ "ccode",
      TRUE ~ .
    )
  ) %>%
  relocate (ccode, year, flow_total)

# population data
wdp_raw = readRDS (file.path (currentwdp_path,"05_003_merge_standard_ages_R_2022_10_11_2017ppp_1_USD.rds"))
wdp = wdp_raw %>% worlddataverse::rds_to_mp (ages = F, inc = T, inc_vec = c(0, 15, 35, 65, Inf))
wdp_clean = wdp %>%
  group_by (ccode, year, age_group) %>%
  summarise (hc.pdf.m = sum (hc.pdf.m, na.rm = T)) %>% ungroup () %>%
  filter (ccode %in% focus_countries & year %in% c(2000:2030)) %>%
  mutate (age_group = ifelse (age_group %in% c("[00,05)","[05,10)","[10,15)"), "0_15", 
                              ifelse (age_group %in% c("[65,70)","[70,75)","[75,INF)"), "65_plus", "16_64"))) %>%
  group_by (ccode, year) %>%
  summarise (hc.pdf.m = sum (hc.pdf.m, na.rm = T)) %>% ungroup ()

# combining into one data frame
input_data = flows_sums %>%
  left_join (wdp_clean, by = c("ccode", "year")) %>%
  arrange (ccode) %>%
  rename_with (
    ~ case_when (
      . == "hc.pdf.m" ~ "pop",
      TRUE ~ .
    )
  ) %>%
  transform (pop_nomig = pop - flow_total) %>%
  pivot_longer (4:5, names_to = "migYN", values_to = "pop")

# chart
pop_plot <- ggplot (input_data %>% transform (pop = pop / 1000000), aes (x = year, y = pop, group = migYN, col = factor (migYN))) + 
  geom_line () + 
  labs (title = "Population Growth", 
        subtitle = "Comparing standard and no migration scenarios, population in Millions", 
        x = "Year", 
        y = "Population") + 
  theme_bw () + 
  worlddataverse::scale_color_wdl() +
  facet_wrap (~ ccode, scales = "free") + 
  scale_y_continuous (labels = scales::number_format (accuracy = 0.01)) + 
  theme (aspect.ratio = 1)
pop_plot

pop_plot_20_30 <- ggplot (input_data %>% transform (pop = pop / 1000000) %>% filter (year %in% 2020:2030), 
                          aes (x = year, y = pop, group = migYN, col = factor (migYN))) + 
  geom_line () + 
  labs (title = "Population Growth", 
        subtitle = "Comparing standard and no migration scenarios, population in Millions", 
        x = "Year", 
        y = "Population") + 
  theme_bw () + 
  facet_wrap (~ ccode, scales = "free") + 
  scale_y_continuous (labels = scales::number_format (accuracy = 0.01)) + 
  theme (aspect.ratio = 1)
pop_plot_20_30

pop_plot_15_30 <- ggplot (input_data %>% 
                            transform (pop = pop / 1000000) %>% 
                            filter (year %in% 2015:2030) %>%
                            mutate (migYN = ifelse (migYN == "pop", "with migration", "without migration")), 
                       aes (x = year, y = pop, group = migYN, col = factor (migYN))) + 
  geom_line () + 
  labs (title = "Population Growth", 
        subtitle = "Comparing standard and no migration scenarios, population in Millions", 
        x = "Year", 
        y = "Population") + 
  theme_bw () + 
  scale_color_manual (name = NULL) + 
  worlddataverse::scale_color_wdl() +
  guides (col = guide_legend (title = "")) + 
  facet_wrap (~ ccode, scales = "free") + 
  scale_y_continuous (labels = scales::number_format (accuracy = 0.01)) + 
  theme (aspect.ratio = 1)
pop_plot_15_30

ggsave ("pop_plot_15_30.pdf", pop_plot_15_30)

pop_plot_10_30 <- ggplot (input_data %>% transform (pop = pop / 1000000) %>% filter (year %in% 2010:2030), 
                          aes (x = year, y = pop, group = migYN, col = factor (migYN))) + 
  geom_line () + 
  labs (title = "Population Growth", 
        subtitle = "Comparing standard and no migration scenarios, population in Millions", 
        x = "Year", 
        y = "Population") + 
  theme_bw () + 
  facet_grid (~ ccode, scales = "free", space = "free") + 
  scale_y_continuous (labels = scales::number_format (accuracy = 0.01)) + 
  theme (aspect.ratio = 1)
pop_plot_10_30

## dependency ratio chart ------------------------------------------------------

wdp_dep = wdp %>%
  group_by (ccode, year, age_group) %>%
  summarise (hc.pdf.m = sum (hc.pdf.m, na.rm = T)) %>% ungroup () %>%
  filter (ccode %in% focus_countries & year %in% c(2000:2040)) %>%
  mutate (age_group = ifelse (age_group %in% c("[00,05)","[05,10)","[10,15)"), "0_15", 
                              ifelse (age_group %in% c("[65,70)","[70,75)","[75,INF)"), "65_plus", "16_64"))) %>%
  group_by (ccode, year, age_group) %>%
  summarise (hc.pdf.m = sum (hc.pdf.m, na.rm = T)) %>% ungroup () %>%
  group_by (ccode, year) %>%
  summarise (dep_ratio = ((hc.pdf.m[age_group == "0_15"] + hc.pdf.m[age_group == "65_plus"]) / hc.pdf.m[age_group == "16_64"]) * 100)

wdp_dep_20_50 = wdp_dep %>% filter (year %in% c(2020, 2030, 2040))

dep_ratio_growth_plot = ggplot (wdp_dep_20_50, aes (x = year, y = dep_ratio, fill = factor (year))) +
  geom_bar (stat = "identity") +
  labs (title = "Dependency Ratio Growth", 
        subtitle = "Comparing dependency ratios for sample countries in 2020, 2030 and 2050", 
        x = "Year", 
        y = "Dependency Ratio") + 
  theme_bw () + 
  worlddataverse::scale_fill_wdl() +
  theme (legend.position = "none", 
         axis.text.x = element_text (angle = 45, vjust = 0.5)) +
  facet_grid (~ ccode, scales = "free", space = "free") 
dep_ratio_growth_plot

ggsave ("dep_ratio_growth_plot.pdf", dep_ratio_growth_plot)

# dependency ratio table
dep_ratio_table = wdp_dep_20_50 %>%
  pivot_wider (names_from = year, values_from = dep_ratio) %>%
  rename_with (
    ~ case_when (
      . == "2020" ~ "dep_ratio_2020",
      . == "2030" ~ "dep_ratio_2030",
      . == "2040" ~ "dep_ratio_2040",
      TRUE ~ .
    )
  ) %>%
  gt () %>%
  tab_header (title = "Dependency Ratio Growth for Sample Countries") %>%
  tab_source_note (source_note = "Source: World Data Lab, World Data Pro (2022)") %>%
  tab_style (locations = cells_column_labels (columns = everything()),
             style = list(cell_borders(sides = "bottom", weight = px(3)),
                          cell_text(weight = "bold"))) %>%
  tab_style(
    locations = cells_title (groups = "title"),
    style = list (cell_text (weight = "bold", size = 24))) %>%
  cols_label(
    dep_ratio_2020 = "Dependency Ratio, 2020",
    dep_ratio_2020 = "Dependency Ratio, 2030",
    dep_ratio_2020 = "Dependency Ratio, 2040")
 # ) %>%
   #(align = "left", columns = ccode)
dep_ratio_table

write_csv (dep_ratio_table, "dep_ratio_table.csv")

install.packages("sjPlot")
library (sjPlot)

sjPlot::sjtab (dep_ratio_table, fun = "xtab")

# population vs migrants age distribution









## population age distribution: local vs migrant

wdp = wdp_raw %>% worlddataverse::rds_to_mp (ages = F, inc = T, inc_vec = c(0, 15, 35, 65, Inf))
wdp_clean_2 = wdp %>%
  group_by (ccode, year, age_group) %>%
  summarise (hc.pdf.m = sum (hc.pdf.m, na.rm = T)) %>% ungroup () %>%
  filter (ccode %in% focus_countries & year %in% c(2000:2030)) %>%
  mutate (age_group = ifelse (age_group %in% c("[00,05)","[05,10)","[10,15)"), "0_14", 
                              ifelse (age_group %in% c("[15,20)","[20,25)","[25,30)","[30,35)"), "15_34",
                                      ifelse (age_group %in% c("[35,40)", "[40,45)", "[45,50)", "[50,55)", "[55,60)", "[60,65)"), "35_64", "65_plus")))) %>%
  group_by (ccode, year, age_group) %>%
  summarise (hc.pdf.m = sum (hc.pdf.m, na.rm = T)) %>% 
  mutate (age_group = ifelse (age_group %in% c("0_14", "65_plus"), "others", age_group)) %>%
  group_by (ccode, year, age_group) %>%
  summarise (pop = sum (hc.pdf.m, na.rm = T))

flow_sums_2 <- flows_sums %>%
  rename_with (
    ~ case_when (
      . == "dest" ~ "ccode",
      TRUE ~ .
    )
  ) %>%
  dplyr::select (-c("X", "flow_total")) %>%
  pivot_longer (3:9, names_to = "age_group", values_to = "pop") %>%
  mutate (age_group = ifelse (age_group %in% c("flow_pri_15_34", "flow_sec_15_34", "flow_ter_15_34"), "15_34", 
                              ifelse (age_group %in% c("flow_pri_35_64","flow_sec_35_64","flow_ter_35_64"), "35_64", "others"))) %>%
  group_by (ccode, year, age_group) %>%
  summarise (pop_mig = sum (pop, na.rm = T)) %>%
  left_join (wdp_clean_2, by = c("ccode", "year", "age_group")) %>%
  group_by (ccode, year) %>%
  mutate (natives = pop / sum (pop, na.rm = T),
          migrants = pop_mig / sum (pop_mig, na.rm = T)) %>%
  dplyr::select (ccode, year, age_group, natives, migrants) %>%
  pivot_longer (4:5, names_to = "group", values_to = "share") %>%
  arrange (ccode, year, group)

# chart

flow_sums_3 = flow_sums_2 %>% 
  mutate (share = share * 100) %>% 
  filter (year == 2020) %>%
  filter (!(ccode %in% c("BEL", "CHE", "CZE", "HUN", "NLD")))
  
age_dist_plot_20 <- ggplot (flow_sums_2 %>% 
                              mutate (share = share * 100) %>% 
                              filter (year == 2020) %>%
                              filter (!(ccode %in% c("BEL", "CHE", "CZE", "HUN", "NLD"))), 
                         aes (x = group, y = share, fill = age_group)) + 
  geom_bar (position = "stack", stat = "identity") + 
  labs (title = "Comparing the age distribution of migrants vs native population",
        subtitle = "Focus on 2020",
        x = "Group of people",
        y = "Age share") + 
  worlddataverse::scale_fill_wdl() +
  worlddataverse::theme_wdl() + 
  facet_wrap (~ ccode, ncol = 4)
age_dist_plot_20

age_dist_plot_30 <- ggplot (flow_sums_2 %>% 
                              mutate (share = share * 100) %>% 
                              filter (year == 2030) %>%
                              filter (!(ccode %in% c("BEL", "CHE", "CZE", "HUN", "NLD"))), 
                            aes (x = group, y = share, fill = age_group)) + 
  geom_bar (position = "stack", stat = "identity") + 
  labs (title = "Comparing the age distribution of migrants vs native population",
        subtitle = "Focus on 2030",
        x = "Group of people",
        y = "Age share") + 
  worlddataverse::scale_fill_wdl() +
  theme_bw () + 
  guides (fill = guide_legend (title = "Age Group")) + 
  facet_wrap (~ ccode, ncol = 4)
age_dist_plot_30

ggsave ("age_dist_plot_30.pdf", age_dist_plot_30)

age_dist_plot_15 <- ggplot (flow_sums_2 %>% 
                              mutate (share = share * 100) %>% 
                              filter (year == 2015) %>%
                              filter (!(ccode %in% c("BEL", "CHE", "CZE", "HUN", "NLD"))), 
                            aes (x = group, y = share, fill = age_group)) + 
  geom_bar (position = "stack", stat = "identity") + 
  labs (title = "Comparing the age distribution of migrants vs native population",
        subtitle = "Focus on 2015",
        x = "Group of people",
        y = "Age share") + 
  worlddataverse::scale_fill_wdl() +
  worlddataverse::theme_wdl() + 
  facet_wrap (~ ccode, ncol = 4)
age_dist_plot_15

age_dist_plot_25 <- ggplot (flow_sums_2 %>% 
                              mutate (share = share * 100) %>% 
                              filter (year == 2025) %>%
                              filter (!(ccode %in% c("BEL", "CHE", "CZE", "HUN", "NLD"))), 
                            aes (x = group, y = share, fill = age_group)) + 
  geom_bar (position = "stack", stat = "identity") + 
  labs (title = "Comparing the age distribution of migrants vs native population",
        subtitle = "Focus on 2025",
        x = "Group of people",
        y = "Age share") + 
  worlddataverse::scale_fill_wdl() +
  worlddataverse::theme_wdl() + 
  facet_wrap (~ ccode, ncol = 4)
age_dist_plot_25

## checking "UN predicts that by 2050, 25% of people in OECD countries will be over 65 years old, three times the proportion in 1950"

# subset object OECD
OECD = c("Austria", "Belgium", "Czech Republic", "Denmark", "Estonia",
         "Finland", "France", "Germany", "Greece", "Hungary", "Iceland",
         "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg",
         "Netherlands", "Norway", "Poland", "Portugal", "Slovak Republic",
         "Slovenia", "Spain", "Sweden", "Switzerland", "United Kingdom",
         "Canada", "Chile", "Colombia", "Mexico", "Costa Rica",
         "United States", "Australia", "Japan", "Korea", "New Zealand",
         "Israel", "Turkey")

# converting to ISO3 country codes
library (countrycode)
OECD_ISO3 = countrycode (sourcevar = OECD,
                         origin = "country.name",
                         destination = "iso3c",
                         warn = TRUE,
                         nomatch = NA)

# shaping wdp data 
wdp_age_65 = wdp %>%
  
  # coding the new age groups
  mutate (age_group = ifelse (age_group %in% c("[65,70)","[70,75)","[75,INF)"), "65_", "0_65")) %>%
  
  # obtaining population totals per age group regardless of spending group
  group_by (ccode, year, age_group) %>%
  summarise (hc.pdf.m = sum (hc.pdf.m, na.rm = T)) %>% ungroup () %>%
  
  # rename hc.pdf.m to pop
  rename_with (~ case_when (. == "hc.pdf.m" ~ "pop", TRUE ~ .)) %>%
  
  # isolating OECD countries
  filter (ccode %in% OECD_ISO3) %>%
  
  # obtaining OECD totals for age groups and years
  group_by (year, age_group) %>%
  summarise (pop = sum (pop, na.rm = T)) %>% ungroup () %>%
  
  # computing 65+ share per population for each year
  group_by (year) %>%
  mutate (share = ((pop [age_group == "65_"]) / (pop [age_group == "65_"] + pop [age_group == "0_65"])) *100) %>%
  
  # reducing the variables to year and 65+ share
  dplyr::select (year, share) %>%
  distinct ()

## OECD dependency ratio numbers
wdp_dep_OECD = wdp_dep %>% 
  
  # isolating OECD countries
  filter (ccode %in% OECD_ISO3) %>%
  
  # aggregating into OECD averages
  group_by (year) %>%
  summarise (dep_ratio = mean (dep_ratio, na.rm = T))





