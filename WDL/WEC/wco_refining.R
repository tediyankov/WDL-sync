
## World Consumer Outlook 2023: refining =================================================================================================================

## Slide 7: converting to grouped bar chart

slide_7_df <- data.frame (year = c(2022, 2023, 2024),
                          imf_weo_gdp_growth = c(3, 2.4, 2.4),
                          wdpro_cc_hc_growth = c(2.7, 2.7, 2.8),
                          wdpro_cc_exp_growth = c(3.7, 3, 3.3)) %>%
  
  # elongating for chart 
  pivot_longer (2:4, names_to = "growth_var", values_to = "pct_delta") %>%
  
  # renaming
  mutate (growth_var = ifelse (growth_var == "imf_weo_gdp_growth", "IMF WEO (GDP growth)", 
                               ifelse (growth_var == "wdpro_cc_hc_growth", "WDPro (CClass Headcounts Growth)", 
                                       ifelse (growth_var == "wdpro_cc_exp_growth", "WDPro (CClass Spending Growth)", NA
                                       ))))
  
## creating plot 
ggplot (data = slide_7_df, aes (x = year, y = pct_delta, fill = growth_var)) + 
  geom_bar (stat = "identity", position = "dodge") + 
  geom_label (aes (label = pct_delta),
              position = position_dodge (width = 0.9),
              show.legend = F) +
  worlddataverse::theme_wdl() + 
  worlddataverse::scale_fill_wdl2() + 
  theme (legend.position = "bottom",
         axis.text.x = element_text (angle = 360)) +
  labs (x = "\nYear",
        y = "Percentage Change",
        title = "Consumer Class Growth (WDL) vs GDP Growth (IMF)") + 
  coord_cartesian(ylim = c(2, 4))

slide_7_df

ggplot(data = slide_7_df, aes(x = year, y = pct_delta, fill = growth_var)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = pct_delta),
             position = position_dodge(width = 0.9),
             show.legend = FALSE,
            vjust = -1) +
  worlddataverse::theme_wdl() +
  worlddataverse::scale_fill_wdl2() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 360)
  ) +
  labs(
    x = "\nYear",
    y = "Percentage Change",
    title = "Consumer Class Growth (WDL) vs GDP Growth (IMF)"
  ) +
  coord_cartesian(ylim = c(2, 4))

## Slide 16: treemap to bar chart and calculating top 2 contributing countries

wdp2 = wdp_raw %>% worlddataverse::rds_to_wdpro (ages = F, inc = T, inc_vec = c(0, 12, 40, 80, 120,  Inf))

slide_16_df = wdp2 %>%
  
  # country-year spending and spending per spending group and age
  dplyr::group_by (year, daily_spending) %>%
  dplyr::summarise (exp = sum (exp.pdf.m, na.rm = T)) %>% ungroup () %>%
  
  # filter years
  filter (year %in% 2023:2024) %>%
  
  # filter out non CC
  filter (!daily_spending == "[0,12)") %>%
  
  # lagging forward
  dplyr::group_by (daily_spending) %>%
  mutate (exp_lag = lag (exp)) %>% ungroup () %>%
  
  # computing delta
  mutate (delta_exp = exp - exp_lag) %>%
  
  # removing NAs
  filter (year == 2024) %>%
  
  # computing share
  mutate (delta_share = delta_exp / sum (delta_exp, na.rm = T)) %>%
  
  # adding label
  mutate (label = paste0 ("+", " ", round (delta_exp / 10^9, 1), " ", "B", " ", "\n(", scales::percent(delta_share, 1), ")")) %>%
  
  # changing levels
  mutate (daily_spending = factor (daily_spending, levels = c("[12,40)", "[40,80)", "[80,120)", "[120,Inf)"))) %>%
  
  # renaming spending groups
  mutate (daily_spending = ifelse (daily_spending == "[12,40)", "Lower Middle", 
                                   ifelse (daily_spending == "[40,80)", "Middle", 
                                           ifelse (daily_spending == "[80,120)", "Upper Middle", 
                                                   ifelse (daily_spending == "[120,Inf)", "Rich", NA 
                                                   ))))) %>%
  
  # changing scale of delta_exp
  mutate (delta_exp = delta_exp / 10^9) %>%
  
  # changing levels
  mutate (daily_spending = factor (daily_spending, levels = c("Lower Middle", "Middle", "Upper Middle", "Rich"))) %>%
  
  # converting to bar chart
  ggplot (aes (x = daily_spending, y = delta_exp, fill = daily_spending)) + 
  geom_bar (stat = "identity") + 
  geom_label (aes (label = label, y = delta_exp), 
              position = position_identity (),
              show.legend = F) + 
  worlddataverse::theme_wdl() + 
  worlddataverse::scale_fill_wdl2() + 
  theme (legend.position = "bottom",
         axis.text.x = element_text (angle = 360)) + 
  labs (x = "\nSpending Group",
        y = "\nChange in Consumer Spending in Billions \n(percentage of total $2.3T added in 2024)",
        title = "$2.3T New Consumers in 2024 \nBreakdown into Spending Groups")

slide_16_df

## current distribution
current_dist = wdp2 %>%
  
  # country-year spending and spending per spending group and age
  dplyr::group_by (year, daily_spending) %>%
  dplyr::summarise (exp = sum (exp.pdf.m, na.rm = T)) %>% ungroup () %>%
  
  # filter years
  filter (year %in% 2023) %>%
  
  # filter out non CC
  filter (!daily_spending == "[0,12)") %>%
  
  # computing shares
  mutate (exp_share = exp / sum (exp, na.rm = T)) %>%
  
  # adding label
  mutate (label = scales::percent(exp_share, 1)) %>%
  
  # changing levels
  mutate (daily_spending = factor (daily_spending, levels = c("[12,40)", "[40,80)", "[80,120)", "[120,Inf)"))) %>%
  
  # renaming spending groups
  mutate (daily_spending = ifelse (daily_spending == "[12,40)", "Lower Middle", 
                                   ifelse (daily_spending == "[40,80)", "Middle", 
                                           ifelse (daily_spending == "[80,120)", "Upper Middle", 
                                                   ifelse (daily_spending == "[120,Inf)", "Rich", NA 
                                                   ))))) %>%
  
  # changing levels
  mutate (daily_spending = factor (daily_spending, levels = c("Lower Middle", "Middle", "Upper Middle", "Rich"))) %>%
  
  # rescaling exp var
  mutate (exp = exp / 10^12) %>%
  
  # creating chart
  ggplot (aes (x = daily_spending, y = exp_share, fill = daily_spending)) + 
  geom_bar (stat = "identity") + 
  geom_label (aes (label = label, y = exp_share), 
              position = position_identity (),
              show.legend = F) + 
  worlddataverse::theme_wdl() + 
  worlddataverse::scale_fill_wdl2() + 
  theme (legend.position = "bottom",
         axis.text.x = element_text (angle = 360)) + 
  labs (x = "\nSpending Group",
        y = "\nConsumer Spending in Trillions \n(percentage of total CC spending)",
        title = "Consumer Spending in 2023")
  
current_dist

## creating a dodge plot with constant and current spending dist

remotes::install_github("coolbutuseless/ggpattern")
library (ggpattern)

slide_16_df = slide_16_df %>% rename (share = delta_share) %>% dplyr::select (year, daily_spending, exp, share, label)
current_dist = current_dist %>% rename (share = exp_share)

joint_exp = rbind (slide_16_df, current_dist) %>%
  
  # converting year var into a current vs added category
  rename (category = year) %>%
  mutate (category = ifelse (category == 2023, "Current distribution (2023)", "Distribution of added spending (2024)")) %>%
  
  # building plot
  ggplot (aes (x = daily_spending, y = share, fill = daily_spending, pattern = category)) +
  geom_bar_pattern(stat = "identity", 
                   position = position_dodge(preserve = "single"),
                   color = "black", 
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.1,
                   pattern_spacing = 0.025,
                   pattern_key_scale_factor = 0.6) +
  scale_fill_discrete(guide = "none") +
  #geom_bar (stat = "identity", position = "dodge") + 
  geom_label (aes (label = label, y = share), 
              position = position_dodge (width = 0.9),
              show.legend = F) + 
  worlddataverse::theme_wdl() + 
  #worlddataverse::scale_fill_wdl() + 
  scale_fill_manual (values = c("white", "white", "white", "white")) + 
  theme (legend.position = "none",
         axis.text.x = element_text (angle = 360)) + 
  labs (x = "\nSpending Group",
        y = "\nShare of Consumer Spending)",
        title = "$2.3T new consumer spending in 2024: \nwhich spending groups add the most?")
joint_exp

## Slide 22: grouping asia and oceania together

wdp_clean_ages = wdp %>% dplyr::ungroup() %>%
  
  # country-year headcounts and spending per spending group and age
  dplyr::group_by (ccode, year, age_group, daily_spending) %>%
  dplyr::summarise (pop = sum (hc.pdf.m, na.rm = T)) %>% dplyr::ungroup() %>%
  
  # recoding age groups into 10-year scale
  dplyr::mutate (age_group = ifelse (age_group %in% c("[00,05)","[05,10)"), '[00,10)', 
                                     ifelse (age_group %in% c("[10,15)","[15,20)"), '[10,20)', 
                                             ifelse (age_group %in% c("[20,25)","[25,30)"), '[20,30)', 
                                                     ifelse (age_group %in% c("[30,35)","[35,40)"), '[30,40)', 
                                                             ifelse (age_group %in% c("[40,45)","[45,50)"), '[40,50)', 
                                                                     ifelse (age_group %in% c("[50,55)","[55,60)"), '[50,60)', 
                                                                             ifelse (age_group %in% c("[60,65)","[65,70)"), '[60,70)', 
                                                                                     ifelse (age_group %in% c("[70,75)","[75,INF)"), '[70,INF)', age_group))))))))) %>%
  
  # aggregating into new groups
  dplyr::group_by (ccode, year, age_group, daily_spending) %>%
  dplyr::summarise (pop = sum (pop, na.rm = T)) %>% dplyr::ungroup() %>%
  
  # recoding spendng groups into CC and not
  dplyr::mutate (daily_spending = ifelse (daily_spending == "[12,Inf)", "CC", "NCC")) %>%
  
  # aggregate 
  dplyr::group_by (ccode, year, age_group, daily_spending) %>%
  dplyr::summarise (pop = sum (pop, na.rm = T)) %>% dplyr::ungroup() %>%
  
  # keeping only consumer class
  dplyr::filter (daily_spending == "CC") %>%
  dplyr::select (-daily_spending) %>%
  
  # years of interest
  dplyr::filter (year %in% c(2022, 2030)) %>%
  
  # computing continent var
  mutate (continent = countrycode (ccode, "iso3c", "continent")) %>%
  
  # combining asia and pacific
  mutate (continent = ifelse (continent %in% c("Asia", "Oceania"), "Asia Pacific", continent)) %>%
  
  # aggregatng over age group
  dplyr::group_by (continent, year, age_group) %>%
  dplyr::summarise (pop = sum (pop, na.rm = T)) %>% dplyr::ungroup() %>%
  
  # computing delta
  dplyr::group_by (continent, age_group) %>%
  dplyr::mutate (pop_lag = Hmisc::Lag (pop, 1),
                 delta_pop = pop - pop_lag) %>% 
  dplyr::ungroup() %>%
  
  # keeping only delta vars
  dplyr::select (continent, year, age_group, delta_pop) %>%
  
  # keeping only non-NAs
  filter (year == 2030) %>%
  
  # turning into millions
  mutate (delta_pop = delta_pop / 10^6) 


## creating chart
ggplot (data = wdp_clean_ages, 
        aes (x = age_group, y = delta_pop, fill = continent)) + 
  geom_bar (stat = "identity", position = "stack") + 
  worlddataverse::theme_wdl() + 
  worlddataverse::scale_fill_wdl2() + 
  theme (legend.position = "bottom", 
         axis.text.x = element_text (angle = 360)) + 
  labs (x = "\nAge Group", 
        y = "\n Absolute Change (Millions)")


## Slide 19: double checking country breakdown

slide_16_df_2 = wdp2 %>%
  
  # country-year spending and spending per spending group and age
  dplyr::group_by (ccode, year, daily_spending) %>%
  dplyr::summarise (exp = sum (exp.pdf.m, na.rm = T)) %>% ungroup () %>%
  
  # filter years
  filter (year %in% 2023:2024) %>%
  
  # filter out non CC
  filter (!daily_spending == "[0,12)") %>%
  
  # lagging forward
  dplyr::group_by (ccode, daily_spending) %>%
  mutate (exp_lag = lag (exp)) %>% ungroup () %>%
  
  # computing delta
  mutate (delta_exp = exp - exp_lag) %>%
  
  # removing NAs
  filter (year == 2024) %>%
  
  # computing share
  mutate (delta_share = delta_exp / sum (delta_exp, na.rm = T)) %>%
  
  # adding label
  mutate (label = paste0 ("+", " ", round (delta_exp / 10^9, 1), " ", "B", " ", "\n(", scales::percent(delta_share, 1), ")")) %>%
  
  # changing levels
  mutate (daily_spending = factor (daily_spending, levels = c("[12,40)", "[40,80)", "[80,120)", "[120,Inf)"))) %>%
  
  # renaming spending groups
  mutate (daily_spending = ifelse (daily_spending == "[12,40)", "Lower Middle", 
                                   ifelse (daily_spending == "[40,80)", "Middle", 
                                           ifelse (daily_spending == "[80,120)", "Upper Middle", 
                                                   ifelse (daily_spending == "[120,Inf)", "Rich", NA 
                                                   ))))) %>%
  
  # changing scale of delta_exp
  mutate (delta_exp = delta_exp / 10^9) %>%
  
  # changing levels
  mutate (daily_spending = factor (daily_spending, levels = c("Lower Middle", "Middle", "Upper Middle", "Rich")))


## Slide 14: breaking down the 10 million from Africa

africa_10m_breakdown = wdp2 %>%
  
  # country-year spending and spending per spending group and age
  dplyr::group_by (ccode, year, daily_spending) %>%
  dplyr::summarise (pop = sum (hc.pdf.m, na.rm = T)) %>% ungroup () %>%
  
  # filter years
  filter (year %in% 2023:2024) %>%
  
  # filter out non CC
  filter (!daily_spending == "[0,12)") %>%
  
  # lagging forward
  dplyr::group_by (ccode, year) %>%
  dplyr::summarise (pop = sum (pop, na.rm = T)) %>% ungroup () %>%
  
  # computing change
  dplyr::group_by (ccode, year) %>%
  pivot_wider (names_from = "year", values_from = "pop") %>%
  mutate (delta = `2024` - `2023`) %>%
  
  # adding continent
  mutate (continent = countrycode (ccode, "iso3c", "continent")) %>%
  
  # filtering only for Africa
  filter (continent == "Africa") %>%
  
  # country names
  mutate (country = countrycode (ccode, "iso3c", "country.name")) %>%
  
  # descending order
  arrange (desc (delta))
  
## Slide 19 (again): dodge bar chart added hc and exp --------------------------

## Distribution for spending
dist_spending = wdp2 %>%
  
  # country-year spending and spending per spending group and age
  dplyr::group_by (year, daily_spending) %>%
  dplyr::summarise (exp = sum (exp.pdf.m, na.rm = T)) %>% ungroup () %>%
  
  # filter years
  filter (year %in% 2023:2024) %>%
  
  # filter out non CC
  filter (!daily_spending == "[0,12)") %>%
  
  # lagging forward
  dplyr::group_by (daily_spending) %>%
  mutate (exp_lag = lag (exp)) %>% ungroup () %>%
  
  # computing delta
  mutate (delta_exp = exp - exp_lag) %>%
  
  # removing NAs
  filter (year == 2024) %>%
  
  # computing share
  mutate (delta_share = delta_exp / sum (delta_exp, na.rm = T)) %>%
  
  # adding label
  mutate (label = paste0 ("+", " ", round (delta_exp / 10^9, 0), " ", "B", " ", "\n(", scales::percent(delta_share, 1), ")")) %>%
  
  # changing levels
  mutate (daily_spending = factor (daily_spending, levels = c("[12,40)", "[40,80)", "[80,120)", "[120,Inf)"))) %>%
  
  # renaming spending groups
  mutate (daily_spending = ifelse (daily_spending == "[12,40)", "Lower Middle", 
                                   ifelse (daily_spending == "[40,80)", "Middle", 
                                           ifelse (daily_spending == "[80,120)", "Upper Middle", 
                                                   ifelse (daily_spending == "[120,Inf)", "Rich", NA 
                                                   ))))) %>%
  
  # changing scale of delta_exp
  mutate (delta_exp = delta_exp / 10^9) %>%
  
  # changing levels
  mutate (daily_spending = factor (daily_spending, levels = c("Lower Middle", "Middle", "Upper Middle", "Rich")))


## Distribution for headcounts
dist_headcounts = wdp2 %>%
  
  # country-year spending and spending per spending group and age
  dplyr::group_by (year, daily_spending) %>%
  dplyr::summarise (pop = sum (hc.pdf.m, na.rm = T)) %>% ungroup () %>%
  
  # filter years
  filter (year %in% 2023:2024) %>%
  
  # filter out non CC
  filter (!daily_spending == "[0,12)") %>%
  
  # lagging forward
  dplyr::group_by (daily_spending) %>%
  mutate (pop_lag = lag (pop)) %>% ungroup () %>%
  
  # computing delta
  mutate (delta_pop = pop - pop_lag) %>%
  
  # removing NAs
  filter (year == 2024) %>%
  
  # computing share
  mutate (delta_share = delta_pop / sum (delta_pop, na.rm = T)) %>%
  
  # adding label
  mutate (label = paste0 ("+", " ", round (delta_pop / 10^6, 0), " ", "M", " ", "\n(", scales::percent(delta_share, 1), ")")) %>%
  
  # changing levels
  mutate (daily_spending = factor (daily_spending, levels = c("[12,40)", "[40,80)", "[80,120)", "[120,Inf)"))) %>%
  
  # renaming spending groups
  mutate (daily_spending = ifelse (daily_spending == "[12,40)", "Lower Middle", 
                                   ifelse (daily_spending == "[40,80)", "Middle", 
                                           ifelse (daily_spending == "[80,120)", "Upper Middle", 
                                                   ifelse (daily_spending == "[120,Inf)", "Rich", NA 
                                                   ))))) %>%
  
  # changing scale of delta_exp
  mutate (delta_pop = delta_pop / 10^6) %>%
  
  # changing levels
  mutate (daily_spending = factor (daily_spending, levels = c("Lower Middle", "Middle", "Upper Middle", "Rich")))


dist_headcounts = dist_headcounts %>% mutate (category = "Distrbution of added CC headcounts") %>%
  dplyr::select (category, daily_spending, delta_share, label)
  
  
dist_spending = dist_spending %>% mutate (category = "Distrbution of added CC spending") %>%
  dplyr::select (category, daily_spending, delta_share, label)

joint_delta = rbind (dist_headcounts, dist_spending) %>%
  
  ggplot (aes (x = daily_spending, y = delta_share, fill = category)) + 
  geom_bar (stat = "identity", position = "dodge") +
  geom_label (aes (label = label, y = delta_share), 
              position = position_dodge (width = 0.9),
              show.legend = F) + 
  worlddataverse::theme_wdl() + 
  theme (legend.position = "bottom",
         axis.text.x = element_text (angle = 360),
         axis.title.y = element_blank(), 
         axis.text.y = element_blank(),
         axis.ticks.y = element_blank()) + 
  labs (x = "\nSpending Group",
        title = "$2.3T new consumer spending in 2024: \nwhich spending groups add the most?")

joint_delta


## Slide 23+24: combine + 10 year age groups -----------------------------------

slide_23 = wdp2 %>%
  
  # country-year spending and spending per spending group and age
  dplyr::group_by (year, daily_spending, age_group) %>%
  dplyr::summarise (pop = sum (hc.pdf.m, na.rm = T)) %>% ungroup () %>%
  
  # isolating consumer class
  filter (!daily_spending == "[0,12)") %>%
  dplyr::group_by (year, age_group) %>%
  dplyr::summarise (pop = sum (pop, na.rm = T)) %>% ungroup () %>%
  
  # 10-yar age groups
  mutate (age_group = ifelse (age_group %in% c("[00,05)", "[05,10)"), "[00,10)",
                              ifelse (age_group %in% c("[10,15)", "[15,20)"), "[10,20)",
                                      ifelse (age_group %in% c("[20,25)", "[25,30)"), "[20,30)",
                                              ifelse (age_group %in% c("[30,35)", "[35,40)"), "[30,40)",
                                                      ifelse (age_group %in% c("[40,45)", "[45,50)"), "[40,50)",
                                                              ifelse (age_group %in% c("[50,55)", "[55,60)"), "[50,60)",
                                                                      ifelse (age_group %in% c("[60,65)", "[65,70)"), "[60,70)",
                                                                              ifelse (age_group %in% c("[70,75)", "[75,INF)"), "[70,INF)", NA
                                                                              ))))))))) %>%
  
  # aggregating into new age groups
  dplyr::group_by (year, age_group) %>%
  dplyr::summarise (pop = sum (pop, na.rm = T)) %>% ungroup () %>%
  
  # filtering year
  filter (year %in% 2023:2024) %>%
  
  # widening
  pivot_wider (names_from = "year", values_from = "pop") %>%
  
  # computing delta
  mutate (delta = `2024` - `2023`) %>%
  
  # removing 2024
  dplyr::select (-`2024`) %>%
  
  # elongaing for bar chart
  pivot_longer (2:3, names_to = "category", values_to = "value") %>%
  
  # renaming category
  mutate (category = ifelse (category == "delta", "Added CClass Headcounts in 2024", "Current CClass Headcounts in 2023")) %>%
  
  # creating shares
  dplyr::group_by (category) %>%
  dplyr::mutate (share = value / sum (value, na.rm = T)) %>% 
  
  # adding labels
  mutate (label = paste0 (round (value / 10^6, 0), " ", "M", "\n", "(", scales::percent(share, 1), ")")) %>%
  
  #changing order
  mutate (category = factor (category, levels = c("Current CClass Headcounts in 2023", "Added CClass Headcounts in 2024"))) %>%
  
  # creating chart
  ggplot (aes (x = age_group, y = share, fill = category)) + 
  geom_bar (stat = "identity", position = "dodge") +
  geom_label (aes (label = label, y = share), 
              position = position_dodge (width = 0.9),
              show.legend = F) + 
  worlddataverse::theme_wdl() + 
  theme (legend.position = "bottom",
         axis.text.x = element_text (angle = 360),
         axis.title.y = element_blank(), 
         axis.text.y = element_blank(),
         axis.ticks.y = element_blank()) + 
  labs (x = "\nAge Group",
        y = "CClass Headcounts",
        title = "Current vs Added CClass Headcounts: \nWhich age group is adding the most in 2024?") 
  
slide_23

## a version with spending
slide_23 = wdp2 %>%
  
  # country-year spending and spending per spending group and age
  dplyr::group_by (year, daily_spending, age_group) %>%
  dplyr::summarise (exp = sum (exp.pdf.m, na.rm = T)) %>% ungroup () %>%
  
  # isolating consumer class
  filter (!daily_spending == "[0,12)") %>%
  dplyr::group_by (year, age_group) %>%
  dplyr::summarise (exp = sum (exp, na.rm = T)) %>% ungroup () %>%
  
  # 10-yar age groups
  mutate (age_group = ifelse (age_group %in% c("[00,05)", "[05,10)"), "[00,10)",
                              ifelse (age_group %in% c("[10,15)", "[15,20)"), "[10,20)",
                                      ifelse (age_group %in% c("[20,25)", "[25,30)"), "[20,30)",
                                              ifelse (age_group %in% c("[30,35)", "[35,40)"), "[30,40)",
                                                      ifelse (age_group %in% c("[40,45)", "[45,50)"), "[40,50)",
                                                              ifelse (age_group %in% c("[50,55)", "[55,60)"), "[50,60)",
                                                                      ifelse (age_group %in% c("[60,65)", "[65,70)"), "[60,70)",
                                                                              ifelse (age_group %in% c("[70,75)", "[75,INF)"), "[70,INF)", NA
                                                                              ))))))))) %>%
  
  # aggregating into new age groups
  dplyr::group_by (year, age_group) %>%
  dplyr::summarise (exp = sum (exp, na.rm = T)) %>% ungroup () %>%
  
  # filtering year
  filter (year %in% 2023:2024) %>%
  
  # widening
  pivot_wider (names_from = "year", values_from = "exp") %>%
  
  # computing delta
  mutate (delta = `2024` - `2023`) %>%
  
  # removing 2024
  dplyr::select (-`2024`) %>%
  
  # elongaing for bar chart
  pivot_longer (2:3, names_to = "category", values_to = "value") %>%
  
  # renaming category
  mutate (category = ifelse (category == "delta", "Added CClass Spending in 2024", "Current CClass Spending in 2023")) %>%
  
  # creating shares
  dplyr::group_by (category) %>%
  dplyr::mutate (share = value / sum (value, na.rm = T)) %>% 
  
  # adding labels
  mutate (label = scales::percent(share, 1)) %>%
  
  #changing order
  mutate (category = factor (category, levels = c("Current CClass Spending in 2023", "Added CClass Spending in 2024"))) %>%
  
  # creating chart
  ggplot (aes (x = age_group, y = share, fill = category)) + 
  geom_bar (stat = "identity", position = "dodge") +
  geom_label (aes (label = label, y = share), 
              position = position_dodge (width = 0.9),
              show.legend = F) + 
  worlddataverse::theme_wdl() + 
  theme (legend.position = "bottom",
         axis.text.x = element_text (angle = 360),
         axis.title.y = element_blank(), 
         axis.text.y = element_blank(),
         axis.ticks.y = element_blank()) + 
  labs (x = "\nAge Group",
        y = "CClass Spending",
        title = "Current vs Added CClass Spending: \nWhich age group is adding the most in 2024?") 

slide_23
  

## Release Note ----------------------------------------------------------------

options (scipen = 99)

releasenote = wdp %>% ungroup () %>%
  
  # country-year spending and spending per spending group and age
  dplyr::group_by (ccode, year, daily_spending) %>%
  dplyr::summarise (pop = sum (hc.pdf.m, na.rm = T),
                    exp = sum (exp.pdf.m, na.rm = T)) %>% ungroup () %>%
  
  # filter for relevant years
  filter (year %in% 2022:2024) %>%
  
  # recoding spendng groups into CC and not
  dplyr::mutate (daily_spending = ifelse (daily_spending == "[12,Inf)", "CC", "NCC")) %>%
  
  # aggregate 
  dplyr::group_by (ccode, year, daily_spending) %>%
  dplyr::summarise (pop = sum (pop, na.rm = T),
                    exp = sum (exp, na.rm = T)) %>% dplyr::ungroup() %>%
  
  # keeping only consumer class
  dplyr::filter (daily_spending == "CC") %>%
  dplyr::select (-daily_spending) %>%
  
  # computing continent var
  mutate (continent = countrycode (ccode, "iso3c", "continent")) %>%
  
  # combining asia and pacific
  mutate (continent = ifelse (continent %in% c("Asia", "Oceania"), "Asia Pacific", continent)) %>%
  
  # changing the scales
  mutate (pop = round (pop / 10^6, 1), 
          exp = round (exp / 10^9, 1)) %>%
  
  # computing delta
  dplyr::group_by (ccode) %>%
  mutate (pop_lag = lag (pop)) %>%
  mutate (pop_delta = pop - pop_lag) %>% ungroup () %>%
  
  # changing to country names
  mutate (ccode = countrycode (ccode, "iso3c", "country.name"))

write.csv (releasenote, "releasenote.csv", row.names = F)
  


