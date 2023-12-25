
# INSIGHT MINING Kathi's data ==================================================

## prelims ---------------------------------------------------------------------

## packages
pacman::p_load (worlddataverse, 
                tidyverse,
                countrycode, 
                data.table, 
                qs, 
                worlddataverse)

## file path
base_path <- worlddataverse::get_wdl_path()
data_path <- file.path (base_path, 'Merge', 'claudio_2023')

## system font 
worlddataverse::font_wdl()

## loading data in
data = qread (file.path (data_path, "i2d2_wdp_urban_rural_age_gender.qs"))

## Viewing a sample
head (data)

## data cleaning ---------------------------------------------------------------

## first clean
data_clean = data %>%
  
  # adding a spending category variable
  mutate (daily_spending = paste0 ("[", inc_lwr, ",", inc_upr, ")")) %>%
  
  # removing inc_lrw and inc_upr
  dplyr::select (-c(inc_lwr, inc_upr))

## subsetting into rural and urban
data_clean_urban = data_clean %>% filter (urb == "urban")
data_clean_rural = data_clean %>% filter (urb == "rural")

## Total population analysis ---------------------------------------------------

data_clean_urban_yeartotalpop = data_clean_urban %>%
  
  dplyr::group_by (year) %>%
  dplyr::summarise (pop = sum (hc, na.rm = T))


## consumer class population analysis ------------------------------------------

data_clean_urban_yearccpop = data_clean_urban %>%
  
  dplyr::group_by (year, daily_spending) %>%
  dplyr::summarise (pop = sum (hc, na.rm = T)) %>%
  mutate (daily_spending = ifelse (daily_spending %in% c("[0,1)","[1,2)","[2,2.15)",
                                                         "[2.15,3)","[3,3.65)",
                                                         "[3.65,4)","[4,5)",
                                                         "[5,6)","[6,6.85)",
                                                         "[6.85,7)","[7,8)",
                                                         "[8,9)","[9,10)",
                                                         "[10,11)","[11,12)"), "notCC", "CC")) %>%
  
  dplyr::group_by (year, daily_spending) %>%
  dplyr::summarise (pop = sum (pop, na.rm = T)) %>%
  filter (!daily_spending == "notCC")

data_clean_urban_yearccspend = data_clean_urban %>%
  
  dplyr::group_by (year, daily_spending) %>%
  dplyr::summarise (exp = sum (exp, na.rm = T)) %>%
  mutate (daily_spending = ifelse (daily_spending %in% c("[0,1)","[1,2)","[2,2.15)",
                                                         "[2.15,3)","[3,3.65)",
                                                         "[3.65,4)","[4,5)",
                                                         "[5,6)","[6,6.85)",
                                                         "[6.85,7)","[7,8)",
                                                         "[8,9)","[9,10)",
                                                         "[10,11)","[11,12)"), "notCC", "CC")) %>%
  
  dplyr::group_by (year, daily_spending) %>%
  dplyr::summarise (exp = sum (exp, na.rm = T)) %>%
  filter (!daily_spending == "notCC")

CAGR_fun <- function (x) {
  if (length (x) < 1L)
    return (numeric ())
  out <- (x / x [[1L]]) ^ (1 / (seq_along (x) - 1)) - 1
  out [[1L]] <- NA_real_
  out
}

## combined analysis
data_clean_urban_popcombined = data_clean_urban_yeartotalpop %>%
  left_join (data_clean_urban_yearccpop, by = "year", suffix = c("_total", "_cc")) %>%
  mutate (pop_total = pop_total / 10^9, 
          pop_cc = pop_cc / 10^9) %>%
  ggplot (aes (x = year)) + 
  geom_line (aes (y = pop_total), col = "#6372fa", linewidth = 1) + 
  geom_line (aes (y = pop_cc), col = "#fea148", linewidth = 1) + 
  theme_wdl () + 
  theme (axis.text.x = element_text (angle = 360)) + 
  labs (y = "Headcounts, in Billions\n", 
        x = "\nYear")

data_clean_urban_popcombined


data_clean_urban_popcombined_dat = data_clean_urban_yeartotalpop %>%
  left_join (data_clean_urban_yearccpop, by = "year", suffix = c("_total", "_cc")) %>%
  mutate (pop_total = pop_total / 10^9, 
          pop_cc = pop_cc / 10^9) %>%
  filter (year >= 2023) %>%
  mutate (CAGR_total = CAGR_fun(pop_total),
          CAGR_cc = CAGR_fun(pop_cc))

## country-specific

data_clean_urban_yeartotalpop_bycountry = data_clean_urban %>%
  
  dplyr::group_by (year, ccode) %>%
  dplyr::summarise (pop = sum (hc, na.rm = T))

data_clean_urban_yearccpop_bycountry = data_clean_urban %>%
  
  dplyr::group_by (year, ccode, daily_spending) %>%
  dplyr::summarise (pop = sum (hc, na.rm = T)) %>%
  mutate (daily_spending = ifelse (daily_spending %in% c("[0,1)","[1,2)","[2,2.15)",
                                                         "[2.15,3)","[3,3.65)",
                                                         "[3.65,4)","[4,5)",
                                                         "[5,6)","[6,6.85)",
                                                         "[6.85,7)","[7,8)",
                                                         "[8,9)","[9,10)",
                                                         "[10,11)","[11,12)"), "notCC", "CC")) %>%
  
  dplyr::group_by (year, ccode, daily_spending) %>%
  dplyr::summarise (pop = sum (pop, na.rm = T)) %>%
  filter (!daily_spending == "notCC")

data_clean_urban_popcombined_bycountry = data_clean_urban_yeartotalpop_bycountry %>%
  left_join (data_clean_urban_yearccpop_bycountry, by = c("year", "ccode"), suffix = c("_total", "_cc")) %>%
  filter (year >= 2023) %>%
  mutate (continent = countrycode (ccode, "iso3c", "continent"))

labels_2050 <- data_clean_urban_popcombined_bycountry %>%
  filter(year == 2050) %>%
  distinct(continent, ccode, pop_total, pop_cc)

library(ggrepel)

ggplot (data = data_clean_urban_popcombined_bycountry, 
        aes (x = year, y = pop_total, group = ccode, col = ccode)) + 
  geom_line () + 
  geom_text_repel(data = labels_2050, aes(x = 2050, label = ccode),
                  box.padding = 0.5, point.padding = 0.1) + 
  theme_wdl () + 
  facet_wrap (~continent, scales = "free") + 
  theme (legend.position = "none")

ggplot (data = data_clean_urban_popcombined_bycountry, 
        aes (x = year, y = pop_cc, group = ccode, col = ccode)) + 
  geom_line () + 
  geom_text_repel(data = labels_2050, aes(x = 2050, label = ccode),
                  box.padding = 0.5, point.padding = 0.1) + 
  theme_wdl () + 
  facet_wrap (~continent, scales = "free") + 
  theme (legend.position = "none")

delta_dat1 = data_clean_urban_popcombined_bycountry %>%
  filter (year %in% c(2023, 2024, 2030, 2050)) %>%
  pivot_wider (names_from = "year", values_from = c("pop_total", "pop_cc")) %>%
  dplyr::select (-daily_spending) %>%
  mutate (tot_delta2324 = pop_total_2024 - pop_total_2023,
          tot_delta2430 = pop_total_2030 - pop_total_2024,
          tot_delta3050 = pop_total_2050 - pop_total_2030,
          cc_delta2324 = pop_cc_2024 - pop_cc_2023,
          cc_delta2430 = pop_cc_2030 - pop_cc_2024,
          cc_delta3050 = pop_cc_2050 - pop_cc_2030) %>%
  dplyr::select (ccode, tot_delta2324, tot_delta2430, tot_delta3050, cc_delta2324, cc_delta2430, cc_delta3050)

ggplot (data = delta_dat1 %>% 
          arrange (desc(tot_delta2324)) %>%
          slice_head (n = 10),
        aes (x = reorder (ccode, -tot_delta2324), y = tot_delta2324, fill = ccode)) +
  geom_bar (stat = "identity") + 
  theme_wdl() + 
  theme (axis.text.x = element_text (angle = 360)) + 
  theme (legend.position = "none") + 
  labs (x = "\nCountry",
        y = "Absolute delta 2023 -> 2024\n")

ggplot (data = delta_dat1 %>% 
          arrange (tot_delta2324) %>%
          slice_head (n = 10),
        aes (x = reorder (ccode, tot_delta2324), y = tot_delta2324, fill = ccode)) +
  geom_bar (stat = "identity") + 
  theme_wdl() + 
  theme (axis.text.x = element_text (angle = 360)) + 
  theme (legend.position = "none") + 
  labs (x = "\nCountry",
        y = "Absolute delta 2023 -> 2024\n")

ggplot (data = delta_dat1 %>% 
          arrange (desc(tot_delta2430)) %>%
          slice_head (n = 10),
        aes (x = reorder (ccode, -tot_delta2430), y = tot_delta2430, fill = ccode)) +
  geom_bar (stat = "identity") + 
  theme_wdl() + 
  theme (axis.text.x = element_text (angle = 360)) + 
  theme (legend.position = "none") + 
  labs (x = "\nCountry",
        y = "Absolute delta 2024 -> 2030\n")

ggplot (data = delta_dat1 %>% 
          arrange (tot_delta2430) %>%
          slice_head (n = 10),
        aes (x = reorder (ccode, tot_delta2430), y = tot_delta2430, fill = ccode)) +
  geom_bar (stat = "identity") + 
  theme_wdl() + 
  theme (axis.text.x = element_text (angle = 360)) + 
  theme (legend.position = "none") + 
  labs (x = "\nCountry",
        y = "Absolute delta 2024 -> 2030\n")

ggplot (data = delta_dat1 %>% 
          arrange (desc(tot_delta3050)) %>%
          slice_head (n = 10),
        aes (x = reorder (ccode, -tot_delta3050), y = tot_delta3050, fill = ccode)) +
  geom_bar (stat = "identity") + 
  theme_wdl() + 
  theme (axis.text.x = element_text (angle = 360)) + 
  theme (legend.position = "none") + 
  labs (x = "\nCountry",
        y = "Absolute delta 2030 -> 2050\n")

ggplot (data = delta_dat1 %>% 
          arrange (tot_delta3050) %>%
          slice_head (n = 10),
        aes (x = reorder (ccode, tot_delta3050), y = tot_delta3050, fill = ccode)) +
  geom_bar (stat = "identity") + 
  theme_wdl() + 
  theme (axis.text.x = element_text (angle = 360)) + 
  theme (legend.position = "none") + 
  labs (x = "\nCountry",
        y = "Absolute delta 2030 -> 2050\n")




ggplot (data = delta_dat1 %>% 
          arrange (desc(cc_delta2324)) %>%
          slice_head (n = 10),
        aes (x = reorder (ccode, -cc_delta2324), y = cc_delta2324, fill = ccode)) +
  geom_bar (stat = "identity") + 
  theme_wdl() + 
  theme (axis.text.x = element_text (angle = 360)) + 
  theme (legend.position = "none") + 
  labs (x = "\nCountry",
        y = "Absolute delta 2023 -> 2024\n")

ggplot (data = delta_dat1 %>% 
          filter (!ccode == "AFG") %>%
          arrange (cc_delta2324) %>%
          slice_head (n = 10),
        aes (x = reorder (ccode, cc_delta2324), y = cc_delta2324, fill = ccode)) +
  geom_bar (stat = "identity") + 
  theme_wdl() + 
  theme (axis.text.x = element_text (angle = 360)) + 
  theme (legend.position = "none") + 
  labs (x = "\nCountry",
        y = "Absolute delta 2023 -> 2024\n")

ggplot (data = delta_dat1 %>% 
          arrange (desc(cc_delta2430)) %>%
          slice_head (n = 10),
        aes (x = reorder (ccode, -cc_delta2430), y = cc_delta2430, fill = ccode)) +
  geom_bar (stat = "identity") + 
  theme_wdl() + 
  theme (axis.text.x = element_text (angle = 360)) + 
  theme (legend.position = "none") + 
  labs (x = "\nCountry",
        y = "Absolute delta 2024 -> 2030\n")

ggplot (data = delta_dat1 %>%  
          filter (!ccode == "AFG") %>%
          arrange (cc_delta2430) %>%
          slice_head (n = 10),
        aes (x = reorder (ccode, cc_delta2430), y = cc_delta2430, fill = ccode)) +
  geom_bar (stat = "identity") + 
  theme_wdl() + 
  theme (axis.text.x = element_text (angle = 360)) + 
  theme (legend.position = "none") + 
  labs (x = "\nCountry",
        y = "Absolute delta 2024 -> 2030\n")

ggplot (data = delta_dat1 %>% 
          arrange (desc(cc_delta3050)) %>%
          slice_head (n = 10),
        aes (x = reorder (ccode, -cc_delta3050), y = cc_delta3050, fill = ccode)) +
  geom_bar (stat = "identity") + 
  theme_wdl() + 
  theme (axis.text.x = element_text (angle = 360)) + 
  theme (legend.position = "none") + 
  labs (x = "\nCountry",
        y = "Absolute delta 2030 -> 2050\n")

ggplot (data = delta_dat1 %>%  
          filter (!ccode == "AFG") %>%
          arrange (cc_delta3050) %>%
          slice_head (n = 10),
        aes (x = reorder (ccode, cc_delta3050), y = cc_delta3050, fill = ccode)) +
  geom_bar (stat = "identity") + 
  theme_wdl() + 
  theme (axis.text.x = element_text (angle = 360)) + 
  theme (legend.position = "none") + 
  labs (x = "\nCountry",
        y = "Absolute delta 2030 -> 2050\n")
  
