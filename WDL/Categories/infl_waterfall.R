
## alternative method for getting inflation for waterfall plot of global spending 2023 -> 2030 by spending group

## real method 

# wdp data
wdp_raw <- readRDS (file.path (currentwdp_path,"05_003_merge_standard_ages_R_2023_04_11_2017ppp_1_USD.rds"))
wdp = wdp_raw %>% worlddataverse::rds_to_wdpro (ages = F, inc = T, inc_vec = c(0, 12, 40, 80, 120, Inf))

## adj df
demographic_breaks <-  file.path (
  base_path,
  "spending_categories",
  "demographic_breakdown"
)

input_path <- file.path (
  demographic_breaks,
  "pc_and_beauty_breakdown",
  "2023-09-07"
)

adj <- readRDS(
  file.path(
    input_path,
    "adjusted_conversion_factors.rds"))

## cleaning PPP data initially into country, year, spending groups and expenditure
wdp_waterfall_ppp = wdp %>%
  
  # aggregating into relevant var groups
  dplyr::group_by (ccode, year, daily_spending) %>%
  dplyr::summarise (exp = sum (exp.pdf.m, na.rm = T))

## converting to real
wdp_waterfall_real = wdp_waterfall_ppp %>%
  
  # adding conversion factors
  left_join (adj %>% 
               filter (year == 2023) %>% 
               dplyr::select (-year), 
             by = "ccode") %>%
  
  # converting exp to nominal
  mutate (exp = exp * hhe_2017ppp_to_current_USD) %>%
  dplyr::select (-hhe_2017ppp_to_current_USD)

## converting to nominal
wdp_waterfall_nominal = wdp_waterfall_ppp %>%
  
  # adding conversion factors
  left_join (adj, by = c("year", "ccode")) %>%
  
  # converting exp to nominal
  mutate (exp = exp * hhe_2017ppp_to_current_USD) %>%
  dplyr::select (-hhe_2017ppp_to_current_USD)

## constructing chart components

# build up
wdp_waterfall = wdp_waterfall_real %>%
  
  # aggregating into year-group-exp
  dplyr::group_by (year, daily_spending) %>%
  dplyr::summarise (exp = sum (exp, na.rm = T)) %>%
  
  # filtering for years of interest
  filter (year %in% c(2023, 2030)) %>%
  
  # calculating spending group deltas
  dplyr::group_by (daily_spending) %>%
  dplyr::summarise (
    exp_delta = sum (exp [year == 2030]) - sum (exp [year == 2023]),
    exp_delta = round (exp_delta/10^12, digits = 2)
  )

# adding 2023 total
wdp_waterfall [nrow (wdp_waterfall) + 1, ] <- 
  list ("Global Spending 2023", 
        round (sum (wdp_waterfall_real$exp [wdp_waterfall_nominal$year == 2023]) / 10^12, 2)
        )

# adding inflation
wdp_waterfall [nrow (wdp_waterfall) + 1, ] <- 
  list ("Inflation",
        round ((sum (wdp_waterfall_nominal$exp [wdp_waterfall_nominal$year == 2030]) - 
           sum (wdp_waterfall_real$exp [wdp_waterfall_real$year == 2030]))/ 10^12, 2)
        )

# ordering
wdp_waterfall <- wdp_waterfall %>%
  arrange (desc (exp_delta))

# adding chart
library (waterfalls)
waterfall(wdp_waterfall,
          calc_total = TRUE, # TRUE: means it will calculate total, which will also be equal to BEAUTY TOTAL 2030
          values = as.numeric (exp_delta),
          total_rect_color = "lightblue",
          total_rect_text_color = "black",
          total_axis_text = "Global spending 2030",
          scale_y_to_waterfall = TRUE,
          rect_width = 1,
          draw_lines = FALSE,
          rect_border = NA) +
  theme_wdl() +
  labs (x = "", 
        y = "Global Expenditure (nominal USD)",
        title = 'Global Expenditure Growth by Spending Group') +
  worlddataverse::font_wdl() + 
  theme (axis.text.x = element_text (angle = 360))





## nominal method --------------------------------------------------------------

# wdp data
wdp_raw <- readRDS (file.path (currentwdp_path,"05_003_merge_standard_ages_R_2023_04_11_2017ppp_1_USD.rds"))
wdp = wdp_raw %>% worlddataverse::rds_to_wdpro (ages = F, inc = T, inc_vec = c(0, 12, 40, 80, 120, Inf))

## adj df
demographic_breaks <-  file.path (
  base_path,
  "spending_categories",
  "demographic_breakdown"
)

input_path <- file.path (
  demographic_breaks,
  "pc_and_beauty_breakdown",
  "2023-09-07"
)

adj <- readRDS(
  file.path(
    input_path,
    "adjusted_conversion_factors.rds"))

## cleaning PPP data initially into country, year, spending groups and expenditure
wdp_waterfall_ppp = wdp %>%
  
  # aggregating into relevant var groups
  dplyr::group_by (ccode, year, daily_spending) %>%
  dplyr::summarise (exp = sum (exp.pdf.m, na.rm = T))

## converting to nominal
wdp_waterfall_nominal = wdp_waterfall_ppp %>%
  
  # adding conversion factors
  left_join (adj, by = c("year", "ccode")) %>%
  
  # converting exp to nominal
  mutate (exp = exp * hhe_2017ppp_to_current_USD) %>%
  dplyr::select (-hhe_2017ppp_to_current_USD)
  
  
## constructing chart components

# 2023 and 2030 bars in nominal
wdp_waterfall_nominal_2330totals = wdp_waterfall_nominal %>%
  
  # aggregating into year-group-exp
  dplyr::group_by (year, daily_spending) %>%
  dplyr::summarise (exp = sum (exp, na.rm = T)) %>%
  
  # filtering for years of interest
  filter (year %in% c(2023, 2030)) %>%
  
  # calculating spending group deltas
  dplyr::group_by (daily_spending) %>%
  dplyr::summarise (
    exp_delta = sum (exp [year == 2030]) - sum (exp [year == 2023]),
    exp_delta = round (exp_delta/10^12, digits = 2)
  )

# adding spending 2023 bar
wdp_waterfall_nominal_2330totals [nrow (wdp_waterfall_nominal_2330totals) + 1, ] <- 
  list ("Global Spending 2023", 
        round (sum (wdp_waterfall_nominal$exp [wdp_waterfall_nominal$year == 2023]) / 10^12, 2))

# calculating % growth in PPP terms 
((sum (wdp_waterfall_ppp$exp [wdp_waterfall_ppp$year == 2030]) - 
    sum (wdp_waterfall_ppp$exp [wdp_waterfall_ppp$year == 2023])) / 
    sum (wdp_waterfall_ppp$exp [wdp_waterfall_ppp$year == 2023])) * 100 # 23.48927 %

# nominal2023 * 23.48927 %
realgrowth <- (sum (wdp_waterfall_nominal$exp [wdp_waterfall_nominal$year == 2023]) * 0.2348927)

# obtaining inflation contribution
inflation <- sum (wdp_waterfall_nominal$exp [wdp_waterfall_nominal$year == 2030]) - realgrowth

# adding inflation contribution to main df
wdp_waterfall_nominal_2330totals [nrow (wdp_waterfall_nominal_2330totals) + 1, ] <- list ("Inflation", round (inflation / 10^12, 2))

# ordering
wdp_waterfall_nominal_2330totals <- wdp_waterfall_nominal_2330totals %>%
  arrange (desc (exp_delta))

# adding chart
library (waterfalls)
waterfall(wdp_waterfall_nominal_2330totals,
          calc_total = TRUE, # TRUE: means it will calculate total, which will also be equal to BEAUTY TOTAL 2030
          values = as.numeric (exp_delta),
          total_rect_color = "lightblue",
          total_rect_text_color = "black",
          total_axis_text = "Global spending 2030",
          scale_y_to_waterfall = TRUE,
          rect_width = 1,
          draw_lines = FALSE,
          rect_border = NA) +
  theme_wdl() +
  labs (x = "", 
        y = "Global Expenditure (nominal USD)",
        title = 'Global Expenditure Growth by Spending Group') +
  worlddataverse::font_wdl() + 
  theme (axis.text.x = element_text (angle = 360))









  
