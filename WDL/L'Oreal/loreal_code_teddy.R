
## L'Oreal Beauty Deck: Teddy's code

## Notes -----------------------------------------------------------------------

# This is the code behind some of the charts in the l'Oreal Beauty deck, presented in July 2023.
# The code below creates the Personal Care scatter plot, growth tile maps, computing CAGRs, 
# obtaining Beauty addressable market and % of spending 2023, disaggregating Total 
# personal care into beauty and others, and creating the subcategories bubble plot.

## Prologue (packages, raw data, file paths) -----------------------------------

## clean environment
rm (list = ls())

## packages
pacman::p_load (worlddataverse, tidyverse, countrycode, haven, gtools, readxl, treemapify, Hmisc)

## system font 
worlddataverse::font_wdl()

## loading paths
base_path <- worlddataverse::get_wdl_path()
wdp_path <- file.path (base_path,'Mpro_2.0')
currentwdp_path <- file.path (wdp_path,'01_Data','R_2023_04_11','2023_04_26_ukr_rebase_2017ppp','03_outputs')
wdp_path1 <- file.path(base_path, 'product_categories', 'trade_model', '03_outputs')
wdp_path2 <- file.path(base_path, 'product_categories', 'loreal', '2023-06-28')

## loading in WDPro latest version
# loading raw file
wdp_raw <- readRDS (file.path (currentwdp_path,"05_003_merge_standard_ages_R_2023_04_11_2017ppp_1_USD.rds"))

## loading in beauty data
allbeauty <- read_excel (file.path(wdp_path1, "beauty_exp_loreal2023-06-28.xlsx"))
beautyregion <- read_excel (file.path(wdp_path1, "beauty_exp_loreal2023-06-28.xlsx"), sheet = 2)
beautytotals <- read_excel (file.path(wdp_path1, "beauty_exp_loreal2023-06-28.xlsx"), sheet = 3)
SUBCatbeauty <- read_excel ( file.path(wdp_path2, "Loreal_Product_Data-2023-06-28.xlsx"))

## loading in personal care data (directly downloaded from Slack, search the file name in the Slack channel)
personal_care_outputs2023_06_28 <- read_excel ("personal_care_outputs2023-06-28.xlsx", sheet = 3)



## Chapter 1: Personal care scatter plot ---------------------------------------

## building plot
scatter_totalexp = ggplot (data = personal_care_outputs2023_06_28 %>%
                             # rescaling into Billions
                             mutate (`Actual Total Personal Care` = `Actual Total Personal Care` / 10^9,
                                     `Predicted Total Personal Care` = `Predicted Total Personal Care` / 10^9,
                                     continent = countrycode (`Country Code`, "iso3c", "continent")) %>%
                             # removing NAs
                             drop_na (), 
                           aes (x = `Actual Total Personal Care`, 
                                y = `Predicted Total Personal Care`,
                                colour = continent)) + 
  
  # baseline scatter plot
  geom_jitter () + 
  
  # adding line of best fit
  geom_smooth (data = personal_care_outputs2023_06_28 %>%
                 mutate (`Actual Total Personal Care` = `Actual Total Personal Care` / 10^9,
                         `Predicted Total Personal Care` = `Predicted Total Personal Care` / 10^9,
                         continent = countrycode (`Country Code`, "iso3c", "continent")) %>%
                 drop_na (),
               inherit.aes = FALSE, 
               aes (x = `Actual Total Personal Care`, 
                    y = `Predicted Total Personal Care`),
               formula = y ~ x, 
               method = "lm") +
  
  # log-transforming axes for better visibility of trend
  scale_y_continuous (trans = "log10") + 
  scale_x_continuous (trans = "log10") + 
  
  # adding titles
  labs (title = "Personal Care Expenditure in 2017 USD PPP",
        x = 'Survey Expenditure (Total)', 
        y = "Estimated WDL Expenditure (Total)") + 
  
  # adding theme and colours
  worlddataverse::theme_wdl() + 
  worlddataverse::scale_color_wdl() 

# viewing plot
scatter_totalexp




## Chapter 2: treemap of 2030 growth: top countries --------------------------

## building plot

# building input data
MapSUBbeauty2330 <- SUBCatbeauty %>%
  
  # filtering for relevant years
  filter (Year %in% c(2023, 2030)) %>%
  
  # keeping only nominal expenditure
  filter (`Expenditure Type` == "nominal") %>%
  
  # keping only relevant variables
  select (Country, Region, Year, `Product Category`, `Product Expenditure`) %>%
  
  # widening expenditure data by product category
  pivot_wider(names_from = "Product Category", values_from = "Product Expenditure") %>%
  rename("Hair.Products" = "Hair Products",
         "Skin.Care" = "Skin Care",
         "Make.Up" = "Make Up") %>%
  
  # obtaining single expenditure by country-year
  dplyr::group_by (Country, Year) %>%
  dplyr::summarise (
    frag = sum (as.numeric (as.character (Fragrances)), na.rm = TRUE),
    hair = sum (as.numeric (as.character (Hair.Products)), na.rm = TRUE),
    makeup = sum (as.numeric (as.character (Make.Up)), na.rm = TRUE),
    skin = sum (as.numeric (as.character (Skin.Care)), na.rm = TRUE)) %>%
  
  # calculating frowth
  reframe (
    frag2330 = frag [Year == 2030] - frag [Year == 2023],
    hair2330 = hair [Year == 2030] - hair [Year == 2023],
    makeup2330 = makeup [Year == 2030] - makeup [Year == 2023],
    skin2330 = skin [Year == 2030] - skin [Year == 2023]
  ) %>%
  
  # elongaing product category data to expenditure by category
  pivot_longer(2:5, names_to = "category", values_to = "value") %>%
  
  # reordering in descending order
  arrange (desc (value)) %>%
  
  # obtaining the top 22 by growth to 2030
  slice_head (n = 22) %>%
  
  # creating plot
  ggplot (aes (area = value, 
               fill = Country, 
               label = paste(category, round(value/10^9, digits = 0), "bn", sep = "\n"))) + 
  
  # creating treemap
  treemapify::geom_treemap (layout = "squarified") +
  
  # adding tile text
  geom_treemap_text(place = "centre",size = 10) +
  
  # creating empty title
  labs(title="") +
  
  # adding WDL theme, colour and font
  theme_wdl() +
  scale_fill_wdl() +
  worlddataverse::font_wdl()

# viewing plot
MapSUBbeauty2330



## Chapter 3: CAGRs and Macro-figures ------------------------------------------

## writing CAGR function
CAGR_fun <- function (x) {
  if (length (x) < 1L)
    return (numeric ())
  out <- (x / x [[1L]]) ^ (1 / (seq_along (x) - 1)) - 1
  out [[1L]] <- NA_real_
  out
}

## building data for regions
CAGR_region = beautyregion %>%
  
  # filter for 2023 - 2030
  filter (Year %in% 2023:2030) %>%
  
  # grouping by region
  dplyr::group_by(`Loreal Region`) %>%
  
  # creating CAGRs
  dplyr::mutate (CAGR = CAGR_fun (`Beauty Expenditure Total`) * 100,
                 CAGR = round (CAGR, 2)) %>%
  
  # filter for year 2030
  filter (Year %in% c(2023, 2030))

## building data for totals
CAGR_total = beautytotals %>%
  
  # filter for 2023 - 2030
  filter (Year %in% 2023:2030) %>%
  
  # # creating CAGRs
  dplyr::mutate (CAGR = CAGR_fun (`Beauty Expenditure Nominal USD`) * 100,
                 CAGR = round (CAGR, 2)) %>%
  
  # filter for year 2030
  filter (Year %in% c(2023, 2030))

## cleaning data (subcategories macro)
subcat_macro = SUBCatbeauty %>%
  
  # expenditure type = nominall
  filter (`Expenditure Type` == "nominal") %>%
  dplyr::select (-`Expenditure Type`) %>%
  
  # obtaining yearly expenditure per category
  dplyr::group_by (Year, `Product Category`) %>%
  dplyr::summarise (exp = sum (`Product Expenditure`, na.rm = T)) %>%
  
  # removing NAs
  drop_na (`Product Category`) %>%
  
  # adding CAGRs
  filter (Year %in% c(2023:2030)) %>%
  dplyr::group_by (`Product Category`) %>%
  mutate (cagr = CAGR_fun (exp)) %>% ungroup () %>%
  
  # filter for 2023 and 2030 
  filter (Year %in% c(2023, 2030)) %>%
  
  # widening for delta calculations
  pivot_wider (names_from = "Year", values_from = 3:4) %>%
  
  # computing delta
  mutate (growth = ((exp_2030 - exp_2023) / exp_2023) * 100) %>%
  
  # converting figs to billons
  mutate (exp_2030 = exp_2030 / 10^9,
          exp_2023 = exp_2023 / 10^9)

# cleaning data (total macro)
beautytotal_macro = beautytotals %>%
  
  # filter for 2023-2030
  filter (Year %in% c(2023:2030)) %>%
  
  # adding CAGRs
  mutate (cagr = CAGR_fun (`Beauty Expenditure Total Nominal`))



## Chapter 4: Beauty addressable market and % of spending 2023 -----------------

## cleaning data
beauty_market = allbeauty %>%
  
  # nominal, beauty share of total spending (%, line), beauty expenditure (bars), 12 countries
  filter (Year == 2023) %>%
  
  # ordering beauty exp. numbers and taking main 12 countries
  arrange (desc (`Beauty Expenditure Total Nominal`)) %>%
  slice (1:12) %>%
  
  # changing the levels of country
  mutate (Country = factor (Country, levels = Country [order (-`Beauty Expenditure Total Nominal`)])) %>%
  
  # scaling Beauty.Expenditure.Total.Nominal to billions
  mutate (`Beauty Expenditure Total Nominal` = `Beauty Expenditure Total Nominal`/10^9, 
          `Beauty Share of Total Expenditure` = `Beauty Share of Total Expenditure` * 100)

## getting the maximum Beauty.Expenditure.Total.Nominal
ymax <- max (beauty_market$`Beauty Expenditure Total Nominal`)

## creating the scaling constant
scaleRight <- max (beauty_market$`Beauty Share of Total Expenditure`) / ymax

## chart code
ggplot(data = beauty_market, aes(x = Country)) +
  
  # creating baseline bar chart
  geom_bar(aes(y = `Beauty Expenditure Total Nominal`, fill = Country), stat = "identity", show.legend = FALSE) +
  
  # adding line overlay
  geom_line(aes(y = `Beauty Share of Total Expenditure` * 40, group = 1), stat = "identity", color = "black", size = 1.5) +
  
  # adding title and axis labels
  labs(title = "Beauty Expenditure in 2023", x = "Country") +
  
  # adding theme and colours
  worlddataverse::theme_wdl() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_wdl() +
  
  # adjusting the two y-axis scales
  scale_y_continuous(
    name = "Beauty Expenditure Total (in billion USD)",
    breaks = c(0, 50, 100),
    sec.axis = sec_axis(
      ~ . / 40, 
      name = "Beauty Share of Total Expenditure", 
      breaks = c(0,0.5,1,1.5, 2,2.5), 
      labels = c(0,0.5,1,1.5, 2,2.5)
    )
  )


## Chapter 5: Beauty addressable market top 12 countries -----------------------

## Beauty addressable market development
beautylarge <- allbeauty %>%
  filter(Year %in% c(2023:2030))

# getting main countries -- this also makes sure it's not going to be messed up due to having 3 rows per country
beautylarge30 <- beautylarge %>%
  filter(Year == 2030) %>%
  arrange (desc (`Beauty Expenditure Total Nominal`)) %>%
  slice(1:12)

# converting beauty exp into bn
beautylarge <- beautylarge %>%
  mutate(`Beauty Expenditure Total Nominal` = `Beauty Expenditure Total Nominal`/10^9)

# filtering for 12 main countries -- this makes sure it's not going to be messed up due to having 3 rows per country
filtered_data <- beautylarge %>%
  filter(Country %in% beautylarge30$Country) 

# adding CAGRs
filtered_data_wCAGRs <- filtered_data %>% 
  dplyr::group_by (Country) %>%
  dplyr::mutate (CAGR = CAGR_fun (`Beauty Expenditure Total Nominal`)) %>%
  dplyr::mutate (CAGR = scales::percent (CAGR, 1)) %>%
  dplyr::mutate (label = round (`Beauty Expenditure Total Nominal`, 1)) %>%
  # keeping only relevant vars
  dplyr::select (Country, Year, `Beauty Expenditure Total Nominal`, CAGR, label) %>%
  filter (Year %in% c(2023, 2025, 2030))

# chart code
ggplot(data = filtered_data_wCAGRs %>% 
         filter (Year %in% c(2023, 2025, 2030)), aes(x = reorder(Country, -`Beauty Expenditure Total Nominal`),
                                                     y = `Beauty Expenditure Total Nominal`,
                                                     fill = factor(Year))) +
  
  # building bar plot
  geom_bar (stat = "identity", position = "dodge") +
  
  # adding label on top
  geom_text (aes(y = `Beauty Expenditure Total Nominal`, label = label), 
             position = position_dodge (width = .9), 
             #vjust = -0.5,
             hjust = -0.1,
             angle = 90) + 
  
  # adding title and axis label and legend label 
  labs(title = "", x = "Country", fill = "Year") +
  
  # theme: adjusting x-axis labels
  theme(axis.text.x = element_text (angle = 45, hjust = 1)) +
  
  # rescaling y axis
  scale_y_continuous(
    name = "Beauty Expenditure Total (in billion USD)",
    breaks = c(0, 50, 100, 130)) +
  
  # WDL theme, colours and font
  worlddataverse::scale_fill_wdl2() +
  worlddataverse::font_wdl() +
  worlddataverse::theme_wdl() + 
  
  # adjusting y axis limits
  ylim (0,175)



## Chapter 6: Bubble chart -----------------------------------------------------






## Chapter 7: Disaggregating total personal care into beauty and others  -------

## building input data 
beauty_and_others_dat = allbeauty %>%
  
  # isolating relevant variables
  dplyr::select (Country, Year, `Personal Care Total Nominal`, `Beauty Expenditure Total Nominal`) %>%
  
  # filtering forr 2023-2030
  filter (Year %in% 2023:2030) %>%
  
  # aggregating into total per year
  dplyr::group_by (Year) %>%
  dplyr::summarise (`Personal Care Total Nominal` = sum (`Personal Care Total Nominal`, na.rm = T),
                    `Beauty Expenditure Total Nominal` = sum (`Beauty Expenditure Total Nominal`, na.rm = T)) %>%
  
  # adding other personal care variable
  mutate (`Other Personal Care Expenditure Total Nominal` = `Personal Care Total Nominal` - `Beauty Expenditure Total Nominal`) %>%
  
  # rescaling
  mutate (`Personal Care Total Nominal` = `Personal Care Total Nominal` / 10^9,
          `Beauty Expenditure Total Nominal` = `Beauty Expenditure Total Nominal` / 10^9,
          `Other Personal Care Expenditure Total Nominal` = `Other Personal Care Expenditure Total Nominal` / 10^9) %>%
  
  # adding CAGRs
  mutate (pc_cagr = CAGR_fun(`Personal Care Total Nominal`),
          b_cagr = CAGR_fun(`Beauty Expenditure Total Nominal`), 
          opc_cagr = CAGR_fun(`Other Personal Care Expenditure Total Nominal`))

# reformatting to allow stacking
dplyr::select (-`Personal Care Total Nominal`) %>%
  pivot_longer (2:3, names_to = "Expenditure Type", values_to = "Expenditure")

## creating chart
ggplot (data = beauty_and_others_dat %>%
          mutate (`Expenditure Type` = factor (`Expenditure Type`, levels = c("Other Personal Care Expenditure Total Nominal", "Beauty Expenditure Total Nominal"))), 
        aes (x = factor (Year), 
             y = Expenditure,
             fill = `Expenditure Type`)) + 
  
  # creating bar chart
  geom_bar (stat = "identity", position = "stack") + 
  
  # adjusting legend position and x-axis label rotation
  theme (legend.position = "bottom",
         axis.text.x = element_text (angle = 360)) + 
  
  # adding WDL theme and colours
  worlddataverse::theme_wdl() + 
  worlddataverse::scale_fill_wdl2()










