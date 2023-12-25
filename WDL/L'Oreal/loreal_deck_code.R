
## L'Oreal Beauty Deck code ====================================================

# 

## Chapter 1: preliminaries ----------------------------------------------------

## clean environment
rm (list = ls())

## packages
pacman::p_load (worlddataverse, tidyverse, countrycode, haven, gtools, readxl, treemapify)
library (Hmisc)

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

## loading in personal care data
personal_care_outputs2023_06_28 <- read_excel ("personal_care_outputs2023-06-28.xlsx", sheet = 3)



## Chapter 2: Personal care scatter plot ---------------------------------------

## creating chart with Total Expenditure
options (scipen = 99)
scatter_totalexp = ggplot (data = personal_care_outputs2023_06_28 %>%
                             mutate (`Actual Total Personal Care` = `Actual Total Personal Care` / 10^9,
                                     `Predicted Total Personal Care` = `Predicted Total Personal Care` / 10^9,
                                     continent = countrycode (`Country Code`, "iso3c", "continent")) %>%
                             drop_na (), 
                           aes (x = `Actual Total Personal Care`, 
                                y = `Predicted Total Personal Care`,
                                colour = continent)) + 
  geom_jitter () + 
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
  scale_y_continuous (trans = "log10") + 
  scale_x_continuous (trans = "log10") + 
  labs (title = "Personal Care Expenditure in 2017 USD PPP",
        x = 'Survey Expenditure (Total)', 
        y = "Estimated WDL Expenditure (Total)") + 
  worlddataverse::theme_wdl() + 
  worlddataverse::scale_color_wdl() 
  
scatter_totalexp



## Chapter 3: diff sources on beauty market size -------------------------------

## creating raw df in Excel and import
beauty_sources <- read_csv("beauty_sources.csv") %>%
  
  # add year column
  mutate (year = 2023) %>%
  relocate (year, .before = "source") %>%
  
  # arrange
  arrange (desc (value_billions_2023)) %>%
  
  # changing loreal to WDL
  mutate (source = ifelse (source == "L'Oreal", "WDL", source))

## creating plot
ggplot (data = beauty_sources, 
        aes (y = year,
             x = value_billions_2023,
             colour = source)) + 
  geom_point () + 
  theme_void () + 
  worlddataverse::scale_color_wdl() + 
  theme (legend.position = "bottom")



## Chapter 4: tile maps of 2030 growth: top countries --------------------------

## building data 
MapSUBbeauty2330 <- SUBCatbeauty %>%
  filter(Year %in% c(2023, 2030)) %>%
  filter (`Expenditure Type` == "nominal") %>%
  select(Country, Region, Year, `Product Category`, `Product Expenditure`) %>%
  pivot_wider(names_from = "Product Category", values_from = "Product Expenditure") %>%
  rename("Hair.Products" = "Hair Products",
         "Skin.Care" = "Skin Care",
         "Make.Up" = "Make Up") %>%
  group_by(Country, Year) %>%
  summarise(
    frag = sum(as.numeric(as.character(Fragrances)), na.rm = TRUE),
    hair = sum(as.numeric(as.character(Hair.Products)), na.rm = TRUE),
    makeup = sum(as.numeric(as.character(Make.Up)), na.rm = TRUE),
    skin = sum(as.numeric(as.character(Skin.Care)), na.rm = TRUE)) %>%
  reframe(
    frag2330 = frag[Year == 2030] - frag[Year == 2023],
    hair2330 = hair[Year == 2030] - hair[Year == 2023],
    makeup2330 = makeup[Year == 2030] - makeup[Year == 2023],
    skin2330 = skin[Year == 2030] - skin[Year == 2023]
  ) %>%
  pivot_longer(2:5, names_to = "category", values_to = "value") %>%
  arrange (desc (value)) %>%
  slice_head (n = 22) %>%
  
  # creating plot
  ggplot (aes (area = value, 
               fill = Country, 
               label = paste(category, round(value/10^9, digits = 0), "bn", sep = "\n"))) + 
  treemapify::geom_treemap (layout="squarified") +
  geom_treemap_text(place = "centre",size = 10) +
  labs(title="") +
  theme_wdl() +
  scale_fill_wdl() +
  worlddataverse::font_wdl()
MapSUBbeauty2330


## Chapter 5: adding CAGRs everywhere ------------------------------------------

## writing CAGR fundtion

CAGR_fun <- function(x) {
  if (length(x) < 1L)
    return(numeric())
  out <- (x / x[[1L]]) ^ (1 / (seq_along(x) - 1)) - 1
  out[[1L]] <- NA_real_
  out
}

## Beauty addressable market development
beautylarge <- allbeauty %>%
  filter(Year %in% c(2023, 2025, 2030))

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
  dplyr::mutate (CAGR = CAGR (`Beauty Expenditure Total Nominal`)) %>%
  dplyr::mutate (CAGR = scales::percent (CAGR, 1))

# chart plot
ggplot (data = filtered_data_wCAGRs, aes(x = reorder(Country, -`Beauty Expenditure Total Nominal`))) +
  geom_bar (aes(y = `Beauty Expenditure Total Nominal`, fill = factor(Year)), stat = "identity", position = "dodge") +
  labs(title = "", x = "Country", fill = "Year") +
  #geom_text () + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(
    name = "Beauty Expenditure Total (in billion USD)",
    breaks = c(0, 50, 100, 130)) +
  worlddataverse::scale_fill_wdl2() +
  worlddataverse::font_wdl() +
  worlddataverse::theme_wdl()

ggplot(data = filtered_data_wCAGRs, aes(x = reorder(Country, -`Beauty Expenditure Total Nominal`),
                                        y = `Beauty Expenditure Total Nominal`,
                                        fill = factor(Year))) +
  geom_bar (stat = "identity", position = "dodge") +
  geom_text (aes(y = `Beauty Expenditure Total Nominal`, label = CAGR), position = position_dodge (width = .9), vjust = -0.5) + 
  labs(title = "", x = "Country", fill = "Year") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(
    name = "Beauty Expenditure Total (in billion USD)",
    breaks = c(0, 50, 100, 130)) +
  worlddataverse::scale_fill_wdl2() +
  worlddataverse::font_wdl() +
  worlddataverse::theme_wdl()


## Haircare addressable market development
Haircare <- SUBCatbeauty %>%
  filter(Year %in% c(2023, 2025, 2030)) %>%
  filter(`Product Category` == "Hair Products") %>%
  select(Country, Year, `Product Category`, `Product Expenditure`) %>%
  group_by(Country, Year, `Product Category`) %>%
  summarise(haircareexp = sum(`Product Expenditure`)) %>%
  ungroup()

# getting main countries -- this also makes sure it's not going to be messed up due to having 3 rows per country
Haircare30 <- Haircare %>%
  filter(Year == 2030) %>%
  arrange(desc(haircareexp)) %>%
  slice(1:12)

# converting hair care exp into bn
Haircare <- Haircare %>%
  mutate(haircareexp = haircareexp/10^9)

# filtering for 12 main countries -- this makes sure it's not going to be messed up due to having 3 rows per country
filtered_data <- Haircare %>%
  filter(Country %in% Haircare30$Country)

# adding CAGRs
filtered_data_wCAGRs <- filtered_data %>% 
  dplyr::group_by (Country) %>%
  dplyr::mutate (CAGR = CAGR (haircareexp)) %>%
  dplyr::mutate (CAGR = scales::percent (CAGR, 1))

# chart plot
ggplot(filtered_data_wCAGRs, aes(x = reorder(Country, -haircareexp),
                          y = haircareexp, 
                          fill = factor(Year))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text (aes(y = haircareexp, label = CAGR), position = position_dodge (width = .9), vjust = -0.5) + 
  labs(title = "", x = "Country", fill = "Year") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(
    name = "Hair Care Expenditure Total (in billion USD)") +
  worlddataverse::scale_fill_wdl2() +
  worlddataverse::font_wdl() +
  worlddataverse::theme_wdl()


## Skincare addressable market development
Skincare <- SUBCatbeauty %>%
  filter(Year %in% c(2023, 2025, 2030)) %>%
  filter(`Product Category` == "Skin Care") %>%
  select(Country, Year, `Product Category`, `Product Expenditure`) %>%
  group_by(Country, Year, `Product Category`) %>%
  summarise(skincareexp = sum(`Product Expenditure`)) %>%
  ungroup()

# getting main countries -- this also makes sure it's not going to be messed up due to having 3 rows per country
Skincare30 <- Skincare %>%
  filter(Year == 2030) %>%
  arrange(desc(skincareexp)) %>%
  slice(1:12)

# converting hair care exp into bn
Skincare <- Skincare %>%
  mutate(skincareexp = skincareexp/10^9)

# filtering for 12 main countries -- this makes sure it's not going to be messed up due to having 3 rows per country
filtered_data <- Skincare %>%
  filter(Country %in% Skincare30$Country)

filtered_data_wCAGRs <- filtered_data %>% 
  dplyr::group_by (Country) %>%
  dplyr::mutate (CAGR = CAGR (skincareexp)) %>%
  dplyr::mutate (CAGR = scales::percent (CAGR, 1))

# chart plot
ggplot(filtered_data_wCAGRs, aes(x = reorder(Country, -skincareexp),
                           y = skincareexp, 
                           fill = factor(Year))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text (aes(y = skincareexp, label = CAGR), position = position_dodge (width = .9), vjust = -0.5) + 
  labs(title = "", x = "Country", fill = "Year") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(
    name = "Skin Care Expenditure Total (in billion USD)") +
  worlddataverse::scale_fill_wdl2() +
  worlddataverse::font_wdl() +
  worlddataverse::theme_wdl()


## Fragrances addressable market development
Fragrances <- SUBCatbeauty %>%
  filter(Year %in% c(2023, 2025, 2030)) %>%
  filter(`Product Category` == "Fragrances") %>%
  select(Country, Year, `Product Category`, `Product Expenditure`) %>%
  group_by(Country, Year, `Product Category`) %>%
  summarise(fragexp = sum(`Product Expenditure`)) %>%
  ungroup()

# getting main countries -- this also makes sure it's not going to be messed up due to having 3 rows per country
Fragrances30 <- Fragrances %>%
  filter(Year == 2030) %>%
  arrange(desc(fragexp)) %>%
  slice(1:12)

# converting hair care exp into bn
Fragrances <- Fragrances %>%
  mutate(fragexp = fragexp/10^9)

# filtering for 12 main countries -- this makes sure it's not going to be messed up due to having 3 rows per country
filtered_data2 <- Fragrances %>%
  filter(Country %in% Fragrances30$Country)

# adding CAGRs
filtered_data_wCAGRs <- filtered_data2 %>% 
  dplyr::group_by (Country) %>%
  dplyr::mutate (CAGR = CAGR (fragexp)) %>%
  dplyr::mutate (CAGR = scales::percent (CAGR, 1))

# chart plot
ggplot(filtered_data_wCAGRs, aes(x = reorder(Country, -fragexp),
                           y = fragexp, 
                           fill = factor(Year))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text (aes(y = fragexp, label = CAGR), position = position_dodge (width = .9), vjust = -0.5) + 
  labs(title = "", x = "Country", fill = "Year") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(
    name = "Fragrances Expenditure Total (in billion USD)") +
  worlddataverse::scale_fill_wdl2() +
  worlddataverse::font_wdl() +
  worlddataverse::theme_wdl()


## Makeup addressable market development
MakeUp <- SUBCatbeauty %>%
  filter(Year %in% c(2023, 2025, 2030)) %>%
  filter(`Product Category` == "Make Up") %>%
  select(Country, Year, `Product Category`, `Product Expenditure`) %>%
  group_by(Country, Year, `Product Category`) %>%
  summarise(makeupexp = sum(`Product Expenditure`)) %>%
  ungroup()

# getting main countries -- this also makes sure it's not going to be messed up due to having 3 rows per country
MakeUp30 <- MakeUp %>%
  filter(Year == 2030) %>%
  arrange(desc(makeupexp)) %>%
  slice(1:12)

# converting hair care exp into bn
MakeUp <- MakeUp %>%
  mutate(makeupexp = makeupexp/10^9)

# filtering for 12 main countries -- this makes sure it's not going to be messed up due to having 3 rows per country
filtered_data3 <- MakeUp %>%
  filter(Country %in% MakeUp30$Country)

# adding CAGRs
filtered_data_wCAGRs <- filtered_data3 %>% 
  dplyr::group_by (Country) %>%
  dplyr::mutate (CAGR = CAGR (makeupexp)) %>%
  dplyr::mutate (CAGR = scales::percent (CAGR, 1))

# chart plot
ggplot(filtered_data_wCAGRs, aes(x = reorder(Country, -makeupexp),
                                 y = makeupexp, 
                                 fill = factor(Year))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text (aes(y = makeupexp, label = CAGR), position = position_dodge (width = .9), vjust = -0.5) + 
  labs(title = "", x = "Country", fill = "Year") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(
    name = "MakeUp Expenditure Total (in billion USD)") +
  worlddataverse::scale_fill_wdl2() +
  worlddataverse::font_wdl() +
  worlddataverse::theme_wdl()


## Chapter 6: CAGRs ------------------------------------------------------------

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
  
## building data for categories
CAGR_subcat = SUBCatbeauty %>%
  
  # isolating relevant variables
  dplyr::select (Country, Year, `Product Category`, )




  
 




















 