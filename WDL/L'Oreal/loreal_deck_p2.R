
## V2 Finalising beauty deck ===================================================

## Chapter 1: preliminaries ----------------------------------------------------

## clean environment
rm (list = ls())

## packages
pacman::p_load (worlddataverse, tidyverse, countrycode, haven, gtools, readxl, treemapify, scatterplot3d)

## system font 
worlddataverse::font_wdl()

## loading paths
base_path <- worlddataverse::get_wdl_path()

## data files
allbeauty <- read_excel ("~/Desktop/new_beauty_2023-06-28.xlsx", sheet = 1)
beautyregion <- read_excel ("~/Desktop/new_beauty_2023-06-28.xlsx", sheet = 2)
beautytotals <- read_excel ("~/Desktop/new_beauty_2023-06-28.xlsx", sheet = 3)
SUBCatbeauty <- read_excel ("~/Desktop/SUBCatbeauty_2023-06-28.xlsx")

## CAGR function

CAGR_fun <- function(x) {
  
  if (length(x) < 1L)
    return(numeric())
  out <- (x / x[[1L]]) ^ (1 / (seq_along(x) - 1)) - 1
  out[[1L]] <- NA_real_
  out
  
}

## Chapter 2: Regional CAGRs for 2030 Beauty Expenditure -----------------------

## regional
CAGR_region = beautyregion %>%
  
  # filter for 2023 - 2030
  filter (Year %in% 2023:2030) %>%
  
  # grouping by region
  dplyr::group_by(`Loreal Region`) %>%
  
  # creating CAGRs
  dplyr::mutate (CAGR = CAGR_fun (`Beauty Expenditure Total Nominal`) * 100,
                 CAGR = round (CAGR, 2)) %>%
  
  # filter for year 2030
  filter (Year %in% c(2023, 2030))

## total
CAGR_total = beautytotals %>%
  
  # filter for 2023 - 2030
  filter (Year %in% 2023:2030) %>%
  
  # # creating CAGRs
  dplyr::mutate (CAGR = CAGR_fun (`Beauty Expenditure Total Nominal`) * 100,
                 CAGR = round (CAGR, 2)) %>%
  
  # filter for year 2030
  filter (Year %in% c(2023, 2030))
  

## Chapter 3: Beauty addressable market and % of spending 2023 -----------------

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
ggplot (data = beauty_market, 
        aes (x = Country)) +
  
  # creating bars
  geom_bar (aes (y = `Beauty Expenditure Total Nominal`, fill = Country), stat = "identity", show.legend = FALSE) + 

  # creating line
  geom_line (aes (y = `Beauty Share of Total Expenditure`*100, group = 1), stat = "identity", color = "black", size = 1.5) + 
  
  # title 
  labs (title = "Beauty Expenditure in 2023", 
        x = "Country") +
  
  # theme 
  worlddataverse::theme_wdl () + 
  theme (axis.text.x = element_text (angle = 45, hjust = 1)) +
  scale_fill_wdl() +
  
  # scaling
  scale_y_continuous(
    name = "Beauty Expenditure Total (in billion USD)",
    breaks = c (0, 50, 100),
    sec.axis = sec_axis (~ . / 100, name = "% of Spending", breaks = c(0, 5), labels = c(0, 5)))


ggplot(data = beauty_market, aes(x = Country)) +
  geom_bar(aes(y = `Beauty Expenditure Total Nominal`, fill = Country), stat = "identity", show.legend = FALSE) +
  geom_line(aes(y = `Beauty Share of Total Expenditure` * 40, group = 1), stat = "identity", color = "black", size = 1.5) +
  labs(title = "Beauty Expenditure in 2023", x = "Country") +
  worlddataverse::theme_wdl() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_wdl() +
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


## Chapter 3: Beauty addressable market development - Top 12 Countries

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
  geom_bar (stat = "identity", position = "dodge") +
  geom_text (aes(y = `Beauty Expenditure Total Nominal`, label = label), 
             position = position_dodge (width = .9), 
             #vjust = -0.5,
             hjust = -0.1,
             angle = 90) + 
  labs(title = "", x = "Country", fill = "Year") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(
    name = "Beauty Expenditure Total (in billion USD)",
    breaks = c(0, 50, 100, 130)) +
  worlddataverse::scale_fill_wdl2() +
  worlddataverse::font_wdl() +
  worlddataverse::theme_wdl() + 
  ylim (0,175)


## Chapter 4: Macro-figures subcat ---------------------------------------------

## cleaning data
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

beautytotal_macro = beautytotals %>%
  
  # filter for 2023-2030
  filter (Year %in% c(2023:2030)) %>%
  
  # adding CAGRs
  mutate (cagr = CAGR_fun (`Beauty Expenditure Total Nominal`))


## Chapter 5: 3D slide ---------------------------------------------------------

## prepping data
plot_3d_dat = SUBCatbeauty %>%
  
  # expenditure type = nominall
  filter (`Expenditure Type` == "nominal") %>%
  dplyr::select (-`Expenditure Type`) %>%
  
  # obtaining yearly expenditure per category and per region
  dplyr::group_by (Year, `Product Category`, Region) %>%
  dplyr::summarise (exp = sum (`Product Expenditure`, na.rm = T)) %>%
  
  # adding CAGRs
  dplyr::group_by (`Product Category`, Region) %>%
  mutate (CAGR = (CAGR_fun (exp)) * 100) %>% ungroup () %>%
  
  # filter years
  filter (Year %in% c(2023, 2030)) %>%
  
  # removing NAs
  drop_na() %>%
  
  # widening for delta calculation
  pivot_wider (names_from = "Year", values_from = 4:5) %>%
  
  # getting absolute difference to 2030
  mutate (diff_2030 = exp_2030 - exp_2023) %>%
  
  # keep relevant vars
  dplyr::select (`Product Category`, Region, exp_2030, CAGR_2030, diff_2030) %>%
  
  # scaling exp_2030 and diff_2030
  mutate (exp_2030 = exp_2030 / 10^9,
          diff_2030 = diff_2030 / 10^9) %>% 
  unite ("category_region", `Product Category`:Region, sep = "_", remove = F, na.rm = F)

## plotting
install.packages("scatterplot3d")
library("scatterplot3d")

colors <- worlddataverse::wdl_cols()
colors <- colors [as.numeric (factor (plot_3d_dat$Region))]

shapes = c(15, 16, 17, 18) 
shapes <- shapes [as.numeric(factor (plot_3d_dat$`Product Category`))]

scatterplot3d (plot_3d_dat$CAGR_2030, plot_3d_dat$diff_2030, plot_3d_dat$exp_2030,
               xlab = "CAGR until 2030 (%)",
               ylab = "Absolute Difference until 2030 (Billion $ Nominal)",
               zlab = "Size in 2030 (Billion $ Nominal)", 
               pch = shapes,
               color = colors)

legend ("right", legend = levels (factor(plot_3d_dat$Region)),
       col = colors, pch = 16)

legend ("bottom", legend = levels (factor(plot_3d_dat$`Product Category`)), 
        pch = c(16, 17, 18, 19), inset = -0.2, xpd = TRUE, horiz = TRUE)


## bubble chart
ggplot (plot_3d_dat, aes (x=CAGR_2030, y=diff_2030, size=exp_2030, color = Region, pch = `Product Category`)) +
  geom_point (alpha=0.5) +
  scale_size (range=c(2, 20), name='Absolute Size') + 
  worlddataverse::theme_wdl() +
  scale_shape_manual(values = c(15, 16, 17, 18))

ggplot (plot_3d_dat,
        aes (x=CAGR_2030, y=diff_2030)) +
  geom_point (aes (size=exp_2030, color = Region), alpha=0.5) +
  geom_text (aes (label = category_region), size = 2) +
  scale_size (range=c(2, 30), name='Absolute Size') + 
  worlddataverse::theme_wdl()


## Chapter 6: Total personal care, beauty and others ---------------------------

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
  geom_bar (stat = "identity", position = "stack") + 
  theme (legend.position = "bottom",
         axis.text.x = element_text (angle = 360)) + 
  worlddataverse::theme_wdl() + 
  worlddataverse::scale_fill_wdl2()
  
  
  
  
















