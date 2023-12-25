
## IOM paper revisions (13 Mar 2023) ===========================================

## clean environment
rm (list = ls())

## load packages
pacman::p_load (tidyverse, countrycode, worlddataverse, ggimage, wcde, envalysis)

## set font
worlddataverse::font_wdl()

## Loading data in
library (wcde)

iiasa_age <- get_wcde (indicator = "bpop",
                       country_name = c("Austria", "Belgium", "Czech Republic", "Denmark", "Estonia",
                                        "Finland", "France", "Germany", "Greece", "Hungary", "Iceland",
                                        "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg",
                                        "Netherlands", "Norway", "Poland", "Portugal", "Slovak Republic",
                                        "Slovenia", "Spain", "Sweden", "Switzerland", "United Kingdom",
                                        "Canada", "Chile", "Colombia", "Mexico", "Costa Rica",
                                        "United States", "Australia", "Japan", "Korea", "New Zealand",
                                        "Israel", "Turkey"))
## cleaning data 
iiasa_age_clean = iiasa_age %>%
  
  # filtering
  filter (year %in% c(2020, 2030, 2040)) %>%
  filter (!age == "All") %>%
  filter (sex == "Both") %>%
  filter (age %in% c("0--19", "20--64", "65+")) %>%
  
  # renaming countries into ISO3 codes 
  mutate (ccode = countrycode (name, "country.name", "iso3c")) %>%
  dplyr::select (ccode, age, year, bpop) %>%
  
  # adding population sum variable
  group_by (ccode, year) %>%
  mutate (pop_sum = sum (bpop, na.rm = T)) %>%
  ungroup () %>%
  
  # computing shares var
  mutate (share = bpop / pop_sum) %>%
  
  # changing the order of the age var 
  mutate (age = factor (age, levels = c("0--19", "20--64", "65+")))

## building plot
library (envalysis)

# theme 
theme_Publication <- function(base_size=14, base_family="helvetica") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(), 
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.key.size= unit(0.2, "cm"),
            legend.margin = unit(0, "cm"),
            legend.title = element_text(face="italic"),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text(face="bold")
    ))
  
}

scale_fill_Publication <- function(...){
  library(scales)
  discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}

scale_colour_Publication <- function(...){
  library(scales)
  discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}

# plot code
ggplot (data = iiasa_age_clean %>%
          filter (ccode %in% c("AUS", "BEL", "CAN", "CHE", "CZE", "DEU", "FRA",
                               "HUN", "IRL", "ISR", "NLD", "POL", "USA")),
        aes (x = year, y = share, fill = age)) + 
  geom_bar (stat = "identity", position = "stack") +
  facet_wrap (~ ccode, nrow = 1, ncol = 13) +
  labs (title = "Age Structure of Sample Countries in 2020, 2030 and 2040",
        subtitle = "IIASA data, Scenario 2") +
  scale_fill_Publication() +
  theme_Publication () + 
  theme (axis.text.x = element_text (angle = 90),
         plot.subtitle = element_text (hjust = 0.5))


# summary stats
iiasa_age_clean_2 = iiasa_age_clean %>%
  group_by (year, age) %>%
  summarise (bpop = sum (bpop, na.rm = T)) %>%
  group_by (year) %>%
  mutate (pop_sum = sum (bpop, na.rm = T)) %>%
  ungroup () %>%
  mutate (share = bpop / pop_sum)
  
  
  
  
  



