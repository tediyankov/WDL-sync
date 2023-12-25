
## preliminaries  --------------------------------------------------------------

## packages
pacman::p_load(tidyverse,countrycode,data.table,ggthemes,showtext,png,grid,cowplot,hrbrthemes,magick,ggrepel,ggbump,gt,janitor,tidyr,purrr)

## paths
base_path <- worlddataverse::get_wdl_path()

if (is.na (base_path)) {base_path = file.path("/Volumes/",
                                              "GoogleDrive-115239951252043738907",
                                              "Shared drives",
                                              "DATA_WDL")}

output_path_wec <- file.path (base_path, "8 billion day")
wdp_path <- file.path(base_path,'Mpro_2.0')
currentwdp_path <- file.path(wdp_path,'01_Data','R_2022_10_11','2022_10_12_second_2017ppp','03_outputs')
output_path <- file.path(base_path,"data_work_misc","loreal_world_cpd_2022-10-04")
input_path <- file.path(output_path,'input_data')

## plot aesthetics -------------------------------------------------------------

pal_worlddataverse <- c("#0dcdc0", "#264653", "#e9c46a", "#f4a261", "#e76f51", "#613FC2")
pal_worlddataverse1 <- c("#613FC2","#000245","#A08CDA","#D848C4","#FCDC00","#FEF1D3","#00A046","#D6D6D6","#F5F5F5","#0dcdc0", "#264653", "#e9c46a", "#f4a261", "#e76f51", "#613FC2")

# get fonts
font_add_google("Work Sans", "worksans")
showtext_auto()

# get logo for branding

# for iMac 
img <- readPNG(source = file.path("/Volumes/",
                                  "GoogleDrive-115239951252043738907",
                                  "Shared drives",
                                  "DATA_WDL",
                                  "worlddataverse",
                                  "wdl_logo.png"))

# for laptop
img <- readPNG(source = "/Volumes/GoogleDrive/Shared drives/DATA_WDL/worlddataverse/wdl_logo.png")

scale_fill_worlddataverse <- function(){
  structure(list(
    scale_fill_manual(values=pal_worlddataverse)
  ))
}

scale_colour_discrete_worlddataverse <- function (){
  structure(list(
    scale_color_manual(values = pal_worlddataverse1)
  ))
}

theme_wdl <- function (base_size = 8, base_font = NA){
  theme_ipsum(base_size=8,base_family="worksans") +
    theme (
      text = element_text (colour = "#000245", face = "plain"),
      plot.title = element_text(size = 18, face = "bold", hjust = 0, colour = "#000245", margin = margin(0,0,8,0)),
      axis.title = element_text(size = 14, face = "italic", colour = "#202020"),
      axis.text.x = element_text (angle = 90, vjust = 0.5, hjust = 1),
      panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "#F5F5F5"),
      panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "#F5F5F5"),
      legend.text = element_text (size = 10, colour = "#000245", face = "plain"),
      legend.title = element_text (colour = "#000245", face = "bold")
    )
}

## bar plot: P&V vs CC for world at 7 8 and 9 billion --------------------------

wdp_bar_data_poor = wdp %>%
  filter (daily_spending == "[0,12)") %>%
  group_by (ccode, year) %>%
  summarise (poorvuln = sum (hc.pdf.m, na.rm = T)) %>%
  ungroup () %>% 
  mutate (continent = countrycode (ccode, "iso3c", "continent")) %>%
  group_by (year, continent) %>%
  summarise (poorvuln = sum (poorvuln, na.rm = T)) %>%
  arrange (year) %>%
  ungroup () %>%
  filter (year %in% c(2011, 2022, 2040)) %>% 
  add_row (year = 2011, continent = "World", poorvuln = sum (.$poorvuln [.$year == 2011])) %>% 
  add_row (year = 2022, continent = "World", poorvuln = sum (.$poorvuln [.$year == 2022])) %>% 
  add_row (year = 2040, continent = "World", poorvuln = sum (.$poorvuln [.$year == 2022])) %>%
  arrange (year) %>%
  mutate (World = "World") %>%
  mutate (poorvuln = poorvuln / 1000000)

wdp_bar_data_cons = wdp %>%
  filter (daily_spending == "[12,Inf)") %>%
  group_by (ccode, year) %>%
  summarise (consumer = sum (hc.pdf.m, na.rm = T)) %>%
  ungroup () %>% 
  mutate (continent = countrycode (ccode, "iso3c", "continent")) %>%
  group_by (year, continent) %>%
  summarise (consumer = sum (consumer, na.rm = T)) %>%
  arrange (year) %>%
  ungroup () %>%
  filter (year %in% c(2011, 2022, 2040)) %>% 
  add_row (year = 2011, continent = "World", consumer = sum (.$consumer [.$year == 2011])) %>% 
  add_row (year = 2022, continent = "World", consumer = sum (.$consumer [.$year == 2022])) %>% 
  add_row (year = 2040, continent = "World", consumer = sum (.$consumer [.$year == 2022])) %>%
  arrange (year) %>%
  mutate (World = "World") %>%
  mutate (consumer = consumer / 1000000)

wdp_bar_data = wdp_bar_data_poor %>% 
  left_join (wdp_bar_data_cons) %>% 
  relocate ("year", "continent", "poorvuln", "consumer", "World") %>%
  pivot_longer (3:4, names_to = "class", values_to = "people")

wdp_bar_7 = ggplot (data = wdp_bar_data %>%
                      filter (year == 2011) %>%
                      filter (!(continent == "World")) %>% 
                      mutate (class = ifelse (class == "consumer", 
                                              "Consumer Class",
                                              "Poor & Vulnerable"),
                              people = people / 1000), 
                    aes (x = class, y = people, fill = continent)) +
  geom_bar (position = "stack", stat = "identity") + 
  theme_wdl () + 
  theme (axis.text.x = element_text (angle = 90, vjust = 1.12)) + 
  scale_fill_worlddataverse () +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

wdp_bar_7
grid::grid.raster(img, x = 0.07, y = 0.03, just = c('left', 'bottom'), width = unit(1.3, 'inches'))

wdp_bar_8 = ggplot (data = wdp_bar_data %>%
                      filter (year == 2022) %>%
                      filter (!(continent == "World")) %>% 
                      mutate (class = ifelse (class == "consumer", 
                                              "Consumer Class",
                                              "Poor & Vulnerable"),
                              people = people / 1000), 
                    aes (x = class, y = people, fill = continent)) +
  geom_bar (position = "stack", stat = "identity") + 
  theme_wdl () + 
  theme (axis.text.x = element_text (angle = 90, vjust = 1.12)) + 
  scale_fill_worlddataverse () +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

wdp_bar_8
grid::grid.raster(img, x = 0.07, y = 0.03, just = c('left', 'bottom'), width = unit(1.3, 'inches'))

wdp_bar_9 = ggplot (data = wdp_bar_data %>%
                      filter (year == 2040) %>%
                      filter (!(continent == "World")) %>% 
                      mutate (class = ifelse (class == "consumer", 
                                              "Consumer Class",
                                              "Poor & Vulnerable"),
                              people = people / 1000), 
                    aes (x = class, y = people, fill = continent)) +
  geom_bar (position = "stack", stat = "identity") + 
  theme_wdl () + 
  theme (axis.text.x = element_text (angle = 90, vjust = 1.12)) + 
  scale_fill_worlddataverse () +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

wdp_bar_9
grid::grid.raster(img, x = 0.07, y = 0.03, just = c('left', 'bottom'), width = unit(1.3, 'inches'))

## bar plot: consumer class growth (headcounts) --------------------------------

wdp_input = wdp %>%
  filter (daily_spending == "[12,Inf)") %>%
  group_by (year) %>%
  summarise (headcount = sum (hc.pdf.m, na.rm = T),
             spending = sum (exp.pdf.m, na.rm = T)) %>%
  filter (year %in% 2009:2030) %>%
  mutate (headcount_lagged = Hmisc::Lag (headcount, 1),
          spending_lagged = Hmisc::Lag (spending, 1)) %>%
  mutate (headcount_delta = headcount - headcount_lagged,
          spending_delta = spending - spending_lagged) %>%
  mutate (headcount_delta = headcount_delta / 1000000,
          spending_delta = spending_delta / 1000000000000) %>%
  dplyr::select (year, headcount_delta, spending_delta) %>%
  filter (year %in% 2010:2030) %>%
  mutate (year = as.numeric (year),
          headcount_delta = as.numeric (headcount_delta),
          spending_delta = as.numeric (spending_delta)) %>%
  mutate (fill = ifelse (headcount_delta > 105,"a",
                         ifelse (headcount_delta < 0, "b","c"))) %>%
  mutate (fill = ifelse (year == 2021, "d", fill))

headcount <- ggplot (wdp_input, aes (x = year, y = headcount_delta, fill = fill)) + 
  geom_bar (stat = "identity") + 
  # geom_line (col = "#e9c46a", lwd = 1) +
  theme_wdl() + 
  theme (legend.position = "none") + 
  scale_fill_manual (values = c("#264652", "red", "#E9C36A", "#5ECEC1")) + 
  scale_x_continuous ("Year", labels = as.character(wdp_input$year), breaks = wdp_input$year) +
  scale_y_continuous ("Headcounts in Millions", labels = seq (-100, 200, 50), breaks = seq (-100, 200, 50)) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

headcount
grid::grid.raster(img, x = 0.07, y = 0.03, just = c('left', 'bottom'), width = unit(1.3, 'inches'))

spending <- ggplot (wdp_input, aes (x = year, y = spending_delta, fill = fill)) + 
  geom_bar (stat = "identity") + 
  # geom_line (col = "#e9c46a", lwd = 1) +
  theme_wdl() + 
  theme (legend.position = "none") + 
  scale_fill_manual (values = c("#264652", "red", "#E9C36A", "#5ECEC1")) + 
  scale_x_continuous ("Year", labels = as.character(wdp_input$year), breaks = wdp_input$year) + 
  #scale_y_continuous ("Headcounts in Millions", labels = seq (-100, 200, 50), breaks = seq (-100, 200, 50)) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

spending
grid::grid.raster(img, x = 0.07, y = 0.03, just = c('left', 'bottom'), width = unit(1.3, 'inches'))

## spending by age -------------------------------------------------------------

wdp_2 = wdp_raw %>% worlddataverse::rds_to_mp (ages = F, inc = T, inc_vec = c(0, 12, 40, 80, 120, Inf))
wdp_input_2 <- wdp_2 %>%
  mutate (age_group = ifelse (age_group %in% c("[00,05)","[05,10)","[10,15)"), "Children",
                              ifelse (age_group %in% c("[15,20)","[20,25)","[25,30)","[30,35)","[35,40)","[40,45)","[45,50)","[50,55)","[55,60)","[60,65)"), "Working Age", "Seniors"))) %>%
  group_by (year, age_group, daily_spending) %>%
  summarise (spending = sum (exp.pdf.m, na.rm = T)) %>%
  filter (year %in% c(2011,2022,2040)) %>%
  mutate (spending = spending / 1000000000000) %>%
  mutate (daily_spending = ifelse (daily_spending == "[0,12)", "Poor & Vulnerable", 
                                   ifelse (daily_spending == "[12,40)", "Lower Middle", 
                                           ifelse (daily_spending == "[40,80)", "Middle", 
                                                   ifelse (daily_spending == "[80,120)", "Upper Middle", "Upper Middle & Rich")))))

plot_spend_age_7 = ggplot (data = wdp_input_2 %>% filter (year == 2011), 
                         aes (x = age_group, y = spending, fill = daily_spending)) + 
  geom_bar (position = "stack", stat = "identity") + 
  theme_wdl () + 
  scale_fill_worlddataverse () +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) + 
  coord_flip ()

plot_spend_age_7
grid::grid.raster(img, x = 0.07, y = 0.03, just = c('left', 'bottom'), width = unit(1.3, 'inches'))

plot_spend_age_8 = ggplot (data = wdp_input_2 %>% filter (year == 2022), 
                           aes (x = age_group, y = spending, fill = daily_spending)) + 
  geom_bar (position = "stack", stat = "identity") + 
  theme_wdl () + 
  scale_fill_worlddataverse () +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) + 
  scale_y_continuous (labels = seq (0, 100, 10), breaks = seq (0, 100, 10)) + 
  coord_flip ()

plot_spend_age_8
grid::grid.raster(img, x = 0.07, y = 0.03, just = c('left', 'bottom'), width = unit(1.3, 'inches'))

plot_spend_age_9 = ggplot (data = wdp_input_2 %>% filter (year == 2040), 
                           aes (x = age_group, y = spending, fill = daily_spending)) + 
  geom_bar (position = "stack", stat = "identity") + 
  theme_wdl () + 
  scale_fill_worlddataverse () +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) + 
  scale_y_continuous (labels = seq (0, 100, 10), breaks = seq (0, 100, 10)) + 
  coord_flip ()

  plot_spend_age_9
grid::grid.raster(img, x = 0.07, y = 0.03, just = c('left', 'bottom'), width = unit(1.3, 'inches'))

wdp_spending_age <- wdp %>%
  mutate (age_group = ifelse (age_group %in% c("[00,05)","[05,10)","[10,15)","[15,20)","[20,25)","[25,30)"), 
                              "Young", 
                              "Old")) %>%
  group_by (year, age_group, daily_spending) %>%
  summarise (spending = sum (exp.pdf.m, na.rm = T)) %>%
  filter (year %in% c(2011,2022,2040)) %>%
  mutate (spending = spending / 1000000000000)
  

## continent breakdown of spending charts: spending not headcounts -------------

wdp_bar_data_poor_spend = wdp %>%
  filter (daily_spending == "[0,12)") %>%
  group_by (ccode, year) %>%
  summarise (poorvuln = sum (exp.pdf.m, na.rm = T)) %>%
  ungroup () %>% 
  mutate (continent = countrycode (ccode, "iso3c", "continent")) %>%
  group_by (year, continent) %>%
  summarise (poorvuln = sum (poorvuln, na.rm = T)) %>%
  arrange (year) %>%
  ungroup () %>%
  filter (year %in% c(2011, 2022, 2040)) %>% 
  #add_row (year = 2011, continent = "World", poorvuln = sum (.$poorvuln [.$year == 2011])) %>% 
  #add_row (year = 2022, continent = "World", poorvuln = sum (.$poorvuln [.$year == 2022])) %>% 
  #add_row (year = 2040, continent = "World", poorvuln = sum (.$poorvuln [.$year == 2022])) %>%
  arrange (year)
  #mutate (World = "World") 

wdp_bar_data_cons_spend = wdp %>%
  filter (daily_spending == "[12,Inf)") %>%
  group_by (ccode, year) %>%
  summarise (consumer = sum (exp.pdf.m, na.rm = T)) %>%
  ungroup () %>% 
  mutate (continent = countrycode (ccode, "iso3c", "continent")) %>%
  group_by (year, continent) %>%
  summarise (consumer = sum (consumer, na.rm = T)) %>%
  arrange (year) %>%
  ungroup () %>%
  filter (year %in% c(2011, 2022, 2040)) %>% 
  #add_row (year = 2011, continent = "World", consumer = sum (.$consumer [.$year == 2011])) %>% 
  #add_row (year = 2022, continent = "World", consumer = sum (.$consumer [.$year == 2022])) %>% 
  #add_row (year = 2040, continent = "World", consumer = sum (.$consumer [.$year == 2022])) %>%
  arrange (year)
  #mutate (World = "World")

wdp_bar_data_spend = wdp_bar_data_poor_spend %>% 
  left_join (wdp_bar_data_cons_spend) %>% 
  relocate ("year", "continent", "poorvuln", "consumer") %>%
  pivot_longer (3:4, names_to = "class", values_to = "spending") %>%
  mutate (spending = spending / 1000000000000)

wdp_bar_7_spend = ggplot (data = wdp_bar_data_spend %>%
                      filter (year == 2011) %>% 
                      mutate (class = ifelse (class == "consumer", 
                                              "Consumer Class",
                                              "Poor & Vulnerable")), 
                    aes (x = class, y = spending, fill = continent)) +
  geom_bar (position = "stack", stat = "identity") + 
  theme_wdl () + 
  theme (axis.text.x = element_text (angle = 90, vjust = 1.12)) + 
  scale_fill_worlddataverse () +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

wdp_bar_7_spend
grid::grid.raster(img, x = 0.07, y = 0.03, just = c('left', 'bottom'), width = unit(1.3, 'inches'))

wdp_bar_8_spend = ggplot (data = wdp_bar_data_spend %>%
                            filter (year == 2022) %>% 
                            mutate (class = ifelse (class == "consumer", 
                                                    "Consumer Class",
                                                    "Poor & Vulnerable")), 
                          aes (x = class, y = spending, fill = continent)) +
  geom_bar (position = "stack", stat = "identity") + 
  theme_wdl () + 
  theme (axis.text.x = element_text (angle = 90, vjust = 1.12)) + 
  scale_fill_worlddataverse () +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

wdp_bar_8_spend
grid::grid.raster(img, x = 0.07, y = 0.03, just = c('left', 'bottom'), width = unit(1.3, 'inches'))

wdp_bar_9_spend = ggplot (data = wdp_bar_data_spend %>%
                            filter (year == 2040) %>% 
                            mutate (class = ifelse (class == "consumer", 
                                                    "Consumer Class",
                                                    "Poor & Vulnerable")), 
                          aes (x = class, y = spending, fill = continent)) +
  geom_bar (position = "stack", stat = "identity") + 
  theme_wdl () + 
  theme (axis.text.x = element_text (angle = 90, vjust = 1.12)) + 
  scale_fill_worlddataverse () +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

wdp_bar_9_spend
grid::grid.raster(img, x = 0.07, y = 0.03, just = c('left', 'bottom'), width = unit(1.3, 'inches'))




































