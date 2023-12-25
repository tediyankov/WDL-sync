
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
    scale_color_manual(values = pal_worlddataverse)
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


## 1 bar chart: classes (headcount) ----------------------------------------------

# input data
wdp_2 = wdp_raw %>% worlddataverse::rds_to_mp (ages = F, inc = T, inc_vec = c(0, 12, 40, 80, 120, Inf))
wdp_input_classes = wdp_2 %>%
  mutate (daily_spending = ifelse (daily_spending == "[0,12)", "Poor & Vulnerable", 
                                   ifelse (daily_spending %in% c("[12,40)", "[40,80)", "[80,120)"), "Middle", "Upper Class & Rich"))) %>%
  group_by (year, daily_spending) %>%
  summarise (people = sum (hc.pdf.m, na.rm = T),
             spending = sum (exp.pdf.m, na.rm = T)) %>%
  filter (year %in% c(2011,2022,2040)) %>%
  mutate (people = people / 1000000000, 
          spending = spending / 1000000000000) %>%
  mutate (daily_spending = as.factor (daily_spending)) %>%
  mutate (daily_spending = factor (daily_spending, levels = c("Upper Class & Rich", "Middle", "Poor & Vulnerable")))

# plot code
plot_1 <- ggplot (wdp_input_classes, aes (x = factor (year), y = people, fill = daily_spending)) + 
  geom_bar (position = "stack", stat = "identity") + 
  theme_wdl () + 
  scale_fill_worlddataverse () +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())
plot_1

## 2 bar chart: classes (spending) ---------------------------------------------
wdp_input_classes_2 = wdp_2 %>%
  filter (!(daily_spending == "[0,12)")) %>%
  mutate (daily_spending = ifelse (daily_spending == "[12,40)", "Lower Middle", 
                                   ifelse (daily_spending == "[40,80)", "Middle", 
                                           ifelse (daily_spending == "[80,120)", "Upper Middle", "Upper Class & Rich")))) %>%
  group_by (year, daily_spending) %>% 
  summarise (spending = sum (exp.pdf.m, na.rm = T)) %>%
  filter (year %in% c(2011,2022,2040)) %>%
  mutate (spending = spending / 1000000000000) %>%
  mutate (daily_spending = as.factor (daily_spending)) %>%
  mutate (daily_spending = factor (daily_spending, levels = c("Upper Class & Rich", "Upper Middle", "Middle", "Lower Middle")))

plot_2 <- ggplot (wdp_input_classes_2, aes (x = factor (year), y = spending, fill = daily_spending)) + 
  geom_bar (position = "stack", stat = "identity") + 
  theme_wdl () + 
  scale_fill_worlddataverse () +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())
plot_2

## 3 bar chart: poor: continent (headcount) ------------------------------------

wdp_input_continuent_poor = wdp %>%
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
  mutate (poorvuln = poorvuln / 1000000000)

wdp_input_continuent_cons = wdp %>%
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
  mutate (consumer = consumer / 1000000000)

wdp_input_continent = wdp_input_continuent_poor %>% 
  left_join (wdp_input_continuent_cons) %>% 
  relocate ("year", "continent", "poorvuln", "consumer") %>%
  pivot_longer (3:4, names_to = "class", values_to = "people") %>%
  filter (!(continent == "Oceania")) %>%
  mutate (continent = as.factor (continent)) %>%
  mutate (continent = factor (continent, levels = c("Europe", "Americas", "Africa", "Asia"))) %>%
  drop_na (continent)

plot_3 <- ggplot (wdp_input_continent %>% 
                    filter (class == "poorvuln") %>%
                    filter (!(continent == "World")), 
                  aes (x = factor (year), y = people, fill = continent)) + 
  geom_bar (position = "stack", stat = "identity") + 
  theme_wdl () + 
  scale_fill_worlddataverse () +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())
plot_3

## 4 bar chart: consumer: continent (headcount) --------------------------------

plot_4 <- ggplot (wdp_input_continent %>% 
                    filter (class == "consumer") %>%
                    filter (!(continent == "World")), 
                  aes (x = factor (year), y = people, fill = continent)) + 
  geom_bar (position = "stack", stat = "identity") + 
  theme_wdl () + 
  scale_fill_worlddataverse () +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())
plot_4

## 5 bar chart: consumer: continent (spending) ---------------------------------

wdp_input_continuent_cons_spending = wdp %>%
  filter (daily_spending == "[12,Inf)") %>% 
  group_by (year, ccode) %>%
  mutate (continent = countrycode (ccode, "iso3c", "continent")) %>%
  ungroup () %>%
  group_by (year, continent) %>%
  summarise (spending = sum (exp.pdf.m, na.rm = T)) %>%
  filter (year %in% c(2011,2022,2040)) %>%
  mutate (spending = spending / 1000000000000) %>%
  filter (!(continent == "Oceania")) %>%
  mutate (continent = as.factor (continent)) %>%
  mutate (continent = factor (continent, levels = c("Europe", "Americas", "Africa", "Asia"))) %>%
  drop_na (continent)
  

plot_5 <- ggplot (wdp_input_continuent_cons_spending %>%
                    filter (!(continent == "World")), 
                  aes (x = factor (year), y = spending, fill = continent)) + 
  geom_bar (position = "stack", stat = "identity") + 
  theme_wdl () + 
  scale_fill_worlddataverse () +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())
plot_5


wdp_raw <- readRDS (file.path (currentwdp_path,"05_003_merge_standard_ages_R_2022_10_11_2017ppp_1_USD.rds"))
wdp = wdp_raw %>% worlddataverse::rds_to_mp (ages = F, inc = T, inc_vec = c(0, 12, Inf))
wdp_checks <- wdp %>%
  group_by (ccode, year, daily_spending) %>%
  summarise (consumer_head = sum (hc.pdf.m, na.rm = T)) %>%
  filter (daily_spending == "[12,Inf)")



blockrand = function(seed,blocksize,N){
  
  set.seed(seed)
  block = rep(1:ceiling (N/blocksize), each = blocksize)
  a1 = data.frame (block, 
                   rand = runif(length(block)), 
                   envelope = 1: length(block))
  a2 = a1 [order (a1$block,a1$rand),]
  a2$arm = rep (c("Arm 1", "Arm 2"), times = length(block)/2)
  assign = a2 [order (a2$envelope),]
  return(assign)
  
}







