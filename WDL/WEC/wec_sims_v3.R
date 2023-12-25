
# add file paths once uploaded 
highWlowE = read.csv (highWlowE_v3.csv)

# dividing into diff rankings

highWlowE_1 = highWlowE %>% filter (rank == 1)
highWlowE_2 = highWlowE %>% filter (rank == 2)
highWlowE_3 = highWlowE %>% filter (rank == 3)

# adding how much each sub-sector contributes to total

highWlowE_1 = highWlowE_1 %>% 
  group_by (sector) %>%
  mutate (em_pc_sector = sum (em_pc)) %>%
  ungroup () %>%
  mutate (shares_sector = em_pc_sector / sum (unique (em_pc_sector)),
          shares_subsector = em_pc / sum (em_pc))

# obtaining highWlowE copositions for 2022:2050


highWlowE_future = function () {
  
  # OECD countries list
  OECD = c("Austria", "Belgium", "Czech Republic", "Denmark", "Estonia", 
           "Finland", "France", "Germany", "Greece", "Hungary", "Iceland", 
           "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", 
           "Netherlands", "Norway", "Poland", "Portugal", "Slovak Republic", 
           "Slovenia", "Spain", "Sweden", "Switzerland", "United Kingdom", 
           "Canada", "Chile", "Colombia", "Mexico", "Costa Rica", 
           "United States", "Australia", "Japan", "Korea", "New Zealand", 
           "Israel", "Turkey")
  
  # converting to ISO3 country codes
  OECD_ISO3 = countrycode (sourcevar = OECD,
                           origin = "country.name",
                           destination = "iso3c",
                           warn = TRUE, 
                           nomatch = NA)
  
  # sub-setting
  wec_dat_oecd = wec_dat %>% 
    filter (iso3c %in% OECD_ISO3)

  # iterating over years
  values <- c(2022:2050)
  datalist <- vector("list", length = length(unique(values)))
  
  for (i in values) {
    
    wec_dat_oecd_1 = wec_dat_oecd [wec_dat_oecd$year == i,]
    
    wec_dat_oecd_1 = wec_dat_oecd_1 %>%
      
      dplyr::select (-c("ndc", "o_1p5c", "h_cpol")) %>%
      group_by (year, iso3c, sector, subsector, pop, gdp) %>%
      summarise (emissions = sum(base)) %>%
      drop_na() %>%
      unite ("ID", iso3c:subsector, sep = "_", remove = F, na.rm = F) %>%
      mutate (em_pc = emissions / pop) 
    
    # setting GDP threshold
    wec_dat_oecd_1$gdp_threshold = mean (wec_dat_oecd_1$gdp)
    
    # first minimum emissions
    wec_dat_oecd_2 = wec_dat_oecd_1 %>% filter (gdp > gdp_threshold)
    highWlowE_1 = setDT(wec_dat_oecd_2)[,.SD[which.min(em_pc)],by = subsector]
    
    # second minimum emissions
    wec_dat_oecd_3 = wec_dat_oecd_2 %>% filter (!(ID %in% highWlowE_1$ID))
    highWlowE_2 = setDT(wec_dat_oecd_3)[,.SD[which.min(em_pc)],by = subsector]
    
    # third minimum emissions
    wec_dat_oecd_4 = wec_dat_oecd_3 %>% filter (!(ID %in% highWlowE_2$ID))
    highWlowE_3 = setDT(wec_dat_oecd_4)[,.SD[which.min(em_pc)],by = subsector]
    
    # binding into 1 data frame containing rankings per sub-sector
    highWlowE = rbind (highWlowE_1, highWlowE_2, highWlowE_3) %>%
      
      dplyr::select (c("year", "iso3c", "sector", "subsector", "em_pc")) %>%
      # adding country names
      mutate (country = countrycode (sourcevar = iso3c,
                                     origin = "iso3c",
                                     destination = "country.name",
                                     warn = TRUE,
                                     nomatch = NA)) %>%
      relocate (country, .before = sector) %>%
      arrange (subsector) %>%
      relocate (year, sector, subsector, iso3c, country, em_pc) 
    
    datalist[[i]] <- highWlowE
    
  }
  
  highWlowE_future = do.call(rbind, datalist)
  
  .GlobalEnv$highWlowE_future = highWlowE_future
  
}

highWlowE_future()


year <- as.factor (seq(2022,2050,1))
subsector <- as.factor (unique (wec_dat$subsector))

cart = expand.grid (year, subsector)

rows= c(1:nrow(cart))
times = 3
cart = cart[rep(rows, times),]

cart = cart %>% unite ("year_subsector", c(Var1, Var2), sep = "_", remove = F, na.rm = F) 
highWlowE_future = highWlowE_future %>% unite ("year_subsector", c(year, subsector), sep = "_", remove = F, na.rm = F) 

cart_ID = as.vector (cart$year_subsector)
highWlowE_future_ID = as.vector (highWlowE_future$year_subsector)

diff = setdiff (cart_ID, highWlowE_future_ID)

cart = cart %>% filter (!(year_subsector %in% unique (diff)))

cart_ID = as.vector (cart$year_subsector)
highWlowE_future_ID = as.vector (highWlowE_future$year_subsector)

diff_2 = setdiff (cart_ID, highWlowE_future_ID)

df = cbind (cart_ID, highWlowE_future_ID)



  


# pie chart

pie = ggplot (highWlowE_1, aes (x = "", y = shares_subsector, fill = sector)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0)

## bump chart 

ggplot(data = highWlowE, aes(x = year, y = ranking, group = country)) +
  geom_line(aes(color = country, alpha = 1), size = 2) +
  geom_point(aes(color = country, alpha = 1), size = 4) +
  geom_point(color = "#FFFFFF", size = 1) +
  scale_y_reverse(breaks = 1:show.top.n) +
  scale_x_continuous(breaks = 1:16, minor_breaks = 1:16, expand = c(.05, .05)) +
  geom_text(data = df.rankings %>% filter(day == "1"),
            aes(label = country, x = 0.5) , hjust = .85, fontface = "bold", color = "#888888", size = 4) +
  geom_text(data = df.rankings %>% filter(day == "16"),
            aes(label = country, x = 16.5) , hjust = 0.15, fontface = "bold", color = "#888888", size = 4) +
  coord_cartesian(ylim = c(1,show.top.n)) + 
  theme(legend.position = "none") +
  labs(x = "Competition days with medals",
       y = "Rank",
       title = "PyeongChang 2018 Olympic Winter Games",
       subtitle = "Countries ranked by overall medals after each competition day")

# data foundations


