




## r2 of the three regressions for Luis ----------------------------------------

## data
pc_age = read_csv (file.path (base_path, 
                              "spending_categories", 
                              "sample_data_wdp", 
                              "05_003_merge_mpro2_ages_R_2023_07_26_2017ppp_1_USD_with_categories_with_demogs_corrected.csv"))


pc_age_for_r2 = pc_age %>%
  
  # isolating vars
  dplyr::group_by (ccode, year, spending_group) %>%
  dplyr::summarise (exp = sum (exp, na.rm = T))

## regression 1: Personal care spending ~ population

# adding population to data
popdat = wdp %>%
  
  # aggregating total 
  dplyr::group_by (ccode, year) %>%
  dplyr::summarise (pop = sum (hc.pdf.m, na.rm = T))

reg1input = pc_age_for_r2 %>% left_join (popdat, by = c("ccode", "year")) %>% drop_na()

# running regression
summary (lm (exp ~ pop, data = reg1input))$r.squared

## regression 2: Personal care spending ~ total spending

# adding total spending to data
spenddat = wdp %>%
  
  # aggregating total 
  dplyr::group_by (ccode, year) %>%
  dplyr::summarise (exp = sum (exp.pdf.m, na.rm = T))

reg2input = pc_age_for_r2 %>% left_join (spenddat, by = c("ccode", "year")) %>% drop_na()

# running regression
summary (lm (`Personal Care` ~ exp, data = reg2input))$adj.r.squared

## regression 3: Personal care spending ~ spending category size in expenditure

# adding individual spending category size
wdp2 <- wdp_raw %>% worlddataverse::rds_to_wdpro (ages = F, inc = T, inc_vec = c(0, 12, 40, 80, 120, Inf))

spendcatdat = wdp2 %>%
  
  # aggregation
  dplyr::group_by (ccode, year, daily_spending) %>%
  dplyr::summarise (exp = sum (exp.pdf.m, na.rm = T)) %>%
  
  # rename 
  rename (spending_group = daily_spending)

reg3input = pc_age_for_r2 %>% left_join (spendcatdat, by = c("ccode", "spending_group", "year")) %>% drop_na() %>% 
  #pivot_longer (5:9, names_to = "daily_spend", values_to = "exp2") %>%
  filter (year %in% 2023)

# running regression
summary (lm (`Personal Care` ~ `[0,12)` + `[12,40)` + `[40,80)` + `[80,120)` + `[120,Inf)`, 
             data = reg3input))$adj.r.squared

## drawing scatter plots

# reg1

reg1plot2023 = ggplot (data = reg1input %>% filter (year %in% 2023), aes (x = pop, y = exp)) + 
  geom_jitter () + 
  #geom_smooth (method = "lm") + 
  stat_poly_line() +
  stat_poly_eq() +
  #scale_y_continuous (trans = "log") + 
  #scale_x_continuous (trans = "log") + 
  worlddataverse::theme_wdl() + 
  labs (#title = "Personal care spending ~ population",
    subtitle = "Subset for 2023, R2 reflects that",
    x = "Population", 
    y = "Personal Care Spending") + 
  theme (axis.text.x = element_text (angle = 360))


reg1plot = ggplot (data = reg1input, aes (x = pop, y = exp)) + 
  geom_jitter () + 
  #geom_smooth (method = "lm") + 
  stat_poly_line() +
  stat_poly_eq() +
  #scale_y_continuous (trans = "log") + 
  #scale_x_continuous (trans = "log") + 
  worlddataverse::theme_wdl() + 
  labs (#title = "Personal care spending ~ population",
    subtitle = "All years, R2 reflects that",
    x = "Population", 
    y = "Personal Care Spending") + 
  theme (axis.text.x = element_text (angle = 360))

reg1plotlog2023 = ggplot (data = reg1input %>% filter (year %in% 2023), aes (x = pop, y = exp)) + 
  geom_jitter () + 
  #geom_smooth (method = "lm") + 
  stat_poly_line() +
  stat_poly_eq() +
  scale_y_continuous (trans = "log") + 
  scale_x_continuous (trans = "log") + 
  worlddataverse::theme_wdl() + 
  #labs (#title = "Personal care spending ~ population",
  #subtitle = "Axes log-transformed and subset for 2023, \nR2 reflects that",
  #x = "Population", 
  #y = "Personal Care Spending") + 
  theme (axis.text.x = element_text (angle = 360)) + 
  theme_void ()
reg1plotlog2023

plotlist <- list (reg1plot, reg1plot2023, reg1plotlog2023)
reg1masterplot <- ggarrange (plotlist = plotlist, nrow = 1, ncol = 3)
annotate_figure (reg1masterplot,
                 top = text_grob ("Personal care spending ~ population",
                                  color = "Black", face = "bold", size = 14))

# reg2

reg2plot2023 = ggplot (data = reg2input %>% filter (year %in% 2023), aes (x = exp.y, y = exp.x)) + 
  geom_jitter () + 
  #geom_smooth (method = "lm") + 
  stat_poly_line() +
  stat_poly_eq() +
  #scale_y_continuous (trans = "log") + 
  #scale_x_continuous (trans = "log") + 
  worlddataverse::theme_wdl() + 
  labs (#title = "Personal care spending ~ population",
    subtitle = "Subset for 2023, R2 reflects that",
    x = "Total spending", 
    y = "Personal Care Spending") + 
  theme (axis.text.x = element_text (angle = 360))


reg2plot = ggplot (data = reg2input, aes (x = exp, y = `Personal Care`)) + 
  geom_jitter () + 
  #geom_smooth (method = "lm") + 
  stat_poly_line() +
  stat_poly_eq() +
  #scale_y_continuous (trans = "log") + 
  #scale_x_continuous (trans = "log") + 
  worlddataverse::theme_wdl() + 
  labs (#title = "Personal care spending ~ population",
    subtitle = "All years, R2 reflects that",
    x = "Total spending", 
    y = "Personal Care Spending") + 
  theme (axis.text.x = element_text (angle = 360))

reg2plotlog2023 = ggplot (data = reg2input %>% filter (year %in% 2023), aes (x = exp.y, y = exp.x)) + 
  geom_jitter () + 
  #geom_smooth (method = "lm") + 
  stat_poly_line() +
  stat_poly_eq() +
  scale_y_continuous (trans = "log") + 
  scale_x_continuous (trans = "log") + 
  worlddataverse::theme_wdl() + 
  #labs (#title = "Personal care spending ~ population",
  #subtitle = "Axes log-transformed and subset for 2023, \nR2 reflects that",
  #x = "Total spending", 
  #y = "Personal Care Spending") + 
  theme (axis.text.x = element_text (angle = 360)) +
  theme_void () 
reg2plotlog2023

plotlist <- list (reg2plot, reg2plot2023, reg2plotlog2023)
reg2masterplot <- ggarrange (plotlist = plotlist, nrow = 1, ncol = 3)
annotate_figure (reg2masterplot,
                 top = text_grob ("Personal care spending ~ total spending",
                                  color = "Black", face = "bold", size = 14))


# reg3
reg3plot = ggplot (data = reg3input %>% pivot_longer (4:8, names_to = "daily_spend", values_to = "exp2"),
                   aes (x = exp.y, y = exp.x)) + 
  geom_jitter () + 
  #geom_smooth (method = "lm") + 
  stat_poly_line() +
  stat_poly_eq() +
  #scale_y_continuous (trans = "log") + 
  #scale_x_continuous (trans = "log") + 
  worlddataverse::theme_wdl() + 
  labs (title = "Personal care spending ~ spending category size",
        subtitle = "All years, R2 reflects that",
        x = "Spending", 
        y = "Personal Care Spending") + 
  theme (axis.text.x = element_text (angle = 360)) + 
  facet_wrap (~daily_spend, scales = "free")

reg3plot

reg3plot2023 = ggplot (data = reg3input %>% 
                         pivot_longer (4:8, names_to = "daily_spend", values_to = "exp2") %>%
                         filter (year %in% 2023),
                       aes (x = exp2, y = exp)) + 
  geom_jitter () + 
  #geom_smooth (method = "lm") + 
  stat_poly_line() +
  stat_poly_eq() +
  #scale_y_continuous (trans = "log") + 
  #scale_x_continuous (trans = "log") + 
  worlddataverse::theme_wdl() + 
  labs (title = "Personal care spending ~ spending category size",
        subtitle = "Subset for 2023, R2 reflects that",
        x = "Spending", 
        y = "Personal Care Spending") + 
  theme (axis.text.x = element_text (angle = 360)) + 
  facet_wrap (~daily_spend, scales = "free")

reg3plot2023


reg3plot2023log = ggplot (data = reg3input, #%>% 
                            #filter (spending_group == "[40,80)"),
                          aes (x = exp.y, y = exp.x)) + 
  geom_jitter () + 
  #geom_smooth (method = "lm") + 
  stat_poly_line() +
  stat_poly_eq() +
  scale_y_continuous (trans = "log") + 
  scale_x_continuous (trans = "log") + 
  worlddataverse::theme_wdl() + 
  #labs (title = "Personal care spending ~ spending category size",
  # subtitle = "Log transformed + Subset for 2023 and $40-$80, R2 reflects that",
  #x = "Spending", 
  #y = "Personal Care Spending") + 
  theme (axis.text.x = element_text (angle = 360)) + 
  theme_void () + 
  facet_wrap (~spending_group, scales = "free")

reg3plot2023log
