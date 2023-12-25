
sample2 = DIOC_2000_20_1 %>% 
  filter (orig_dest %in% c("AFG_AUS", "ALB_TUR", "ECU_JPN"))

values = c("AFG_AUS", "ALB_TUR", "ECU_JPN")

datalist = vector("list", length = length(unique(values)))



interpol = function () {
  
  data1 = DIOC_2000_20_1
  data1 [data1 == 0] <- NA
  
  values = c("AFG_AUS", "ALB_TUR", "ECU_JPN")
  
  datalist = vector("list", length = length(unique(values)))
  
  for (i in values) {
    
    data1 = data1 %>%
      
      filter (orig_dest == i) %>%
      
      mutate (pri_15_34_lagged = Lag (pri_15_34, -1)) %>%
      mutate (pri_15_34_growth = (pri_15_34_lagged - pri_15_34)/pri_15_34) %>%
      mutate (pri_15_34_growth = Lag (pri_15_34_growth, 1)) %>%
      
      mutate (sec_15_34_lagged = Lag (sec_15_34, -1)) %>%
      mutate (sec_15_34_growth = (sec_15_34_lagged - sec_15_34)/sec_15_34) %>%
      mutate (sec_15_34_growth = Lag (sec_15_34_growth, 1)) %>%
      
      mutate (ter_15_34_lagged = Lag (ter_15_34, -1)) %>%
      mutate (ter_15_34_growth = (ter_15_34_lagged - ter_15_34)/ter_15_34) %>%
      mutate (ter_15_34_growth = Lag (ter_15_34_growth, 1)) %>%
      
      mutate (pri_35_64_lagged = Lag (pri_35_64, -1)) %>%
      mutate (pri_35_64_growth = (pri_35_64_lagged - pri_35_64)/pri_35_64) %>%
      mutate (pri_35_64_growth = Lag (pri_35_64_growth, 1)) %>%
      
      mutate (sec_35_64_lagged = Lag (sec_35_64, -1)) %>%
      mutate (sec_35_64_growth = (sec_35_64_lagged - sec_35_64)/sec_35_64) %>%
      mutate (sec_35_64_growth = Lag (sec_35_64_growth, 1)) %>%
      
      mutate (ter_35_64_lagged = Lag (ter_35_64, -1)) %>%
      mutate (ter_35_64_growth = (ter_35_64_lagged - ter_35_64)/ter_35_64) %>%
      mutate (ter_35_64_growth = Lag (ter_35_64_growth, 1))
    
    pri_15_34_growth_avg = mean (data1$pri_15_34_growth, na.rm = T)
    sec_15_34_growth_avg = mean (data1$sec_15_34_growth, na.rm = T)
    ter_15_34_growth_avg = mean (data1$ter_15_34_growth, na.rm = T)
    pri_35_64_growth_avg = mean (data1$pri_35_64_growth, na.rm = T)
    sec_35_64_growth_avg = mean (data1$sec_35_64_growth, na.rm = T)
    ter_35_64_growth_avg = mean (data1$ter_35_64_growth, na.rm = T)
    
    data1$pri_15_34 [data1$year == 2020] <- data1$pri_15_34 [data1$year == 2015] * (1 + pri_15_34_growth_avg)
    data1$sec_15_34 [data1$year == 2020] <- data1$sec_15_34 [data1$year == 2015] * (1 + sec_15_34_growth_avg)
    data1$ter_15_34 [data1$year == 2020] <- data1$ter_15_34 [data1$year == 2015] * (1 + ter_15_34_growth_avg)
    data1$pri_35_64 [data1$year == 2020] <- data1$pri_35_64 [data1$year == 2015] * (1 + pri_35_64_growth_avg)
    data1$sec_35_64 [data1$year == 2020] <- data1$sec_35_64 [data1$year == 2015] * (1 + sec_35_64_growth_avg)
    data1$ter_35_64 [data1$year == 2020] <- data1$ter_35_64 [data1$year == 2015] * (1 + ter_35_64_growth_avg)
    
    data1 = data1 %>% select (1:11)
    
    data2 = data1 %>% filter (year == 2020)
    data2 [,6:11] = data2 [,6:11] / sum (data2 [,6:11])
    
    data3 = data1 %>% filter (!(year == 2020))
    data4 = rbind (data3, data2)
    
    datalist[[i]] <- data4
  }
  
  big_data = do.call(rbind, datalist)
  
  .GlobalEnv$DIOC_2000_20 = big_data
  
}

interpol()






values = unique (DIOC_2000_20_1$orig_dest)

datalist = vector("list", length = length(unique(values)))

for (i in values) {
  
  data1 = DIOC_2000_20_1 [DIOC_2000_20_1$orig_dest == i,]
  data1 [data1 == 0] <- NA
  
  data1 = data1 %>%
    
    mutate (pri_15_34_lagged = Lag (pri_15_34, -1)) %>%
    mutate (pri_15_34_growth = (pri_15_34_lagged - pri_15_34)/pri_15_34) %>%
    mutate (pri_15_34_growth = Lag (pri_15_34_growth, 1)) %>%
    
    mutate (sec_15_34_lagged = Lag (sec_15_34, -1)) %>%
    mutate (sec_15_34_growth = (sec_15_34_lagged - sec_15_34)/sec_15_34) %>%
    mutate (sec_15_34_growth = Lag (sec_15_34_growth, 1)) %>%
    
    mutate (ter_15_34_lagged = Lag (ter_15_34, -1)) %>%
    mutate (ter_15_34_growth = (ter_15_34_lagged - ter_15_34)/ter_15_34) %>%
    mutate (ter_15_34_growth = Lag (ter_15_34_growth, 1)) %>%
    
    mutate (pri_35_64_lagged = Lag (pri_35_64, -1)) %>%
    mutate (pri_35_64_growth = (pri_35_64_lagged - pri_35_64)/pri_35_64) %>%
    mutate (pri_35_64_growth = Lag (pri_35_64_growth, 1)) %>%
    
    mutate (sec_35_64_lagged = Lag (sec_35_64, -1)) %>%
    mutate (sec_35_64_growth = (sec_35_64_lagged - sec_35_64)/sec_35_64) %>%
    mutate (sec_35_64_growth = Lag (sec_35_64_growth, 1)) %>%
    
    mutate (ter_35_64_lagged = Lag (ter_35_64, -1)) %>%
    mutate (ter_35_64_growth = (ter_35_64_lagged - ter_35_64)/ter_35_64) %>%
    mutate (ter_35_64_growth = Lag (ter_35_64_growth, 1))
  
  pri_15_34_growth_avg = mean (data1$pri_15_34_growth, na.rm = T)
  sec_15_34_growth_avg = mean (data1$sec_15_34_growth, na.rm = T)
  ter_15_34_growth_avg = mean (data1$ter_15_34_growth, na.rm = T)
  pri_35_64_growth_avg = mean (data1$pri_35_64_growth, na.rm = T)
  sec_35_64_growth_avg = mean (data1$sec_35_64_growth, na.rm = T)
  ter_35_64_growth_avg = mean (data1$ter_35_64_growth, na.rm = T)
  
  data1$pri_15_34 [data1$year == 2020] <- data1$pri_15_34 [data1$year == 2015] * (1 + pri_15_34_growth_avg)
  data1$sec_15_34 [data1$year == 2020] <- data1$sec_15_34 [data1$year == 2015] * (1 + sec_15_34_growth_avg)
  data1$ter_15_34 [data1$year == 2020] <- data1$ter_15_34 [data1$year == 2015] * (1 + ter_15_34_growth_avg)
  data1$pri_35_64 [data1$year == 2020] <- data1$pri_35_64 [data1$year == 2015] * (1 + pri_35_64_growth_avg)
  data1$sec_35_64 [data1$year == 2020] <- data1$sec_35_64 [data1$year == 2015] * (1 + sec_35_64_growth_avg)
  data1$ter_35_64 [data1$year == 2020] <- data1$ter_35_64 [data1$year == 2015] * (1 + ter_35_64_growth_avg)
  
  data1 = data1 %>% select (1:11)
  
  data2 = data1 %>% filter (year == 2020)
  data2 [,6:11] = data2 [,6:11] / sum (data2 [,6:11])
  
  data3 = data1 %>% filter (!(year == 2020))
  data4 = rbind (data3, data2)
  
  datalist[[i]] <- data4
  
}

big_data = do.call(rbind, datalist)


data1 = sample2
data1 [data1 == 0] <- NA

data1 = data1 %>%
  
  group_by (year, orig_dest, orig, region_birth, dest) %>%
  
  mutate (pri_15_34_lagged = Lag (pri_15_34, -1)) %>%
  mutate (pri_15_34_growth = (pri_15_34_lagged - pri_15_34)/pri_15_34) %>%
  mutate (pri_15_34_growth = Lag (pri_15_34_growth, 1)) %>%
  
  mutate (sec_15_34_lagged = Lag (sec_15_34, -1)) %>%
  mutate (sec_15_34_growth = (sec_15_34_lagged - sec_15_34)/sec_15_34) %>%
  mutate (sec_15_34_growth = Lag (sec_15_34_growth, 1)) %>%
  
  mutate (ter_15_34_lagged = Lag (ter_15_34, -1)) %>%
  mutate (ter_15_34_growth = (ter_15_34_lagged - ter_15_34)/ter_15_34) %>%
  mutate (ter_15_34_growth = Lag (ter_15_34_growth, 1)) %>%
  
  mutate (pri_35_64_lagged = Lag (pri_35_64, -1)) %>%
  mutate (pri_35_64_growth = (pri_35_64_lagged - pri_35_64)/pri_35_64) %>%
  mutate (pri_35_64_growth = Lag (pri_35_64_growth, 1)) %>%
  
  mutate (sec_35_64_lagged = Lag (sec_35_64, -1)) %>%
  mutate (sec_35_64_growth = (sec_35_64_lagged - sec_35_64)/sec_35_64) %>%
  mutate (sec_35_64_growth = Lag (sec_35_64_growth, 1)) %>%
  
  mutate (ter_35_64_lagged = Lag (ter_35_64, -1)) %>%
  mutate (ter_35_64_growth = (ter_35_64_lagged - ter_35_64)/ter_35_64) %>%
  mutate (ter_35_64_growth = Lag (ter_35_64_growth, 1)) %>%
  ungroup ()

pri_15_34_growth_avg = mean (data1$pri_15_34_growth, na.rm = T)
sec_15_34_growth_avg = mean (data1$sec_15_34_growth, na.rm = T)
ter_15_34_growth_avg = mean (data1$ter_15_34_growth, na.rm = T)
pri_35_64_growth_avg = mean (data1$pri_35_64_growth, na.rm = T)
sec_35_64_growth_avg = mean (data1$sec_35_64_growth, na.rm = T)
ter_35_64_growth_avg = mean (data1$ter_35_64_growth, na.rm = T)

data1$pri_15_34 [data1$year == 2020] <- data1$pri_15_34 [data1$year == 2015] * (1 + pri_15_34_growth_avg)
data1$sec_15_34 [data1$year == 2020] <- data1$sec_15_34 [data1$year == 2015] * (1 + sec_15_34_growth_avg)
data1$ter_15_34 [data1$year == 2020] <- data1$ter_15_34 [data1$year == 2015] * (1 + ter_15_34_growth_avg)
data1$pri_35_64 [data1$year == 2020] <- data1$pri_35_64 [data1$year == 2015] * (1 + pri_35_64_growth_avg)
data1$sec_35_64 [data1$year == 2020] <- data1$sec_35_64 [data1$year == 2015] * (1 + sec_35_64_growth_avg)
data1$ter_35_64 [data1$year == 2020] <- data1$ter_35_64 [data1$year == 2015] * (1 + ter_35_64_growth_avg)

data1 = data1 %>% select (1:11)

data2 = data1 %>% filter (year == 2020)
data2 [,6:11] = data2 [,6:11] / sum (data2 [,6:11])

data3 = data1 %>% filter (!(year == 2020))
data4 = rbind (data3, data2)








test = function (x, data) {
  
  data1 = sample2 [sample2$orig_dest == i]
  
  data1 = data1 %>%
    
    mutate (pri_15_34_lagged = Lag (pri_15_34, -1)) %>%
    mutate (pri_15_34_growth = (pri_15_34_lagged - pri_15_34)/pri_15_34) %>%
    mutate (pri_15_34_growth = Lag (pri_15_34_growth, 1)) %>%
    
    mutate (sec_15_34_lagged = Lag (sec_15_34, -1)) %>%
    mutate (sec_15_34_growth = (sec_15_34_lagged - sec_15_34)/sec_15_34) %>%
    mutate (sec_15_34_growth = Lag (sec_15_34_growth, 1)) %>%
    
    mutate (ter_15_34_lagged = Lag (ter_15_34, -1)) %>%
    mutate (ter_15_34_growth = (ter_15_34_lagged - ter_15_34)/ter_15_34) %>%
    mutate (ter_15_34_growth = Lag (ter_15_34_growth, 1)) %>%
    
    mutate (pri_35_64_lagged = Lag (pri_35_64, -1)) %>%
    mutate (pri_35_64_growth = (pri_35_64_lagged - pri_35_64)/pri_35_64) %>%
    mutate (pri_35_64_growth = Lag (pri_35_64_growth, 1)) %>%
    
    mutate (sec_35_64_lagged = Lag (sec_35_64, -1)) %>%
    mutate (sec_35_64_growth = (sec_35_64_lagged - sec_35_64)/sec_35_64) %>%
    mutate (sec_35_64_growth = Lag (sec_35_64_growth, 1)) %>%
    
    mutate (ter_35_64_lagged = Lag (ter_35_64, -1)) %>%
    mutate (ter_35_64_growth = (ter_35_64_lagged - ter_35_64)/ter_35_64) %>%
    mutate (ter_35_64_growth = Lag (ter_35_64_growth, 1))
  
  pri_15_34_growth_avg = mean (data1$pri_15_34_growth, na.rm = T)
  sec_15_34_growth_avg = mean (data1$sec_15_34_growth, na.rm = T)
  ter_15_34_growth_avg = mean (data1$ter_15_34_growth, na.rm = T)
  pri_35_64_growth_avg = mean (data1$pri_35_64_growth, na.rm = T)
  sec_35_64_growth_avg = mean (data1$sec_35_64_growth, na.rm = T)
  ter_35_64_growth_avg = mean (data1$ter_35_64_growth, na.rm = T)
  
  data1$pri_15_34 [data1$year == 2020] <- data1$pri_15_34 [data1$year == 2015] * (1 + pri_15_34_growth_avg)
  data1$sec_15_34 [data1$year == 2020] <- data1$sec_15_34 [data1$year == 2015] * (1 + sec_15_34_growth_avg)
  data1$ter_15_34 [data1$year == 2020] <- data1$ter_15_34 [data1$year == 2015] * (1 + ter_15_34_growth_avg)
  data1$pri_35_64 [data1$year == 2020] <- data1$pri_35_64 [data1$year == 2015] * (1 + pri_35_64_growth_avg)
  data1$sec_35_64 [data1$year == 2020] <- data1$sec_35_64 [data1$year == 2015] * (1 + sec_35_64_growth_avg)
  data1$ter_35_64 [data1$year == 2020] <- data1$ter_35_64 [data1$year == 2015] * (1 + ter_35_64_growth_avg)
  
  data1 = data1 %>% select (1:11)
  
  data2 = data1 %>% filter (year == 2020)
  data2 [,6:11] = data2 [,6:11] / sum (data2 [,6:11])
  
  data3 = data1 %>% filter (!(year == 2020))
  data4 = rbind (data3, data2)
  
  data = data4
  return (data)
  
}

test()









pri_15_34_growth_avg = mean (data$pri_15_34_growth, na.rm = T)

sample1$pri_15_34 [sample1$year == 2020] <- 
  (sample1$pri_15_34 [sample1$year == 2015] * pri_15_34_growth_avg) + 
  sample1$pri_15_34 [sample1$year == 2015]

sample1 = sample1 %>% select (-c("pri_15_34_lagged", "pri_15_34_growth"))












sample2 = DIOC_2000_20_1 %>% 
  filter (orig_dest %in% c("AFG_AUS", "ALB_TUR", "ECU_JPN")) %>% 
  group_by (year, orig_dest, orig, region_birth, dest) %>%
  mutate (pri_15_34_lagged = Lag (pri_15_34, -1),
          pri_15_34_growth = Lag(((pri_15_34_lagged - pri_15_34)/pri_15_34),1),
          pri_15_34_growth_avg = mean (pri_15_34_growth, na.rm = T))


#### TRY R-BINDING SOLUTION NEXT


















interpol = function () {
  
  data = sample2 %>% 
    
    group_by (year, orig_dest, orig, region_birth, dest) %>%
    
    mutate (pri_15_34_lagged = Lag (pri_15_34, -1)) %>%
    mutate (pri_15_34_growth = (pri_15_34_lagged - pri_15_34)/pri_15_34) %>%
    mutate (pri_15_34_growth = Lag (pri_15_34_growth, 1)) %>%
    
    mutate (sec_15_34_lagged = Lag (sec_15_34, -1)) %>%
    mutate (sec_15_34_growth = (sec_15_34_lagged - sec_15_34)/sec_15_34) %>%
    mutate (sec_15_34_growth = Lag (sec_15_34_growth, 1)) %>%
    
    mutate (ter_15_34_lagged = Lag (ter_15_34, -1)) %>%
    mutate (ter_15_34_growth = (ter_15_34_lagged - ter_15_34)/ter_15_34) %>%
    mutate (ter_15_34_growth = Lag (ter_15_34_growth, 1)) %>%
    
    mutate (pri_35_64_lagged = Lag (pri_35_64, -1)) %>%
    mutate (pri_35_64_growth = (pri_35_64_lagged - pri_35_64)/pri_35_64) %>%
    mutate (pri_35_64_growth = Lag (pri_35_64_growth, 1)) %>%
    
    mutate (sec_35_64_lagged = Lag (sec_35_64, -1)) %>%
    mutate (sec_35_64_growth = (sec_35_64_lagged - sec_35_64)/sec_35_64) %>%
    mutate (sec_35_64_growth = Lag (sec_35_64_growth, 1)) %>%
    
    mutate (ter_35_64_lagged = Lag (ter_35_64, -1)) %>%
    mutate (ter_35_64_growth = (ter_35_64_lagged - ter_35_64)/ter_35_64) %>%
    mutate (ter_35_64_growth = Lag (ter_35_64_growth, 1))
  
  pri_15_34_growth_avg = mean (data$pri_15_34_growth, na.rm = T)
  sec_15_34_growth_avg = mean (data$sec_15_34_growth, na.rm = T)
  ter_15_34_growth_avg = mean (data$ter_15_34_growth, na.rm = T)
  pri_35_64_growth_avg = mean (data$pri_35_64_growth, na.rm = T)
  sec_35_64_growth_avg = mean (data$sec_35_64_growth, na.rm = T)
  ter_35_64_growth_avg = mean (data$ter_35_64_growth, na.rm = T)
  
  data$pri_15_34 [data$year == 2020] <- (data$pri_15_34 [data$year == 2015] * pri_15_34_growth_avg) + data$pri_15_34 [data$year == 2015]
  data$sec_15_34 [data$year == 2020] <- (data$sec_15_34 [data$year == 2015] * sec_15_34_growth_avg) + data$sec_15_34 [data$year == 2015]
  data$ter_15_34 [data$year == 2020] <- (data$ter_15_34 [data$year == 2015] * ter_15_34_growth_avg) + data$ter_15_34 [data$year == 2015]
  data$pri_35_64 [data$year == 2020] <- (data$pri_35_64 [data$year == 2015] * pri_35_64_growth_avg) + data$pri_35_64 [data$year == 2015]
  data$sec_35_64 [data$year == 2020] <- (data$sec_35_64 [data$year == 2015] * sec_35_64_growth_avg) + data$sec_35_64 [data$year == 2015]
  data$ter_35_64 [data$year == 2020] <- (data$ter_35_64 [data$year == 2015] * ter_35_64_growth_avg) + data$ter_35_64 [data$year == 2015]
  
  .GlobalEnv$sample2 = data
  
}

interpol()













sample2 = DIOC_2000_20_1 %>% filter (orig_dest %in% c("AFG_AUS", "ALB_TUR", "ECU_JPN"))







sample3 = DIOC_2000_20_1 %>% filter (year == 2015)














