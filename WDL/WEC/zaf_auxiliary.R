
# function to calc spatial distribution give input variables
spatial_dist <- function(input_vars, dat){
  
  # subset
  d <- subset(dat, subset = year %in% c(2017, 2018, 2019), select = c(c('year', 'Province'), input_vars))
  
  # mean of years
  d <- aggregate(d, by = list(d$Province), FUN = function(x) return(ifelse(is.numeric(x), mean(x),NaN)))
  
  # add information of input_vars
  if (length(input_vars) > 1) {
    
    tmp <- rowSums(d[, 4:ncol(d)])
    
  } else {
    
    tmp <- d[,4]
    
    
  }
  
  
  # extract relevant data
  d <- data.frame(province = d$Group.1, share = tmp/sum(tmp))
  
  # return
  return(d)
  
  
}



# function to calculate subnational emissions
emis2sub_national <- function(dat_emis_zaf, dat_dist, sec2dat) {
  
  # interators
  years <- unique(dat_emis_zaf$year)
  subsec <- unique(dat_emis_zaf$subsector)
  
  # output
  out <- c()
  
  
  # iterate over subsector
  for (s in subsec) {
    
    # select relevant data
    rel_dat <- strsplit(sec2dat[sec2dat$subsec == s, 'data'], '&')[[1]]
    
    # calculat distribution
    s_d <- spatial_dist(rel_dat, dat_dist)
    
    # iterate over year
    for (y in years) {
      
      # subset
      dat_emis_zaf_s_y <- subset(dat_emis_zaf, year == y & subsector == s)
      
      # distribute & summarize res in object
      tmp <- data.frame(year = y,
                        subsec = s,
                        prov = s_d$province,
                        ndc = dat_emis_zaf_s_y$ndc * s_d$share,
                        o_1p5c = dat_emis_zaf_s_y$o_1p5c * s_d$share,
                        base = dat_emis_zaf_s_y$base * s_d$share)
      
      # add to output
      out <- rbind(out, tmp)
      
      
    }
    
    
  }
  
  
  rel_dat <- strsplit(sec2dat[sec2dat$subsec == "CROPS", 'data'], '&')[[1]]
  s_d <- spatial_dist (rel_dat, dat_dist)
  
  
  # merge sector to output
  out <- merge(out, sec2dat[,-3], by = "subsec", all.x = T, all.y = T)
  
  # return
  return(out)
  
} 
