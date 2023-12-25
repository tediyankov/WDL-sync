
## checking weird stuff

drc_shares = drc_hh_clean %>%
  pivot_wider (names_from = urban, values_from = wgt_pctl) %>%
  mutate (urban_share = Urban / (Urban + Rural),
          rural_share = Rural / (Urban + Rural)) %>%
  dplyr::select (percentile, urban_share, rural_share)

# LOESS (local) regression models 
model_urban <- loess (urban_share ~ percentile, data = dat_shares, span = 0.10)
model_rural <- loess (rural_share ~ percentile, data = dat_shares, span = 0.10)

# creating df with predicted values for specified ranges
out = data.frame (percentile = c(dat_shares$percentile, wdp_clean$cs [wdp_clean$ccode == country])) %>% 
  arrange (percentile) %>%
  mutate (ccode = country) %>%
  relocate (ccode, .before = percentile)
out$urban <- predict (model_urban, newdata = out)
out$rural <- predict (model_rural, newdata = out)
