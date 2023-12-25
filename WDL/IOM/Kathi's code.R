
install.packages("Hmisc")
library(Hmisc)

### prediction

# create data set for predictions

df_pred = data.exp.full %>%
  rename (year0 = year) %>%
  drop_na()


df_pred$flows.lagged <- Lag(df_pred$flows, -1)

vars.input = c("flows", "dist", "gdp.pc.orig", "gdp.pc.dest", "population.orig", "population.dest", 
                "age0.64.orig", "age0.64.dest", "midclass.orig", "midclass.dest","edu.orig", "edu.dest", "flows.lagged")

for (i in vars.input){
  df_pred[,i] = log1p(df_pred[,i])
}

df_pred$year0 = as.numeric(df_pred$year0)

linear_first = lm (formula = flows ~ dist 
                  + gdp.pc.orig
                  + gdp.pc.dest 
                  + population.orig 
                  + population.dest
                  + age0.64.orig 
                  + age0.64.dest 
                  + midclass.orig 
                  + midclass.dest 
                  + edu.orig 
                  + edu.dest 
                  + as.factor (colony)
                  + as.factor(contig) 
                  + as.factor(comlang.off) 
                  + as.factor(year0),
                  data = df_pred
                  )

# 25 = lagged flows
# 6 = dest.code but now it's pred flow

# make predictions
df_pred = cbind(df_pred[,c(3, 2, 4, 7, 25)], predict(linear_first, df_pred[,-7]))
names(df_pred)[6] = "pred_flow"
df_pred[,4:6] = sapply(df_pred[,4:6], exp)

df_pred[,6] = sapply(df_pred[,6], round)

write.csv(df_pred, "gravity.model.predictions.csv")

## notes for tomorrow: reverse logs, reverse (x+1), add fertility and unemployment data.























# take exponent to reverse logs
df_pred[,4:] = sapply(df_pred[,4:5], exp)

# round results
df_pred[,4:5] = sapply(df_pred[,4:5], round)

# set 1 to 0 in flow and predictions
df_pred$flow[df_pred$flow==1] = 0
df_pred$pred_flow[df_pred$pred_flow==1] = 0

# add predicted flows as flows in 2020-2025
df_pred$flow[df_pred$year0==2020] = df_pred$pred_flow[df_pred$year0==2020]

# divide flows by 5 to get average annual numbers
df_pred$flow = df_pred$flow/5

#### get annual flows for 2021 to 2025 based on gdp growth

# get sum of migration flows by destination
df_sum = data.table(df_pred)[, .(flow = sum(flow, na.rm = TRUE)), by = .(dest, year0)]

# compute shares of countries of origin in total migration after 2020
df_share = data.table(df_pred)[year0==2020]
df_share = df_share[, ":="(flow_share = flow/sum(flow, na.rm = TRUE)), by = .(dest)]

# prepare gdp data
df_gdp = data.table(mpro)[, .(ccode, year, GDP.PC.PPP)]
# df_gdp = df_gdp[ccode%in%c("DEU", "PRT") & year>1999 & year<2026]
df_gdp = df_gdp[year>1999 & year<2026]
names(df_gdp) = c("dest", "year", "gdp_pc")

# compute gdp growth
df_gdp_copy = copy(df_gdp)
df_gdp_copy$year = df_gdp_copy$year+1
names(df_gdp_copy) = c("dest", "year", "gdp_prev")
df_gdp = merge(df_gdp, df_gdp_copy, by = c("dest", "year"))
df_gdp$gdp_grw = df_gdp$gdp_pc/df_gdp$gdp_prev-1

# compute average gdp growth of 5-year periods
df_gdp$year0 = (df_gdp$year-1)-((df_gdp$year-1)%%5)
df_gdp_avg = copy(df_gdp)[, .(gdp_grw = mean(gdp_grw, na.rm = TRUE)), by = .(dest, year0)]

# combine gdp and migrant flow data
mig_gdp = merge(df_sum, df_gdp_avg, by = c("dest", "year0"))

# create country dummies
for (i in unique(mig_gdp$dest)) {
  mig_gdp[, paste0("dest_", i)] = 0
  mig_gdp[mig_gdp$dest==i, paste0("dest_", i)] = 1
}

# regress migrant flow on GDP growth
mig_gdp$flow[mig_gdp$flow==0] = 1
linear_gdp = lm(log(flow) ~ ., data = mig_gdp[,3:ncol(mig_gdp)])

# get annual gdp growth for 2021 to 2025
gdp_21_25 = copy(df_gdp)[year>2020]
gdp_21_25 = gdp_21_25[, .(dest, year, gdp_grw)]

# create country dummies
for (i in unique(gdp_21_25$dest)) {
  gdp_21_25[, paste0("dest_", i)] = 0
  gdp_21_25[gdp_21_25$dest==i, paste0("dest_", i)] = 1
}

# predict annual migration flows
gdp_21_25 = cbind(gdp_21_25[,1:2], predict(linear_gdp, gdp_21_25[,3:ncol(gdp_21_25)]))
gdp_21_25 = rename(gdp_21_25, flow_gdp = V2)
gdp_21_25$flow_gdp = exp(gdp_21_25$flow_gdp)

# create data frame with total flows from 2021 to 2025
flow_21_25 = copy(df_sum)[year0==2020]
flow_21_25$flow_sum = flow_21_25$flow*5

# combine total flows with distribution of flows over years
flow_21_25 = merge(gdp_21_25, flow_21_25, by = "dest")

# rescale distribution of flows to match total flows from 2021 to 2025
flow_21_25 = flow_21_25[, ":="(flow = round(flow_gdp*(flow_sum/sum(flow_gdp)))), by = .(dest)]

# check result for germany and portugal
test = copy(flow_21_25)[dest%in%c("DEU", "PRT")]
ggplot() +
  geom_col(test, mapping = aes(x = year, y = flow, fill = dest), position = "dodge") +
  labs(x = "", y = "Number of migrants") +
  scale_fill_manual(values = c("#4daf4a", "#377eb8")) +
  theme_bw()

# select columns and combine annual flows with shares of countries of origin
df_share = df_share[, .(dest, orig, flow_share)]
flow_21_25 = flow_21_25[, .(dest, year, flow)]
flow_21_25 = rename(flow_21_25, year0 = year)
flow_21_25 = merge(df_share, flow_21_25, by = "dest", allow.cartesian = TRUE)

# get flows by country of origin and year
flow_21_25$flow = flow_21_25$flow*flow_21_25$flow_share

# select columns and years
flow_21_25 = flow_21_25[, .(dest, orig, year0, flow)]
df_pred = data.table(df_pred)[, .(dest, orig, year0, flow)]
df_pred = df_pred[year0!=2020]

# combine annual predictions for 2021-2025 with 5-year period predictions
pred_comb = rbind(df_pred, flow_21_25)

# round migration flows
pred_comb$flow = round(pred_comb$flow)

# save results
# write.csv(pred_comb, file.path(input_path, "data","gravity_prediction_v3.csv"), row.names = FALSE)

# check result for germany, portugal and moldova
test = copy(pred_comb)[dest%in%c("DEU", "PRT") & orig=="MDA"]
ggplot() +
  geom_col(test, mapping = aes(x = as.character(year0), y = flow, fill = dest), position = "dodge") +
  labs(x = "", y = "Number of migrants") +
  scale_fill_manual(values = c("#4daf4a", "#377eb8")) +
  theme_bw()