install.packages("Hmisc")
library(Hmisc)

## prediction for five-year flow model (using logs)  ---------------------------------------------------------------------------------------------------------------

# create data set for predictions
df_pred5 = data.input5 %>% # code for data.input5 found in gravity.model.input.final.R
  rename (year0 = year) %>%
  drop_na()

# lag flows by 5 years
df_pred5$flows.lagged <- Lag(df_pred5$flows, -1)

# as the df is grouped so that every country-pair has 5 obsv, I remove every 2020 obsv as otherwise it carries through the first obsv from the next country pairing
df_pred5$flows.lagged = ifelse (df_pred5$year0 == 2020,
                                NA,
                                df_pred5$flows.lagged) 

vars.input = c("flows", "dist", "gdp.pc.orig", "gdp.pc.dest", "population.orig", "population.dest", 
               "age0.64.orig", "age0.64.dest", "midclass.orig", "midclass.dest","edu.orig", "edu.dest", "flows.lagged")

# log(x+1) on all variables
for (i in vars.input){
  df_pred5[,i] = log1p(df_pred5[,i])
}

# convert year to numeric
df_pred5$year0 = as.numeric(df_pred5$year0)

# run large GM
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
                   data = df_pred5
)

# 25 = lagged flows
# 6 = dest.code but now it's pred flow

# make predictions
df_pred5 = cbind(df_pred5[,c(3, 2, 4, 7, 25)], predict(linear_first, df_pred5[,-7]))
names(df_pred5)[6] = "pred_flow"

# reverse logs
df_ppred5rev = df_pred5
df_ppred5rev[,4:6] = sapply(df_ppred5rev[,4:6], exp)

# reverse (x+1)
df_ppred5rev[,4:6] = df_ppred5rev[,4:6] - 1

# round results
df_ppred5rev[,6] = sapply(df_ppred5rev[,6], round)

# add distance back
data.input5 = data.input5 %>% drop_na() 

df_ppred5rev$dist = data.input5$dist
df_ppred5rev = df_ppred5rev %>% 
  relocate (dist, .before = year0) %>%
  rename_with(
    ~ case_when(
      . == "year0" ~ "year",
      TRUE ~ .
    )
  )

# export table
write.csv(df_ppred5rev, "gm.pred.fiveyearflows.csv")

## prediction for one-year flow model (using logs) ---------------------------------------------------------------------------------------------------------------

## data input

df_pred1 = data.input1 %>% drop_na()

# lagging flows
df_pred1$flows.lagged = Lag(df_pred1$flows, -1)
df_pred1$flows.lagged = ifelse (df_pred1$year == 2019,
                                NA,
                                df_pred1$flows.lagged) 
df_pred1 = df_pred1 %>% relocate (flows.lagged, .before = dist)

# taking log(x+1)
vars = c("flows", "flows.lagged", "dist", "population.orig", "gdp.orig", "gdp.pc.orig", "population.dest", "gdp.dest", "gdp.pc.dest")
for (i in vars){
  df_pred1[,i] = log1p(df_pred1[,i])
}

# running model
linear_second = lm (formula = flows ~ dist 
                    + population.orig 
                    + population.dest
                    + gdp.orig 
                    + gdp.dest
                    + gdp.pc.orig
                    + gdp.pc.dest
                    + as.factor(year)
                    + as.factor(contig)
                    + as.factor(comlang_off)
                    + as.factor(colony)
                    + as.factor(col45)
                    + as.factor(landlocked_orig)
                    + as.factor(landlocked_dest), 
                    data = df_pred1)

# predictions

df_pred1 = cbind(df_pred1[,1:7], predict(linear_second, df_pred1[,-5]))
names(df_pred1)[8] = "pred.flow"

# reverse logs
df_pred1rev = df_pred1
df_pred1rev[,5:8] = sapply(df_pred1rev[,5:8], exp)

# reverse (x+1)
df_pred1rev[,5:8] = df_pred1rev[,5:8] - 1

# round results
df_pred1rev[,8] = sapply(df_pred1rev[,8], round)

write.csv(df_pred1rev, "gm.pred.oneyearflows.csv")

## plots  ---------------------------------------------------------------------------------------------------------------

# scatter plots

install.packages("showtext")
library(showtext)
font_add("Work Sans", "CopyOfWorkSans-VariableFont_wght.ttf")
showtext_auto()
font_add("Work Sans Heavy", "WorkSansHeavy.ttf")
showtext_auto()

# five-year flows (logs)

png(filename = "fiveyearflowsplot")
fiveyearflowsplot <- ggplot (df_pred5, aes(x = pred_flow, y = flows)) + 
  geom_point (fill = "#A08CDA", colour = "#A08CDA", shape = ".", size = 2) + 
  geom_smooth(method = "lm", colour = "#0F1290", size = 0.5, linetype = "dashed") + 
  labs (title = "Real vs. Pred flows (Five Year Flows)",
        x = "Predicted Flows",
        y = "Real Flows") + 
  theme(panel.background = element_rect(fill = "white", colour = "#0F1290", size = 1, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "white"),
        text = element_text(family = "Work Sans", size = 10),
        plot.title = element_text(family = "Work Sans Heavy", size = 18, face = "bold")) + 
  xlim (0,max(df_pred5$pred_flow)) + 
  ylim (0,max(df_pred5$flows)) +
  stat_cor(method = "pearson") + 
  stat_function(fun = fit1, geom = "line", colour = "#FCDC00")
fiveyearflowsplot
dev.off()

# predicted flows vs. lagged flows (the one that Juan got excited about)

fiveyearflowsplot.lagged <- ggplot (df_pred5, aes(x = pred_flow, y = flows.lagged)) + 
  geom_point (fill = "#A08CDA", colour = "#A08CDA", shape = ".", size = 2) + 
  geom_smooth(method = "lm", colour = "#FCDC00", size = 0.5) + 
  labs (title = "Lagged Flows vs. Pred flows (Five Year Flows)",
        x = "Predicted Flows",
        y = "Real Flows (lagged") + 
  theme(panel.background = element_rect(fill = "white", colour = "#0F1290", size = 1, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "white"),
        text = element_text(family = "Work Sans", size = 10),
        plot.title = element_text(family = "Work Sans Heavy", size = 18, face = "bold")) + 
  xlim (0,max(df_pred5$pred_flow)) + 
  ylim (0,max(df_pred5$flows)) +
  stat_cor(method = "pearson") 
fiveyearflowsplot.lagged

write.csv(df_pred5, "df_pred5.csv")

# one-year flows (logs)

oneyearflowsplot <- ggplot (df_pred1, aes(x = pred.flow, y = flows)) + 
  geom_point (fill = "#A08CDA", colour = "#A08CDA", shape = ".", size = 2) + 
  geom_smooth(method = "lm", colour = "#0F1290", size = 0.5, linetype = "dashed") + 
  labs (title = "Real vs. Pred flows (One Year Flows)",
        x = "Predicted Flows",
        y = "Real Flows") + 
  theme(panel.background = element_rect(fill = "white", colour = "#0F1290", size = 1, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "white"),
        text = element_text(family = "Work Sans", size = 10),
        plot.title = element_text(family = "Work Sans Heavy", size = 18, face = "bold")) + 
  xlim (0,max(df_pred1$pred.flow)) + 
  ylim (0,max(df_pred1$flows)) +
  stat_cor(method = "pearson")