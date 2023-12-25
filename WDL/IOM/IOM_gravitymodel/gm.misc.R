
## fixing prelim. NA erasure method: take sample means

## FIVE YEAR FLOWS

# copying input data 
df_pred5 = data.input5 %>% drop_na()

# lagging flows
df_pred5$flows.lagged <- Lag(df_pred5$flows, -1)
df_pred5$flows.lagged = ifelse (df_pred5$year == 2020,
                                NA,
                                df_pred5$flows.lagged)

vars.input = c("flows", "dist", "gdp.pc.orig", "gdp.pc.dest", "population.orig", "population.dest", 
               "age0.64.orig", "age0.64.dest", "midclass.orig", "midclass.dest","edu.orig", "edu.dest", "flows.lagged")

# log(x+1) on all variables
for (i in vars.input){
  df_pred5[,i] = log1p(df_pred5[,i])
}

# split data into training and test sets
set.seed(123)
split = sample.split(df_pred5$flows, 
                     SplitRatio = 0.8, 
                     ) 

training.set = subset(df_pred5, split == TRUE)
test.set = subset(df_pred5, split == FALSE)

# running model
m5 = lm (formula = flows ~ dist 
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
                   + as.factor(year),
                   data = training.set
         )

# predictions
m5.pred.out = cbind(test.set[,c(1,2,3,4,7,11,25)],
                    predict(m5, test.set[,-7]))
names(m5.pred.out)[8] = "pred.flow"

#plot
flows5.out.lagged <- ggplot (m5.pred.out, aes(x = pred.flow, y = flows.lagged)) + 
  geom_point (fill = "#A08CDA", colour = "#A08CDA", shape = ".", size = 2) + 
  geom_smooth(method = "lm", colour = "#FCDC00", size = 0.5) + 
  labs (title = "Out of sample: predicted flows vs lagged 5-yr flows",
        x = "Predicted Flows",
        y = "Real Flows (lagged") + 
  theme(panel.background = element_rect(fill = "white", colour = "#0F1290", size = 1, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "white"),
        text = element_text(family = "Work Sans", size = 10),
        plot.title = element_text(family = "Work Sans Heavy", size = 18, face = "bold")) + 
  xlim (0,max(m5.pred.out$pred.flow)) + 
  ylim (0,max(m5.pred.out$flows)) +
  stat_cor(method = "pearson") 
flows5.out.lagged

## ONE YEAR FLOWS: generating out-sample predictions ------------------------------------------------------------

# copying input data
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

library("caTools")

# split data into training and test set
set.seed(123)
split = sample.split(df_pred1$flows, 
                     SplitRatio = 0.8, 
                     ) 

training.set = subset(df_pred1, split == TRUE)
test.set = subset(df_pred1, split == FALSE)

# running model
m1 = lm (formula = flows ~ dist 
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
                    data = training.set)

# out of sample predictions
m1.pred.out <- cbind(test.set[,1:7],
                     predict(m1, test.set[,-5]))
names(m1.pred.out)[8] = "pred.flow"

# plot
flows1.out.lagged <- ggplot (m1.pred.out, aes(x = pred.flow, y = flows.lagged)) + 
  geom_point (fill = "#A08CDA", colour = "#A08CDA", shape = ".", size = 2) + 
  geom_smooth(method = "lm", colour = "#FCDC00", size = 0.5) + 
  labs (title = "Out of sample: predicted flows vs lagged 1-yr flows",
        x = "Predicted Flows",
        y = "Real Flows (lagged") + 
  theme(panel.background = element_rect(fill = "white", colour = "#0F1290", size = 1, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "white"),
        text = element_text(family = "Work Sans", size = 10),
        plot.title = element_text(family = "Work Sans Heavy", size = 18, face = "bold")) + 
  xlim (0,max(m1.pred.out$pred.flow)) + 
  ylim (0,max(m1.pred.out$flows)) +
  stat_cor(method = "pearson") 
flows1.out.lagged





