
pacman::p_load(texreg,
               stargazer,
               tidyverse
               )

## FIVE-YEAR FLOWS MODEL ----------------------------------------------------------------------------------------

## removing all data points == 0 to allow logs

library(texreg)

input.1 = data.input5 %>% 
  filter (flows != 0 & 
            age0.64.orig != 0 &
            age0.64.dest != 0 &
            midclass.orig != 0 &
            midclass.dest != 0 &
            edu.orig != 0 &
            edu.dest != 0
          )

model.small = lm(formula = log (flows) ~ log (dist) 
                 + log (gdp.pc.orig) 
                 + log (gdp.pc.dest)
                 + log (population.orig)
                 + log (population.dest)
                 + as.factor (contig)
                 + as.factor (comlang.off)
                 + as.factor (year),
                 data = input.1
                 )

model.large = lm (formula = log (flows) ~ log (dist) 
                  + log (gdp.pc.orig) 
                  + log (gdp.pc.dest)
                  + log (population.orig)
                  + log (population.dest)
                  + log (age0.64.orig)
                  + log (age0.64.dest)
                  + log (midclass.orig)
                  + log (midclass.dest)
                  + log (edu.orig)
                  + log (edu.dest)
                  + as.factor (colony)
                  + as.factor (contig)
                  + as.factor (comlang.off)
                  + as.factor (year),
                  data = input.1
                  )

model.large.nonlogged = lm (formula = flows ~ dist 
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
                            data = input.1
                            )

model.small.nonlogged = lm (formula = flows ~ dist 
                            + gdp.pc.orig
                            + gdp.pc.dest 
                            + population.orig 
                            + population.dest
                            + as.factor(contig) 
                            + as.factor(comlang.off) 
                            + as.factor(year),
                            data = input.1
                            )

library("stargazer")

stargazer(model.small, 
          model.large, 
          model.small.nonlogged,
          model.large.nonlogged,
          type = "html",
          column.labels = c("model.small", "model.large", "model.small.nonlogged", "model.large.nonlogged"),
          out = "gravity_model_results_1.htm"
          )

## changing all data points == 0 to 0.0000000000000000001

input.2 = data.input5 

vars.input.2 = c("flows", "dist", "gdp.pc.orig", "gdp.pc.dest", "population.orig", "population.dest", 
                 "age0.64.orig", "age0.64.dest", "midclass.orig", "midclass.dest","edu.orig", "edu.dest")

for (i in vars.input.2) {
  input.2[,i] = ifelse (input.2[,i] == 0,
                        0.0000000000000000001,
                        input.2[,i])
}

model.small = lm(formula = log (flows) ~ log (dist) 
                 + log (gdp.pc.orig) 
                 + log (gdp.pc.dest)
                 + log (population.orig)
                 + log (population.dest)
                 + as.factor (contig)
                 + as.factor (comlang.off)
                 + as.factor (year),
                 data = input.2
                 )

model.large = lm (formula = log (flows) ~ log (dist) 
                  + log (gdp.pc.orig) 
                  + log (gdp.pc.dest)
                  + log (population.orig)
                  + log (population.dest)
                  + log (age0.64.orig)
                  + log (age0.64.dest)
                  + log (midclass.orig)
                  + log (midclass.dest)
                  + log (edu.orig)
                  + log (edu.dest)
                  + as.factor (colony)
                  + as.factor (contig)
                  + as.factor (comlang.off)
                  + as.factor (year),
                  data = input.2
                  )

model.large.nonlogged = lm (formula = flows ~ dist 
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
                            data = input.2
                            )

model.small.nonlogged = lm (formula = flows ~ dist 
                            + gdp.pc.orig
                            + gdp.pc.dest 
                            + population.orig 
                            + population.dest
                            + as.factor(contig) 
                            + as.factor(comlang.off) 
                            + as.factor(year),
                            data = input.2
                            )

library("stargazer")

stargazer(model.small, 
          model.large, 
          model.small.nonlogged,
          model.large.nonlogged,
          type = "html",
          column.labels = c("model.small", "model.large", "model.small.nonlogged", "model.large.nonlogged"),
          out = "gravity_model_results_2.htm"
          )

## taking the log (x + 1) of every variable that needs to be logged

input.3 = data.input5
vars.input.3 = c("flows", "dist", "gdp.pc.orig", "gdp.pc.dest", "population.orig", "population.dest", 
                 "age0.64.orig", "age0.64.dest", "midclass.orig", "midclass.dest","edu.orig", "edu.dest")

for (i in vars.input.3){
  input.3[,i] = log1p(input.3[,i])
  }

model.large = lm (formula = flows ~ dist 
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
                            data = input.3
                  )

model.small = lm (formula = flows ~ dist 
                            + gdp.pc.orig
                            + gdp.pc.dest 
                            + population.orig 
                            + population.dest
                            + as.factor(contig) 
                            + as.factor(comlang.off) 
                            + as.factor(year),
                            data = input.3
                  )

stargazer(model.small, 
          model.large, 
          type = "html",
          column.labels = c("model.small", "model.large"),
          out = "gravity_model_results_3.htm"
)

## ONE YEAR FLOWS MODEL  ------------------------------------------------------------------------------------

# taking log(x+1)
vars = c("flows", "dist", "population.orig", "gdp.orig", "gdp.pc.orig", "population.dest", "gdp.dest", "gdp.pc.dest")
for (i in vars){
  data.input1[,i] = log1p(data.input1[,i])
}
  
# running gravity model
model.yearly = lm (formula = flows ~ dist 
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
                   data = data.input1)

stargazer(model.yearly,
          type = "html",
          out = "gravity_model_results_4.htm"
          )



## MISCELLANEOUS ------------------------------------------------------------------

factorvars = c("year", "contig", "comlang_off", "colony", "col45", "landlocked_orig", "landlocked_dest")

for (i in factorvars) {
  input.5[,i] = as.factor(input.5[,i])
}








