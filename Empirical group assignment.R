###group projekt

library(stargazer)
library(tidyverse)
library(ggthemes)
library(ggplot2)
library(lmtest)
library(car)
library(readxl)
library(dplyr)
library(stringr)

#open data
df <- read_excel("2025.04.11-04.17.xlsx")

table(df$Company)
table(df$Settlement)

#creating weekly average
colnames(df) <- c("date", "city", "company", "adress", "diesel", "gas")
df <- df |> 
  group_by(adress, city, company) |>
  summarise(diesel_avg = mean(diesel),
            gas_avg = mean(gas))

#regressions
model1 <- lm( df$diesel_avg~ df$company ,data = df)
summary(model1)

#creating dummy for budapest and highway
df <- df %>%
  mutate(
    highway_dummy = as.numeric(str_detect(adress, "M[0-9]+")),
    budapest_dummy = as.numeric(str_detect(adress, "Budapest")))

model2 <- lm(df$diesel_avg ~ df$highway_dummy + df$budapest_dummy)
summary(model2)

#model 2 for gas
model20 <- lm(df$gas_avg ~ df$highway_dummy + df$budapest_dummy)
summary(model20)


# https://github.com/Lecsak014/Econ_empirical_assignment.git



### Factors: company, highway, location like esat or west, budapest or notû

#company, small comapanis group together

#get highway out of adress
df_highway <- df %>%
  filter(str_detect(adress,"M[0-9]+" ))
#budapest dummy
df_Budapest <- df %>%
  filter(str_detect(adress,"Budapest+" ))
### Regression models, interpretation, explanantory power, concluison

df <- df %>%
  mutate(
    highway_dummy = as.numeric(str_detect(adress, "M[0-9]+")),
    budapest_dummy = as.numeric(str_detect(adress, "Budapest")))

### figures

###difference between gas and diesel


