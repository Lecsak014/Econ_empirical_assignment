library(stargazer)
library(tidyverse)
library(ggthemes)
library(ggplot2)
library(lmtest)
library(car)
library(readxl)

# data in

df <- read_excel("2025.04.11-04.17.xlsx")

table(df$Company)

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



















