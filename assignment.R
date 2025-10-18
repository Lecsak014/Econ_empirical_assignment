library(stargazer)
library(tidyverse)
library(ggthemes)
library(ggplot2)
library(lmtest)
library(car)
library(readxl)

# data in


df <- read_excel("2025.04.11-04.17.xlsx")

# rename columns, so it is easier to work with

colnames(df) <- c("date", "city", "company", "adress", "diesel", "gas")

df <- df |> 
  group_by(adress, city, company) |>
  summarise(diesel_avg = mean(diesel),
            gas_avg = mean(gas))



table(df$company)















