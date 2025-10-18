library(stargazer)
library(tidyverse)
library(ggthemes)
library(ggplot2)
library(lmtest)
library(car)
library(readxl)

# data in


df <- read_excel("2025.04.11-04.17.xlsx")

<<<<<<< HEAD
# rename columns, so it is easier to work with
=======
table(df$Company)
>>>>>>> 4b096737ad26e0b8a9f5b0dc6d97a51a39f6d3c9

colnames(df) <- c("date", "city", "company", "adress", "diesel", "gas")

df <- df |> 
  group_by(adress, city, company) |>
  summarise(diesel_avg = mean(diesel),
            gas_avg = mean(gas))



<<<<<<< HEAD
table(df$company)
=======
>>>>>>> 4b096737ad26e0b8a9f5b0dc6d97a51a39f6d3c9















