setwd("C:/Users/kissl/Desktop/Corvinus/Öko")

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

weekly_avg <- df %>%
  group_by(Station_ID, Fuel_Type) %>%     # group by station (and optionally fuel)
  summarise(
    avg_price = mean(Price, na.rm = TRUE) # compute mean price
  ) %>%
  ungroup()

























