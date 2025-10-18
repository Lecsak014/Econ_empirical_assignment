###group projekt

library(stargazer)
library(tidyverse)
library(ggthemes)
library(ggplot2)
library(lmtest)
library(car)
library(readxl)

# data in
df <- read_excel("2025.04.11-04.17.xlsx")
table(df$company)

# rename columns, so it is easier to work with
colnames(df) <- c("date", "city", "company", "adress", "diesel", "gas")

# making the weekly average prices
df <- df |> 
  group_by(adress, city, company) |>
  summarise(diesel_avg = mean(diesel),
            gas_avg = mean(gas))

# making chategories based on the company

# data frame to help decide
count <- df |> group_by(company) |>
  summarise(n = n())
# we dont need it anymore
rm(count)

df$company_f <- "Other"
df$company_f[df$company == "Mol"] <- "Mol"
df$company_f[df$company == "Orlen"] <- "Orlen"
df$company_f[df$company == "Shell"] <- "Shell"
df$company_f[df$company == "Omv"] <- "Omv"
df$company_f <- relevel(as.factor(df$company_f), ref = "Other")
table(df$company_f)

# data for cars / people

car <- read_excel("Passenger cars per thousand capita_2024.xlsx")

car <- car[,c(2,7)]

colnames(car) <- c("city", "car_per_p")

# In the original dataframe some cities are misspelled

df[df$city == "Fűzesabony",]$city <- "Füzesabony"
df[df$city == "Pűspökladány",]$city <- "Püspökladány"
df[df$city == "Fűle",]$city <- "Füle"
df[df$city == "Fűlöpszállás",]$city <- "Fülöpszállás"
df[df$city == "Révfűlöp",]$city <- "Révfülöp"
df[df$city == "Sűkösd",]$city <- "Sükösd"
df[df$city == "Sűmeg",]$city <- "Sümeg"
df[df$city == "Tiszafűred",]$city <- "Tiszafüred"
df <- df[df$city != "Tolna-Mözs",]

# join the new data to the original

df<- left_join(df, car, by = "city")
summary(df$car_per_p)

df$car_per_p <- as.numeric(df$car_per_p)
summary(df$car_per_p)

# checking for NAs

df_na <- df[is.na(df$car_per_p), ]
table(df_na$city)

# It cant find Felcsút, probably because of the ","

df[df$city == "Felcsút",]$car_per_p
car[car$city == "Felcsút",]$car_per_p
df[df$city == "Felcsút",]$car_per_p <- 2023.3

df_na <- df[is.na(df$car_per_p), ]
table(df_na$city)
# Only Budapest left

car[car$city == "Budapest",]$car_per_p
df[is.na(df$car_per_p),]$car_per_p <- 429.2

df_na <- df[is.na(df$car_per_p), ]
table(df_na$city)
# finally...

rm(car)
rm(df_na)

#regressions

model1 <- lm( df$diesel_avg~ df$company_f ,data = df)
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

















