###group projekt

library(stargazer)
library(tidyverse)
library(ggthemes)
library(ggplot2)
library(lmtest)
library(car)
library(readxl)


#######################################
# Data importing and variable making #
#####################################

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

# data for cars / 1000 people

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
# getting the data manually from the data frame
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

#creating dummy for budapest and highway
df <- df %>%
  mutate(
    highway_dummy = as.numeric(str_detect(adress, "M[0-9]+")),
    budapest_dummy = as.numeric(str_detect(adress, "Budapest")))

# Adding data with the income for every county

dgh <- read_excel("dgh_download_2025.xlsx")
cty <- read_excel("stadat-mun0192-20.1.2.7-en.xlsx")

dgh <- dgh[c(-1, -2),]
dgh <- dgh[,c(1,4)]
colnames(dgh) <- c("city", "county")
cty <- cty[,c(1,3)]
colnames(cty) <- c("county", "avg_wage")
dgh <- left_join(dgh, cty, by = "county")
dgh <- dgh[,c(1,3)]

df <- left_join(df, dgh, by ="city")

summary(df$avg_wage)

df_na <- df[is.na(df$avg_wage), ]
table(df_na$city)
# it's the districts again

# getting the data from the cty data frame
df[is.na(df$avg_wage),]$avg_wage <- 782459

df_na <- df[is.na(df$avg_wage), ]
table(df_na$city)

rm(df_na)
rm(cty)
rm(dgh)

#######################
# Data visualization #
#####################


# UNDER CONSTRUCTION


#################
### MODELING ###
################

### model 1: only company

# diesel
model1_d <- lm(diesel_avg~ company_f ,data = df)
summary(model1_d)

# gas
model1_g <- lm(gas_avg~ company_f ,data = df)
summary(model1_g)

### model 2: adding highway and budapest

# diesel
model2_d <- lm(diesel_avg ~ company_f + highway_dummy + budapest_dummy, data = df)
summary(model2_d)

# gas
model2_g <- lm(gas_avg ~ company_f + highway_dummy + budapest_dummy, data = df)
summary(model2_g)


### model 3: adding the car / 1000 people


# diesel
model3_d <- lm(diesel_avg ~ company_f + highway_dummy + budapest_dummy + car_per_p, data = df)
summary(model3_d)

# gas
model3_g <- lm(gas_avg ~ company_f + highway_dummy + budapest_dummy + car_per_p, data = df)
summary(model3_g)


### model 4: adding the average wage of the county 


# diesel
model4_d <- lm(diesel_avg ~ company_f + highway_dummy + budapest_dummy + car_per_p + avg_wage, data = df)
summary(model4_d)

# gas
model4_g <- lm(gas_avg ~ company_f + highway_dummy + budapest_dummy + car_per_p + avg_wage, data = df)
summary(model4_g)



### Model selection ####

anova(model2_d, model3_d, model4_d)

AIC(model2_d, model3_d, model4_d)
BIC(model2_d, model3_d, model4_d)

anova(model2_g, model3_g, model4_g)

AIC(model2_g, model3_g, model4_g)
BIC(model2_g, model3_g, model4_g)





