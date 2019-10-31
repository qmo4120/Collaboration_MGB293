

##load package
library(tidyr)
library(tidyverse)
library(dbplyr)

##load data set
data_original <-read.csv("~/Desktop/Collaboration_MGB293/Homework2/SteamDD.csv")

##explore data set
View(data_original)
str(data_original)

##change key countinuous variables to numeric

data_original$age_week <- as.numeric(data_original$age_week)
data_original$price <- as.numeric(data_original$price)
data_original$unit_sales <- as.numeric(gsub(",","", data_original$unit_sales))

##explore data set again
View(data_original)
glimpse(data_original)

##create data set with dummy variables

data_dummy <- data_original %>%
  mutate(X_strategy = ifelse(genre =="Strategy",1,0),
         X_action = ifelse(genre =="Action",1,0),
         X_roleplay = ifelse(genre == "Role-Playing",1,0),
         X_group = ifelse(promotion == "Yes",1,0),
         X_period = ifelse(week == "week2", 1,0),
         group = ifelse(promotion == "Yes", "treatment","control"),
         period = ifelse(week == "week2", "treatment","control"),
         X_group_x_period = X_group * X_period,
         X_group_x_period_x_strategy = X_group_x_period*X_strategy,
         X_group_x_period_x_action = X_group_x_period*X_action,
         X_group_x_period_x_roleplay = X_group_x_period*X_roleplay,
         X_lo_price = ifelse(price <= 22.99, 1,0),
         X_hi_price = ifelse(price > 22.99, 1,0),
         X_lo_age = ifelse(age_week <= 14, 1,0),
         X_hi_age = ifelse(age_week > 14, 1,0),
         X_group_x_period_roleplay_x_lo_price = X_group_x_period*X_roleplay*X_lo_price,
         X_group_x_period_action_x_lo_price = X_group_x_period*X_action*X_lo_price,
         X_group_x_period_strategy_x_lo_price = X_group_x_period*X_strategy*X_lo_price,
         X_group_x_period_roleplay_x_hi_price = X_group_x_period*X_roleplay*X_hi_price,
         X_group_x_period_action_x_hi_price = X_group_x_period*X_action*X_hi_price,
         X_group_x_period_strategy_x_hi_price = X_group_x_period*X_strategy*X_hi_price,
         X_group_x_period_roleplay_x_lo_age = X_group_x_period*X_roleplay*X_lo_age,
         X_group_x_period_action_x_lo_age = X_group_x_period*X_action*X_lo_age,
         X_group_x_period_strategy_x_lo_age = X_group_x_period*X_strategy*X_lo_age,
         X_group_x_period_roleplay_x_hi_age = X_group_x_period*X_roleplay*X_hi_age,
         X_group_x_period_action_x_hi_age = X_group_x_period*X_action*X_hi_age,
         X_group_x_period_strategy_x_hi_age = X_group_x_period*X_strategy*X_hi_age,
         X_group_x_period_x_lo_age = X_group*X_period*X_lo_age,
         X_group_x_period_x_lo_price = X_group*X_period*X_lo_price,
         X_group_x_X_price = X_group*X_lo_price,
         X_group_x_X_age = X_group*X_lo_age
         )


View(data_dummy)





  
##perform linear regression
##retention time


##unit sale
reg_unitsale <- lm(data_dummy$unit_sales ~ data_dummy$X_group+ 
                data_dummy$X_period+ 
                data_dummy$X_group_x_period)


##part2

data_dummy_only <- data_dummy %>%
  dplyr::select(unit_sales,
                X_strategy,
                X_roleplay,
                X_group,
                X_period,
                X_lo_price,
                X_group_x_period,
                X_lo_age,
                X_group_x_period_x_lo_age,
                X_group_x_period_x_lo_price,
                X_group_x_period_x_strategy,
                X_group_x_period_x_roleplay,
                X_group_x_X_price,
                X_group_x_X_age
  )

##find out any multicolinearity among variables
library(corrplot)
data_dummy_cor <- cor(data_dummy_only)
corrplot(data_dummy_cor)

##unit sale
reg_unitsale_2 <- lm(unit_sales ~ ., data = data_dummy_only)







