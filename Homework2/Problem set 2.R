##Install packages
install.packages("tidyverse")
install.packages("dbplyr")

##load package
library(tidyr)
library(tidyverse)
library(dbplyr)

##load data set
data_original <-read.csv("~/Desktop/Collaboration_MGB293/Homework2/SteamDD.csv")

##explore data set
View(data_original)
glimpse(data_original)

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
         X_group_x_period_roleplay = X_group_x_period*X_roleplay
  )





  
##perform linear regression
##retention time
reg_age <- lm(data_dummy$age_week ~ data_dummy$X_group+ 
                                    data_dummy$X_period+ 
                                    data_dummy$X_group_x_period)

##unit sale
reg_unitsale <- lm(data_dummy$unit_sales ~ data_dummy$X_group+ 
                data_dummy$X_period+ 
                data_dummy$X_group_x_period)


##price
reg_price <- lm(data_dummy$price ~ data_dummy$X_group+ 
                  data_dummy$X_period+ 
                  data_dummy$X_group_x_period)

##part2
##retention time (with interaction with other variable)
reg_age_2 <- lm(data_dummy$age_week ~ data_dummy$X_group+ 
                data_dummy$X_period+
                data_dummy$X_strategy+
                data_dummy$X_action+
                data_dummy$X_roleplay+
                data_dummy$X_group_x_period+
                data_dummy$X_group_x_period_x_strategy+
                data_dummy$X_group_x_period_x_action+
                data_dummy$X_group_x_period_roleplay
                )

##unit sale
reg_unitsale_2 <- lm(data_dummy$unit_sales ~ data_dummy$X_group+ 
                     data_dummy$X_period+ 
                     data_dummy$X_strategy+
                     data_dummy$X_action+
                     data_dummy$X_roleplay+
                     data_dummy$X_group_x_period+
                     data_dummy$X_group_x_period_x_strategy+
                     data_dummy$X_group_x_period_x_action+
                     data_dummy$X_group_x_period_roleplay
                     )


##price
reg_price_2 <- lm(data_dummy$price ~ data_dummy$X_group+ 
                  data_dummy$X_period+
                  data_dummy$X_strategy+
                  data_dummy$X_action+
                  data_dummy$X_roleplay+
                  data_dummy$X_group_x_period+
                  data_dummy$X_group_x_period_x_strategy+
                  data_dummy$X_group_x_period_x_action+
                  data_dummy$X_group_x_period_roleplay
                  )


