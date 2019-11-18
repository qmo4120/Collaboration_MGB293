library(tidyr)
library(corrplot)
library(tidyverse)
library(dplyr)
library(MASS)

##load data
data_original <- read.csv("~/Desktop/Collaboration_MGB293/Homework4/DynamicPricing.csv")
data_original <- data_original %>%
  dplyr::select("Seats.Sold", "Weeks.Before","Month_of_Flight", "Price" )

##create dummy variable
data_dummy <- data_original %>%
  dplyr::mutate(week_5 = ifelse(Weeks.Before == 5, 1,0 ),
                week_4 = ifelse(Weeks.Before == 4, 1,0 ),
                week_3 = ifelse(Weeks.Before == 3, 1,0 ),
                week_2 = ifelse(Weeks.Before == 2, 1,0 ),
                week_1 = ifelse(Weeks.Before == 1, 1,0 ),
                month_Jan = ifelse(Month_of_Flight == "Jan", 1,0),
                month_Feb = ifelse(Month_of_Flight == "Feb", 1,0),
                month_Mar = ifelse(Month_of_Flight == "Mar", 1,0),
                month_Apr = ifelse(Month_of_Flight == "Apr", 1,0),
                month_May = ifelse(Month_of_Flight == "May", 1,0),
                month_Jun = ifelse(Month_of_Flight == "Jun", 1,0),
                month_Jul = ifelse(Month_of_Flight == "Jul", 1,0),
                month_Aug = ifelse(Month_of_Flight == "Aug", 1,0),
                month_Sep = ifelse(Month_of_Flight == "Sep", 1,0),
                month_Oct = ifelse(Month_of_Flight == "Oct", 1,0),
                month_Nov = ifelse(Month_of_Flight == "Nov", 1,0),
                month_Dec = ifelse(Month_of_Flight == "Dec", 1,0)
  )%>%
  dplyr::select(-"Weeks.Before", -"Month_of_Flight")
  


##visualize data 

data_cor <- cor(data_dummy)
corrplot(data_cor)
plot(data_dummy$Seats.Sold~data_dummy$Price)


########################################ModleOne#############################################




data_dummy_add_1 <- data_dummy %>%
  dplyr::mutate(price_x_week5 = Price*week_5,
                price_x_week4 = Price*week_4,
                price_x_week3 = Price*week_3,
                price_x_week2 = Price*week_2,
                price_x_week1 = Price*week_1,
                price_x_Jan = Price*month_Jan,
                price_x_Feb = Price*month_Feb,
                price_x_Mar = Price*month_Mar,
                price_x_Apr = Price*month_Apr,
                price_x_May = Price*month_May,
                price_x_Jun = Price*month_Jun,
                price_x_Jul = Price*month_Jul,
                price_x_Aug = Price*month_Aug,
                price_x_Sep = Price*month_Sep,
                price_x_Oct = Price*month_Oct,
                price_x_Nov = Price*month_Nov,
                price_x_Dec = Price*month_Dec
  )

reg_1 <- lm(Seats.Sold~., data = data_dummy_add_1)

step.model_1 <- stepAIC(reg_1, direction = "both", 
                        trace = FALSE)
summary(step.model_1)


########################################ModleTwo#############################################
##add dummy variable
data_dummy_add_2 <- data_dummy %>%
  dplyr::mutate(
    Jan_week1_price = month_Jan*week_1*Price,
    Jan_week2_price = month_Jan*week_2*Price,
    Jan_week3_price = month_Jan*week_3*Price,
    Jan_week4_price = month_Jan*week_4*Price,
    Jan_week5_price = month_Jan*week_5*Price,
    Feb_week1_price = month_Feb*week_1*Price,
    Feb_week2_price = month_Feb*week_2*Price,
    Feb_week3_price = month_Feb*week_3*Price,
    Feb_week4_price = month_Feb*week_4*Price,
    Feb_week5_price = month_Feb*week_5*Price,
    Mar_week1_price = month_Mar*week_1*Price,
    Mar_week2_price = month_Mar*week_2*Price,
    Mar_week3_price = month_Mar*week_3*Price,
    Mar_week4_price = month_Mar*week_4*Price,
    Mar_week5_price = month_Mar*week_5*Price,
    Apr_week1_price = month_Apr*week_1*Price,
    Apr_week2_price = month_Apr*week_2*Price,
    Apr_week3_price = month_Apr*week_3*Price,
    Apr_week4_price = month_Apr*week_4*Price,
    Apr_week5_price = month_Apr*week_5*Price,
    May_week1_price = month_May*week_1*Price,
    May_week2_price = month_May*week_2*Price,
    May_week3_price = month_May*week_3*Price,
    May_week4_price = month_May*week_4*Price,
    May_week5_price = month_May*week_5*Price,
    Jun_week1_price = month_Jun*week_1*Price,
    Jun_week2_price = month_Jun*week_2*Price,
    Jun_week3_price = month_Jun*week_3*Price,
    Jun_week4_price = month_Jun*week_4*Price,
    Jun_week5_price = month_Jun*week_5*Price,
    Jul_week1_price = month_Jul*week_1*Price,
    Jul_week2_price = month_Jul*week_2*Price,
    Jul_week3_price = month_Jul*week_3*Price,
    Jul_week4_price = month_Jul*week_4*Price,
    Jul_week5_price = month_Jul*week_5*Price,
    Aug_week1_price = month_Aug*week_1*Price,
    Aug_week2_price = month_Aug*week_2*Price,
    Aug_week3_price = month_Aug*week_3*Price,
    Aug_week4_price = month_Aug*week_4*Price,
    Aug_week5_price = month_Aug*week_5*Price,
    Sep_week1_price = month_Sep*week_1*Price,
    Sep_week2_price = month_Sep*week_2*Price,
    Sep_week3_price = month_Sep*week_3*Price,
    Sep_week4_price = month_Sep*week_4*Price,
    Sep_week5_price = month_Sep*week_5*Price,
    Oct_week1_price = month_Oct*week_1*Price,
    Oct_week2_price = month_Oct*week_2*Price,
    Oct_week3_price = month_Oct*week_3*Price,
    Oct_week4_price = month_Oct*week_4*Price,
    Oct_week5_price = month_Oct*week_5*Price,
    Nov_week1_price = month_Nov*week_1*Price,
    Nov_week2_price = month_Nov*week_2*Price,
    Nov_week3_price = month_Nov*week_3*Price,
    Nov_week4_price = month_Nov*week_4*Price,
    Nov_week5_price = month_Nov*week_5*Price,
    Dec_week1_price = month_Dec*week_1*Price,
    Dec_week2_price = month_Dec*week_2*Price,
    Dec_week3_price = month_Dec*week_3*Price,
    Dec_week4_price = month_Dec*week_4*Price,
    Dec_week5_price = month_Dec*week_5*Price
  )

reg_2 <- lm(Seats.Sold~., data = data_dummy_add_2)

step.model_2 <- stepAIC(reg_2, direction = "both", 
                        trace = FALSE)
summary(step.model_2)
