library(tidyr)
library(corrplot)
library(tidyverse)
library(dbplyr)
library(lattice)
library(dplyr)

##load data
data_original <- read.csv("~/Desktop/Collaboration_MGB293/Homework4/DynamicPricing.csv")
data_original <- data_original %>%
  dplyr::select("Seats.Sold", "Weeks.Before","Month_of_Flight", "Price" )

##create dummy variable
data_dummy <- data_original %>%
  mutate(week_5 = ifelse(Weeks.Before == 5, 1,0 ),
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
  select(-"Weeks.Before", -"Month_of_Flight")%>%
  mutate(price_x_week5 = Price*week_5,
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

##perform regression analysis
reg <- lm(Seats.Sold ~., data = data_dummy)
