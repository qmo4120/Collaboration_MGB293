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

data_dummy_add_2 <- data_dummy %>%
  dplyr::mutate(price_x_week5 = Price*week_5,
                price_x_week4 = Price*week_4,
                price_x_week3 = Price*week_3,
                price_x_week2 = Price*week_2,
                price_x_week1 = Price*week_1
               
                )

reg_2 <- lm(Seats.Sold~price_x_week5+
              price_x_week4+
              price_x_week3+
              price_x_week2+
              price_x_week1+
              week_5+
              week_4+
              week_3+
              week_2+
              week_1,
              data = data_dummy_add_2)
summary(reg_2)

seat_sold <- function(p5,p4,p3,p2,p1) (46.748489+(-0.214206*p5)+70.483956)+(46.748489+(-0.216287*p4)+
                                    85.130350)+(46.748489+(-0.193156*p3)+70.408310)+(46.748489+(-0.164923*p2)+48.274968)+(46.748489+(-0.085006*p1))
optim(c(366,373,404,417,374), fn=seat_sold, method = "BFGS")

