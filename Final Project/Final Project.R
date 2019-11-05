library(tidyr)
library(corrplot)
library(tidyverse)
library(dbplyr)
library(car)
#install.packages("fastDummies")
library(fastDummies)

##load .csv file

data_original <- read.csv("~/Desktop/Collaboration_MGB293/Final Project/AB_NYC_2019.csv")
str(data_original)




##create dummy variable for neighborhood
data


data_subset <- data_original%>%
  dplyr::select("neighbourhood_group", "neighbourhood","room_type")

data_dummy <- fastDummies::dummy_cols(data_subset)

data_other <- data_original%>%
  dplyr::select("price", "minimum_nights","number_of_reviews", "reviews_per_month", "calculated_host_listings_count", "availability_365")

data_combined <- cbind.data.frame(data_dummy,data_other)

data_combined <- drop_na(data_combined)

data_combined <- data_combined %>%
  dplyr::select(-"neighbourhood_group", -"neighbourhood", -"room_type")

reg_1 <- lm(price~., data = data_combined)
