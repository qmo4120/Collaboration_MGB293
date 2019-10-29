library(tidyr)
library(corrplot)
library(tidyverse)
library(dbplyr)

##load .csv file

data_original <- read.csv("~/Desktop/Collaboration_MGB293/Final Project/AB_NYC_2019.csv")
str(data_original)

data_original <- data_original%>%
  group_by(neighbourhood_group)

neiborhood_group <- unique(data_original$neighbourhood_group)
neiborhood <- unique(data_original$neighbourhood)



##create dummy variable for neighborhood
data_dummy <- data_original%>%
  mutate(X_Brooklyn = ifelse(neighbourhood_group == "Brooklyn", 1,0),
         X_Manhattan = ifelse(neighbourhood_group == "Manhattan", 1,0),
         X_Queens = ifelse(neighbourhood_group == "Queens", 1,0),
         X_Staten_island = ifelse(neighbourhood_group == "Staten Island", 1,0),
         X_Bronx = ifelse(neighbourhood_group =="Bronx", 1,0)
        )