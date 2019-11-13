library(dbplyr)
library(tidyr)


data <- read.csv("~/Desktop/Collaboration_MGB293/Homework3/demographics.csv")

data_dummy <- data %>%
  mutate(X_LA = ifelse(Region == "Los Angeles", 1,0),
         X_SB = ifelse(Region == "Santa Barbara", 1,0),
         X_SD = ifelse(Region == "San Diego", 1,0),
         X_FRE = ifelse(Region == "Fresno", 1,0),
         X_INC2 = ifelse(Income == "Hight", 1,0)
         )

intercept = c(A_LA = -0.7, A_SB = -0.2, A_SD = -0.5, A_FRE = -0.4, A_INC = 0.25)

price_discount = c(B_LA = -0.06, B_SB = -0.06, B_SD = -0.06, B_FRE = -0.07, B_INC = 0.02)

sqrt_price_discount = c(B_LA = -0.2, B_SB = -0.2, B_SD = -0.2, B_FRE = -0.25, B_INC = 0.1)