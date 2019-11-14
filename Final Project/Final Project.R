library(tidyr)
library(corrplot)
library(tidyverse)
library(dbplyr)
library(car)
#install.packages("fastDummies")
library(fastDummies)
library(MASS) ##for step-wise modeling

##load .csv file

data_original <- read.csv("~/Desktop/Collaboration_MGB293/Final Project/AB_NYC_2019.csv")
str(data_original)





data_subset <- data_original%>%
  dplyr::select("neighbourhood_group", "neighbourhood","room_type")

data_dummy <- fastDummies::dummy_cols(data_subset)

data_other <- data_original%>%
  dplyr::select("price", 
                "minimum_nights",
                "number_of_reviews", 
                "reviews_per_month", 
                "calculated_host_listings_count", 
                "availability_365")

data_combined <- cbind.data.frame(data_dummy,data_other)

data_combined <- drop_na(data_combined)

data_combined <- data_combined %>%
  mutate(X_hi_price=ifelse(price >= 101, 1,0))%>%
  dplyr::select(-"neighbourhood_group", -"neighbourhood", -"room_type",-"price")

##load package for machine learning

library(caTools) # contains function for splitting the data
library(caret) # contains function for creating a confusion matrix
library(e1071) # contains function for confusion matrix
library(pROC) ## contains function for ROC curve

str(data_combined)

# Split the data into training set (80%) and testing set (20%)

set.seed(123) # ensure reproducibility
#Sample.split() separate data set into ratio = TRUE:False. In this case
#in this case, 80% will be true and rest will be false
sample <- sample.split(data_combined, SplitRatio = .8)
train <- data_combined[sample == TRUE,]
test <- data_combined[sample == FALSE,]

##apply a logistic model
logreg <- glm(formula = X_hi_price ~ .,
              family = binomial(link = "logit"),
              data = train)

# Generate predictions using the `test` set and store in column called `prediction`

test <- test %>%
  mutate(prediction = predict(logreg, newdata = test, type = "response"))

## Apply a decision rule: if `prediction` > 0.5, change it to 1, else change it to 0

test <- test %>%
  mutate(prediction = ifelse(prediction > 0.5, 1, 0))

# Change `prediction` and `X_hi_price` columns to factors

test <- test %>%
  mutate(prediction = as.factor(prediction),
        X_hi_price = as.factor(X_hi_price))

##generate confusion matrix
confusion_matrix <- confusionMatrix(data = test$prediction, reference = test$X_hi_price, positive = NULL)
confusion_matrix


###  The test data is where this truly counts.
roc_curve <- plot(roc(train$X_hi_price, logreg$fitted.values))


step.model <- stepAIC(logreg, direction = "both", 
                      trace = FALSE)
summary(step.model)

