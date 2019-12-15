library(tidyverse)
library(rpart)
library(rpart.plot)
library(caret)
library(e1071)

# Load in the credit card fraud data (CCF)
data <- read_csv("Data Science/Examples/Credit Card Fraud/Data/creditcard.csv")
data$Class <- factor(data$Class)
# Create train and test partition
partition_index <- createDataPartition(y = data$Class, 
                                       times = 1, 
                                       p = 0.7, 
                                       list = FALSE)
ccf_data_training <- data[partition_index, ]
ccf_data_testing  <- data[-partition_index, ]

# Proportion of 1s in each
length(ccf_data_testing$Class[ccf_data_testing$Class == 1])/length(ccf_data_testing$Class) * 100
length(ccf_data_training$Class[ccf_data_training$Class == 1])/length(ccf_data_training$Class) * 100

# Create folds for the model training
folds <- 5
cvIndex <- createFolds(factor(ccf_data_training$Class), 
                       folds,
                       returnTrain = T)
trControl <- trainControl(method = "cv",
                          index = cvIndex,
                          number = folds)

# Create a tree diagram using the CCF training data ----
ccf_rpart <- train(Class ~ ., 
                   data = ccf_data_training,
                   trControl = trControl,
                   tuneLength = 20,
                   method = "rpart",
                   metric = "Kappa")
predicted_rpart_values <- predict(ccf_rpart, 
                                  newdata = ccf_data_testing)
confusionMatrix(predicted_rpart_values,
                ccf_data_testing$Class, 
                positive = "1")

# Create a random forest the CCF training data ----
ccf_rf <- train(Class ~ ., 
                data = ccf_data_training,
                trControl = trControl,
                tuneLength = 1,
                method = "ranger",
                metric = "Kappa")
predicted_rf_values <- predict(ccf_rf, 
                               newdata = ccf_data_testing)
confusionMatrix(predicted_rf_values,
                ccf_data_testing$Class, 
                positive = "1")

