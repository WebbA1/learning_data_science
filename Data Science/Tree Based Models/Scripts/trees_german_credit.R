library(tidyverse)
library(caret)
# In the online example, 1 is good, while 0 is bad
data("GermanCredit")
str(GermanCredit)
# Use CV 
partition_index <- createDataPartition(GermanCredit$Class, 
                                       times = 1, 
                                       p = 0.75, 
                                       list = FALSE)
GermanCredit_training <- GermanCredit[partition_index, ]
GermanCredit_testing  <- GermanCredit[-partition_index, ]
# Use Stratified cross validation to equally distribute class
folds <- 5
cvIndex <- createFolds(factor(GermanCredit_training$Class), 
                       folds,
                       returnTrain = T)
trControl <- trainControl(method = "cv",
                          index = cvIndex,
                          number = folds)

# Create a tree diagram using the German credit training data ----
credit_rpart <- train(Class ~ ., 
                      data = GermanCredit_training,
                      trControl = trControl,
                      tuneLength = 20,
                      method = "rpart")
predicted_rpart_values <- predict(credit_rpart, 
                                  newdata = GermanCredit_testing)
confusionMatrix(predicted_rpart_values,
                GermanCredit_testing$Class, 
                positive = "Good")


# Create a random forest using the German credit training data ----
credit_random_forest <- train(Class ~ .,
                              data = GermanCredit_training,
                              trControl = trControl,
                              tuneLength = 20,
                              method = "ranger")
predicted_random_forest_values <- predict(credit_random_forest, 
                                          newdata = GermanCredit_testing)
confusionMatrix(predicted_random_forest_values,
                GermanCredit_testing$Class, 
                positive = "Good")

# Create a xgBoost using the German credit training data ----
credit_xgboost <- train(Class ~ .,
                        data = GermanCredit_training,
                        trControl = trControl,
                        tuneLength = 5,
                        method = "xgbLinear")
predicted_xgboost_values <- predict(credit_xgboost, 
                                          newdata = GermanCredit_testing)
confusionMatrix(predicted_xgboost_values,
                GermanCredit_testing$Class, 
                positive = "Good")
# Misclasification Rate - Just (1 - accuracy) in the confusion matrix 
mean(predicted_xgboost_values != GermanCredit_testing$Class)
# Create a AUC ROC Curve, it tells us how well our model distinguishes between classes.
# Area under the curve, Reciever Operating Characterisitcs
