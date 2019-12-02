library(tidyverse)
library(caret)
library(rpart)
library(rpart.plot)
library(ranger)
library(xgboost)


# Keep 25% of the data for tetsing
set.seed(64736)
partition_index <- caret::createDataPartition(iris$Species, 
                                              times = 1, 
                                              p = 0.75,
                                              list = FALSE)
iris_train <- iris[partition_index, ]
iris_test <- iris[-partition_index, ]

# Train a tree model with the Iris data
ctrl_boot <- trainControl(method = "boot",
                          number = 999)
ctrl_cv <- trainControl(method = "cv", 
                        number = 5)

iris_rpart_model <- train(Species ~ ., 
                          data = iris_train, 
                          method = "rpart",
                          trControl = ctrl_cv, 
                          tuneLength = 20)
predicted_values_rpart <- predict(iris_rpart_model, 
                                  newdata = iris_test)
test_set_rpart <- data.frame(obs = iris_test$Species,
                             pred = predicted_values_rpart)
confusionMatrix(data = test_set_rpart$pred, 
                reference = test_set_rpart$obs)

# Train a random forest with the Iris data
iris_train_random_forest <- train(Species ~ .,
                                  data = iris_train,
                                  method = "ranger",
                                  trControl = ctrl_cv,
                                  tuneLength = 20)

predicted_values_random_forest <- predict(iris_train_random_forest, 
                                          newdata = iris_test)
test_set_rf <- data.frame(obs = iris_test$Species, 
                          pred = predicted_values_random_forest)
confusionMatrix(data = test_set_rf$pred, 
                reference = test_set_rf$obs)

# Train an XG Boost model
iris_train_xgboost <- train(Species ~ .,
                            data = iris_train,
                            method = "xgbLinear",
                            trControl = ctrl_cv,
                            tuneLength = 5)
predicted_values_xgboost<- predict(iris_train_xgboost, 
                                          newdata = iris_test)
test_set_xgboost <- tibble(obs = iris_test$Species,
                           pred = predicted_values_xgboost)
confusionMatrix(data = test_set_xgboost$pred,
                reference = test_set_xgboost$obs)
