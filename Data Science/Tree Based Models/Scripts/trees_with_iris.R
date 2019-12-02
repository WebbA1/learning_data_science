library(tidyverse)
library(caret)
library(rpart)
library(rpart.plot)

# Train a tree model with the Iris data
ctrl_boot <- trainControl(method = "boot",
                          number = 999)
ctrl_cv <- trainControl(method = "cv", 
                        number = 20)

iris_rpart_model <- train(Species ~ ., 
                          data = iris, 
                          method = "rpart",
                          trControl = ctrl_cv, 
                          tuneLength = 20)
iris_rpart_model
iris_rpart_model$results
final_model <- rpart(Species ~ ., 
                     data = iris,
                     cp = iris_rpart_model$bestTune[[1]])
rpart.plot(final_model)

# Train a random forest with the Iris data
library(ranger)
iris_train_random_forest <- train(Species ~ .,
                                  data = iris,
                                  method = "ranger",
                                  trControl = ctrl_cv,
                                  tunelength = 20)