# Sourcing & Library
source("./Kaggle/House Price Estimates/Scripts/Data/import_data.R")
source("./Kaggle/House Price Estimates/Scripts/Data/data_training_proc.R")

str(hp_data_fin)


# Split the data into a training and validation dataset
set.seed(524)
hp_testing_data <- hp_data_fin %>%
  filter(!is.na(SalePrice))
hp_data_fin_train_only <- hp_data_fin %>%
  filter(is.na(SalePrice))


random_rows <- floor(sample(1:nrow(hp_data_fin_train_only), 
                            size = nrow(hp_data_fin_train_only) * 0.8))
hp_training_data <- hp_data_fin_train_only[random_rows, ]
hp_validation_data <- hp_data_fin_train_only[-random_rows, ]


# Parameter Tuning
# We will tune our model with a 5 fold CV
fitControl <- trainControl(method = "repeatedcv",
                           number = 5,
                           repeats = 5)

gridParameters <- expand.grid(alpha = 0.1, 
                              lambda = seq(1, 10000, 1000))
# Caret - GLMNET
glmnetFit1 <- train(SalePrice ~ ., 
                    data = hp_training_data %>%
                      select(-Id), 
                    method = "glmnet", 
                    trControl = fitControl, 
                    tuneGrid = gridParameters)
glmnetFit1
# Visualise Parameter Results
glmnetFit1$results %>% 
  mutate(alpha = as.factor(alpha)) %>%
  ggplot() +
  geom_line(aes(x = lambda, y = MAE, colour = alpha))

# Prediction ----
# Predicting on the validation data
prediction_results <- tibble(pred = predict(glmnetFit1, hp_validation_data),
                             obs = hp_validation_data$SalePrice)
forecast::accuracy(prediction_results$pred, prediction_results$obs)


# Non Caret GLMNET ----
# Take a look at the coefficients in the model
glmnetFit2 <- glmnet::glmnet(x = hp_training_data %>%
                               select(-Id, -SalePrice) %>%
                               as.matrix(),
                             y = hp_training_data %>%
                               select(SalePrice) %>%
                               as.matrix(), 
                             alpha = glmnetFit1$bestTune$alpha, 
                             lambda = glmnetFit1$bestTune$lambda)

# Create actual vs fitted
model_fit <- tibble(fitted = predict(glmnetFit2, hp_training_data %>%
                                       select(-Id, -SalePrice) %>%
                                       as.matrix())[, 1],
                    actual = hp_training_data %>%
                      pull(SalePrice))
R2(model_fit$fitted, model_fit$actual)
coefficients(glmnetFit2)

# Test Model Prediction
prediction_results <- tibble(pred = predict(glmnetFit2, hp_validation_data %>%
                                       select(-Id, -SalePrice) %>%
                                       as.matrix())[, 1],
                    obs = hp_validation_data %>%
                      pull(SalePrice))
forecast::accuracy(prediction_results$pred, prediction_results$obs)
