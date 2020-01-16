# Start the modelling process for house price estimates
# Cross validation set up.
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10)

lmFit1 <- train(SalePrice ~ LotArea + X.1stFlrSF., 
                 data = hp_training_data, 
                 method = "glmnet", 
                 trControl = fitControl)
