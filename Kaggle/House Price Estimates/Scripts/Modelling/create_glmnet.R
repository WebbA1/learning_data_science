# Sourcing & Library
source("./Kaggle/House Price Estimates/Scripts/Data/import_data.R")
source("./Kaggle/House Price Estimates/Scripts/Data/data_training_proc.R")
source("./Kaggle/House Price Estimates/Scripts/Data/data_validation_proc.R")

str(hp_training_data_fin)



# Parameter Tuning
# We will tune our model with a 5 fold CV
fitControl <- trainControl(method = "repeatedcv",
                           number = 5,
                           repeats = 5)
                       
# GLMNET
glmentFit1 <- train(SalePrice ~ ., 
                    data = hp_training_data_fin, 
                    method = "glmnet", 
                    trControl = fitControl,
                    tuneLength = 1)
glmentFit1
summary(glmentFit1)


predict(glmentFit1, hp_validation_data)
