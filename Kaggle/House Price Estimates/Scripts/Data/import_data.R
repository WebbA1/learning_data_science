# Import Data Script for SalePrice Estimation Model
# Library
library(tidyverse)
library(caret)
library(forecast)
library(glmnet)
# Load in the training dataset (Class = SalePrice)
# We need to import in both the training and the test datasets, process together
hp_data_raw_train <- read_csv("./Kaggle/House Price Estimates/Data/train.csv")
hp_data_raw_test <- read_csv("./Kaggle/House Price Estimates/Data/test.csv")

# Create a total dataset to proccess in "data_training_proc.R"
hp_data <- bind_rows(hp_data_raw_train, hp_data_raw_test)
