# Import Data Script for SalePrice Estimation Model
# Library
library(tidyverse)
library(caret)

# Load in the training dataset (Class = SalePrice)
hp_data <- read_csv("./Kaggle/House Price Estimates/Data/train.csv")

# Remove the Id column - We will not be needing it
# hp_data_no_Id <- hp_data %>% 
#   select(-Id)

# Split the data into a training and validation dataset
set.seed(524)
random_rows <- floor(sample(1:nrow(hp_data), size = nrow(hp_data) * 0.8))
hp_training_data <- hp_data[random_rows, ]
hp_validation_data <- hp_data[-random_rows, ]