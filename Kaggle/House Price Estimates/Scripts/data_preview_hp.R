# Estimate House Prices for the Kaggle Competition
# Library
library(tidyverse)
library(caret)

# Load in mapping for types
types_mapping <- read_csv("./Kaggle/House Price Estimates/Inputs/identify_column_class_complete.csv")
# Load in the training dataset (Class = SalePrice)
# TODO Define the class for each column
hp_data <- read_csv("./Kaggle/House Price Estimates/Data/train.csv", 
                    col_types = types_mapping %>%
                      pull(class_type) %>%
                      paste(collapse = ""))

# Split the data into a training and validation dataset
set.seed(524)
random_rows <- floor(sample(1:nrow(hp_data), size = nrow(hp_data) * 0.8))
hp_training_data <- hp_data[random_rows, ]
hp_validation_data <- hp_data[-random_rows, ]

