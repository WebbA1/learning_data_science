# Import Data Script for SalePrice Estimation Model
# Library
library(tidyverse)
library(caret)
library(forecast)
library(glmnet)
# Load in the training dataset (Class = SalePrice)
hp_data <- read_csv("./Kaggle/House Price Estimates/Data/train.csv")