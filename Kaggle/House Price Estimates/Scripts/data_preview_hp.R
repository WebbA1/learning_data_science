# Estimate House Prices for the Kaggle Competition
# Library
library(tidyverse)
library(caret)
library(anom)

# Load in the training dataset (Class = SalePrice)
hp_training_data <- read_csv("./Kaggle/House Price Estimates/Data/train.csv")
str(hp_training_data)

# Class Deep Dive ----
# Closer look at the class variable
# 1. What is the distribution of SalePrice 
summary(hp_training_data %>%
          select(SalePrice))
hp_training_data %>%
  select(SalePrice) %>%
  ggplot(aes(x = SalePrice)) +
  geom_histogram(bins = 100)
# 2. Look at the SalePrice over time
hp_training_data %>%
  group_by(YrSold) %>%
  summarise(SalePrice = mean(SalePrice)) %>%
  ggplot(aes(x = YrSold, y = SalePrice)) +
  geom_col()

# Variable Deep Dive ----
# Closer look at the correlation and interation of the variables and the class
