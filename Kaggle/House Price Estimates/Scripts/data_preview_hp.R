# Estimate House Prices for the Kaggle Competition
# Library
library(tidyverse)
library(caret)
library(anomalize)
library(corrplot)

# Load in mapping for types
types_mapping <- read_csv("./Kaggle/House Price Estimates/Inputs/identify_column_class_complete.csv")
# Load in the training dataset (Class = SalePrice)
# TODO Define the class for each column
hp_data <- read_csv("./Kaggle/House Price Estimates/Data/train.csv", 
                    col_types = types_mapping %>%
                      pull(class_type) %>%
                      paste(collapse = ""))
str(hp_data)

# Converting to numeric values for the regression
dmy <- dummyVars(" ~ .",
                 data = hp_data,
                 na.action = 0)
hp_data_transformed <- data.frame(predict(dmy,
                                          newdata = hp_data)) %>%
  as_tibble() %>%
  replace_na(list(Alley.Grvl = 0, 
                  Alley.Pave = 0,
                  LotFrontage = 0))

# Split the data into a training and validation dataset
set.seed(524)
random_rows <- floor(sample(1:nrow(hp_data_transformed), size = nrow(hp_data_transformed) * 0.8))
hp_training_data <- hp_data_transformed[random_rows, ]
hp_validation_data <- hp_data_transformed[-random_rows, ]

# Check which columns have only one unique value
hp_training_data %>% 
  gather("key", "value") %>% 
  distinct() %>% 
  group_by(key) %>% 
  tally() %>% 
  filter(n == 1)


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
#correlation <- cor(data.frame(hp_training_data))
correlation <- cor(x = hp_training_data %>%
                     select(SalePrice),
                   y = hp_training_data %>%
                     select(-Id, 
                            -SalePrice, 
                            -starts_with("GarageYrBlt"),
                            -starts_with("YearrBuilt"),
                            -starts_with("YearRemodAdd"),
                            -starts_with("YearBuilt")))

correlation %>%
  as_tibble() %>%
  gather("key", "value") %>%
  ggplot(aes(x = key, y = value)) +
  geom_col()

# PCA on the variables
prcomp(hp_training_data %>%
         select(-Condition2.RRAn,
                -Exterior1st.BrkComm,
                -Exterior1st.CBlock,
                -HeatingQC.Po,
                -YearBuilt.1882), scale = TRUE)

# Closer look at how the numeric values affect the SalePrice
continous_numeric_values <- hp_training_data %>%
  select(SalePrice, LotFrontage, LotArea, MasVnrArea, BsmtFinSF1, BsmtFinSF2,
         BsmtUnfSF, TotalBsmtSF, `1stFlrSF`, `2ndFlrSF`, LowQualFinSF, 
         GrLivArea, WoodDeckSF, OpenPorchSF, EnclosedPorch, `3SsnPorch`, 
         ScreenPorch, PoolArea, MiscVal, GarageArea) %>%
  gather("key", "value", -SalePrice) 
# Lot Area
continous_numeric_values %>%
  filter(value != 0,
         key == "LotArea") %>%
  ggplot(aes(x = value, y = SalePrice, colour = key)) +
  geom_point() +
  facet_wrap(~key) +
  geom_smooth(method = "auto", aes(colour = "blue")) +
  theme_dark()

# Lot Area
continous_numeric_values %>%
  filter(value != 0,
         key %in% c("TotalBsmtSF", "1stFlrSF"),
         !is.na(value)) %>%
  ggplot(aes(x = value, y = SalePrice, colour = key)) +
  geom_point() +
  facet_wrap(~key) +
  geom_smooth(method = "lm", formula = "y ~ x") +
  theme_dark()

discrete_numeric_values <- hp_training_data %>%
  select(SalePrice, BsmtFullBath, BsmtHalfBath, FullBath, HalfBath, BedroomAbvGr,
         KitchenAbvGr, TotRmsAbvGrd, Fireplaces, GarageCars) %>%
  gather("key", "value", -SalePrice)

# Bedrooms above ground and total rooms above ground
discrete_numeric_values %>%
  filter(key %in% c("BedroomAbvGr", "TotRmsAbvGrd")) %>%
  ggplot(aes(x = value, y = SalePrice, colour = key, group = value)) +
  geom_boxplot() +
  facet_wrap(~key)
# 
discrete_numeric_values %>%
  filter(!key %in% c("BedroomAbvGr", "TotRmsAbvGrd")) %>%
  ggplot(aes(x = value, y = SalePrice, colour = key, group = value)) +
  geom_boxplot() +
  facet_wrap(~key)

# Look at BsmtQual
hp_data %>%
  select(SalePrice, starts_with("BsmtQual")) %>%
  gather("key", "value", -SalePrice) %>%
  ggplot(aes(x = value, y = SalePrice, colour = key, group = value)) +
  geom_boxplot() +
  facet_wrap(~key)


hp_training_data$BsmtQual.Ex