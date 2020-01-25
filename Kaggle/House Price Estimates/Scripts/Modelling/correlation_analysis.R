# Correlation Analysis
source("./Kaggle/House Price Estimate/Scripts/data_preview.R")

# Converting to numeric values for the regression
dmy <- dummyVars(" ~ .",
                 data = hp_training_data,
                 na.action = 0)
hp_training_data_dummy <- data.frame(predict(dmy,
                                             newdata = hp_training_data)) %>%
  as_tibble() %>%
  replace_na(list(Alley.Grvl = 0, 
                  Alley.Pave = 0,
                  LotFrontage = 0))


# Closer look at the correlation and interation of the variables and the class
#correlation <- cor(data.frame(hp_training_data))
correlation <- cor(x = hp_training_data_dummy %>%
                     select(SalePrice),
                   y = hp_training_data_dummy %>%
                     select(-Id, 
                            -SalePrice, 
                            -starts_with("GarageYrBlt"),
                            -starts_with("YearrBuilt"),
                            -starts_with("YearRemodAdd"),
                            -starts_with("YearBuilt")))

# Distribution of correlation values
correlation %>%
  as_tibble() %>%
  gather("key", "value") %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 100)

# Select the correlation values that are greater than |0.4|
correlation %>%
  as_tibble() %>%
  gather("key", "value") %>%
  mutate(abs_value = abs(value)) %>%
  filter(abs_value >= 0.4)
