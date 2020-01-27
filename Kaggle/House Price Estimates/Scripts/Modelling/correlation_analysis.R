# Correlation Analysis
source("./Kaggle/House Price Estimate/Scripts/data_preview.R")

# Converting to numeric values for the regression
dmy <- dummyVars(" ~ .",
                 data = hp_training_data,
                 na.action = 0, 
                 sep = "_")
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


# We have a list of variabes to look at now
hp_training_data_dummy %>%
  ggplot(aes(x = Neighborhood_NridgHt, 
             y = SalePrice, 
             group = Neighborhood_NridgHt, 
             colour = Neighborhood_NridgHt)) +
  geom_boxplot()
hp_training_data_dummy %>%
  ggplot(aes(x = Neighborhood_Somerst, 
             y = SalePrice, 
             group = Neighborhood_Somerst, 
             colour = Neighborhood_Somerst)) +
  geom_boxplot()


hp_training_data %>%
  ggplot(aes(x = Neighborhood, 
             y = SalePrice,
             colour = Neighborhood)) +
  geom_boxplot()

# Look at outliers on the basement SF
hp_training_data_outlier <- hp_training_data %>%
  mutate(Outlier = case_when(TotalBsmtSF < 6000 ~ TRUE,
                             TRUE ~ FALSE))

hp_training_data %>%
  select(SalePrice, TotalBsmtSF) %>%
  filter(TotalBsmtSF != 0) %>%
  ggplot(aes(x = TotalBsmtSF, y = SalePrice)) +
  geom_point()
hp_training_data_outlier %>%
  select(SalePrice, Outlier, TotalBsmtSF) %>%
  filter(TotalBsmtSF != 0) %>%
  ggplot(aes(x = TotalBsmtSF, y = SalePrice, colour = Outlier)) +
  geom_point()

hp_training_data_outlier %>%
  select(SalePrice, Outlier, `1stFlrSF`) %>%
  filter(`1stFlrSF` != 0) %>%
  ggplot(aes(x = `1stFlrSF`, y = SalePrice, colour = Outlier)) +
  geom_point()

hp_training_data_outlier %>%
  ggplot(aes(x = Neighborhood, y = SalePrice)) +
  geom_boxplot()

