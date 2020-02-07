# Correlation Analysis
library(Amelia)
source("./Kaggle/House Price Estimates/Scripts/Data/import_data.R")
# hp_training_data

# Firstly, pull out all the columns that are numeric. We can conduct correlation on these.
numeric_columns <- names(which(sapply(hp_training_data %>% select(-Id), is.numeric)))

# Secondly, we can isolate non-numeric columns. How to make them numeric
hp_training_data_char <- hp_training_data %>%
  select(-numeric_columns)
str(hp_training_data_char)
# We have 43 variables containing characters. How shhp_data we convert them to numeric? 
hp_data %>% 
  select(starts_with("Bsmt")) %>% 
  gather("key", "value") %>%
  filter(is.na(value)) %>%
  group_by(key) %>%
  tally()
hp_data %>% 
  select(starts_with("Bsmt")) %>%
  slice(333)
missing_Bsmt <- hp_training_data_char %>% 
  select(Id, starts_with("Bsmt")) %>%
  gather("key", "value", -1) %>%
  filter(is.na(value)) %>%
  distinct(Id)
hp_training_data_char %>% 
  select(Id, starts_with("Bsmt")) %>%
  filter(Id %in% missing_Bsmt$Id)


# Missing values in the numeric columns
hp_training_data_num <- hp_training_data %>%
  select(Id, numeric_columns) %>%
  gather("key", "value", -Id) %>%
  filter(is.na(value)) %>%
  group_by(key) %>%
  tally()

# Columns with missing data


