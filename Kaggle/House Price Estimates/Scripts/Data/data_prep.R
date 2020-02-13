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

# How many contain NAs?
char_NA_col <- hp_training_data_char %>%
  gather("key", "value", -Id) %>%
  filter(is.na(value)) %>%
  distinct(key) %>% 
  arrange(key)
# There are 16 features which contain NAs. 
# 1. Ally ----
# There are 1099 NAs, 33 Pave, and 36 Grvl.
hp_training_data_char %>%
  group_by(Alley) %>%
  tally()
# I will convert the Alley feature into a factor. Therefore, we will create a dummy
# for the Grvl and Pave instances. Ignoring the NAs. This is because the values in the 
# feature are nominal. So an increasing value means nothing in this feature. 
hp_training_data_fin <- hp_training_data %>%
  mutate(Alley = as.factor(Alley))

# Bsmt* ----
# Deal with the 5 Bsmt features altogether as they will all be linked.
hp_training_data_char %>%
  select(starts_with("Bsmt")) %>%
  gather("key", "value") %>%
  group_by(key, value) %>%
  tally() %>%
  filter(is.na(value))
# All have 30 NAs apart from Exposure and FinType2, whom have 31
hp_training_data_char %>%
  select(Id, starts_with("Bsmt")) %>%
  filter(is.na(BsmtFinType2)|is.na(BsmtExposure)) %>%
  View()
# BsmtExposure has one value that has been noted as not having a basement. 
# However, the FinType says that the basement is unfinished. So maybe, the data collector
# did not know what exposure the basement would have.
# For the missing BsmtFinType2, this probably means that the basement does not 
# have a second room in it.

# For all of the Bsmt variables, we can apply an ordinal value to the characters. 
# Where if the data is NA, we give it the number zero.
data <- tibble(test = c("Gd", "Bd", "Gd", NA))
data %>%
  mutate(test = case_when(is.na(test) ~ 0,
                          test == "Bd" ~ 1,
                          test == "Gd" ~ 2))





# We have 43 variables containing characters. How shhp_data we convert them to numeric? ----
# Missing values in the numeric columns
hp_training_data_num <- hp_training_data %>%
  select(Id, numeric_columns) %>%
  gather("key", "value", -Id) %>%
  filter(is.na(value)) %>%
  group_by(key) %>%
  tally()

# Columns with missing data


