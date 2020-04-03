# Data Preparation ----
library(Amelia)
source("./Kaggle/House Price Estimates/Scripts/Data/import_data.R")
# hp_training_data

# Firstly, pull out all the columns that are numeric. We can conduct correlation on these.
numeric_columns <- names(which(sapply(hp_training_data %>% select(-Id), is.numeric)))

# NA Characters ----
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
# There are 16 character features which contain NAs. 
# _1. Ally ----
# There are 1099 NAs, 33 Pave, and 36 Grvl.
hp_training_data_char %>%
  group_by(Alley) %>%
  tally()
# I will convert the Alley feature into a factor. Therefore, we will create a dummy
# for the Grvl and Pave instances. Ignoring the NAs. This is because the values in the 
# feature are nominal. So an increasing value means nothing in this feature. 
# TODO Final dataset called 'hp_training_data_fin'
hp_training_data_fin <- hp_training_data %>%
  mutate(Alley = as.factor(Alley))

# _2. Bsmt ----
# Deal with the 5 Bsmt features altogether as they will all be linked.
hp_training_data_char %>%
  select(starts_with("Bsmt")) %>%
  gather("key", "value") %>%
  group_by(key, value) %>%
  tally() %>%
  filter(is.na(value))

# Check if theyre all the same Ids
hp_training_data_char %>%
  select(Id, starts_with("Bsmt")) %>%
  gather("key", "value", -1) %>%
  filter(is.na(value)) %>%
  group_by(Id) %>%
  tally()

# All have 30 NAs apart from Exposure and FinType2, whom have 31
hp_training_data_char %>%
  select(Id, starts_with("Bsmt")) %>%
  filter(is.na(BsmtFinType2)|is.na(BsmtExposure)) 
hp_training_data %>%
  select(Id, starts_with("Bsmt")) %>%
  filter(Id %in% c(949, 333))
# BsmtExposure has one value that has been noted as not having a basement. 
# However, the FinType says that the basement is unfinished. So maybe, the data collector
#   did not know what exposure the basement would have.
# For the missing BsmtFinType2, this probably means that the basement does not 
#   have a second room in it.

# For ID == 333, we can put the NA as 'Unf', as there is SF in room 2
# For ID == 949, its a bit more difficult. The basement is unfinished and exposure 
#   is unclear to the data collector
# TODO what to do to 949

# For all of the Bsmt variables, we can apply an ordinal value to the characters. 
# Where if the data is NA, we give it the number zero.
# TODO Apply all the 'hp_training_data_fin' at the bottom 
hp_training_data_fin <- hp_training_data_fin %>%
  # BsmtCond - Ordinal
  mutate(BsmtCond = case_when(is.na(BsmtCond) ~ 0,
                              BsmtCond == "Po" ~ 1,
                              BsmtCond == "Fa" ~ 2,
                              BsmtCond == "TA" ~ 3,
                              BsmtCond == "Gd" ~ 4,
                              BsmtCond == "Ex" ~ 5)) %>%
  # BsmtExposure - Ordinal
  mutate(BsmtExposure = case_when(Id == 949 ~ 0,
                                  is.na(BsmtExposure) ~ 0,
                                  BsmtExposure == "No" ~ 1,
                                  BsmtExposure == "Mn" ~ 2,
                                  BsmtExposure == "Av" ~ 3,
                                  BsmtExposure == "Gd" ~ 4)) %>%
  # BsmtFinType1 - Ordinal
  mutate(BsmtFinType1 = case_when(is.na(BsmtFinType1) ~ 0,
                                  BsmtFinType1 == "Unf" ~ 1,
                                  BsmtFinType1 == "LwQ" ~ 2,
                                  BsmtFinType1 == "Rec" ~ 3,
                                  BsmtFinType1 == "BLQ" ~ 4,
                                  BsmtFinType1 == "ALQ" ~ 5,
                                  BsmtFinType1 == "GLQ" ~ 6)) %>%
  # BsmtFinType2 - Ordinal - Special Case
  mutate(BsmtFinType2 = case_when(Id == 333 ~ 1,
                                  is.na(BsmtFinType2) ~ 0,
                                  BsmtFinType2 == "Unf" ~ 1,
                                  BsmtFinType2 == "LwQ" ~ 2,
                                  BsmtFinType2 == "Rec" ~ 3,
                                  BsmtFinType2 == "BLQ" ~ 4,
                                  BsmtFinType2 == "ALQ" ~ 5,
                                  BsmtFinType2 == "GLQ" ~ 6)) %>%
  # BsmtQual - Ordinal
  mutate(BsmtQual = case_when(is.na(BsmtQual) ~ 0,
                              BsmtQual == "Po" ~ 1,
                              BsmtQual == "Fa" ~ 2,
                              BsmtQual == "TA" ~ 3,
                              BsmtQual == "Gd" ~ 4,
                              BsmtQual == "Ex" ~ 5))


data <- tibble(test = c("Gd", "Bd", "Gd", NA))
data %>%
  mutate(test = case_when(is.na(test) ~ 0,
                          test == "Bd" ~ 1,
                          test == "Gd" ~ 2))





# _3. Electrical* ----
hp_training_data_char %>%
  group_by(Electrical) %>%
  tally()
# Just the one NA, no reason for it. 
# The Electrical feature will be converted to a factor as it will have to be a nominal value. 
# We cant tell whether having different electric systems in the house can be improvements on one another.
hp_training_data_fin <- hp_training_data_fin %>%
  # Electrical - Nominal
  mutate(Electrical = as.factor(Electrical))

# _4. Fence ----
hp_training_data_char %>%
  group_by(Fence) %>%
  tally()
# Nominal values, NA means there is no fence
hp_training_data_fin <- hp_training_data_fin %>%
  # Fence - Ordinal
  mutate(Fence = case_when(is.na(Fence) ~ 0,
                           Fence == "MnWw" ~ 1,
                           Fence == "GdWo" ~ 2,
                           Fence == "MnPrv" ~ 3,
                           Fence == "GdPrv" ~ 4))
# _5. FireplaceQu ----
hp_training_data_char %>%
  group_by(FireplaceQu) %>%
  tally()
# Simple again, NA means no fireplace
hp_training_data_fin <- hp_training_data_fin %>%
  # FireplaceQu - Ordinal
  mutate(FireplaceQu = case_when(is.na(FireplaceQu) ~ 0,
                                 FireplaceQu == "Po" ~ 1,
                                 FireplaceQu == "Fa" ~ 2,
                                 FireplaceQu == "TA" ~ 3,
                                 FireplaceQu == "Gd" ~ 4,
                                 FireplaceQu == "Ex" ~ 5))
# _6. Garage ----
# how many NAs in each column for Garages
hp_training_data_char %>%
  select(starts_with("Garage")) %>%
  gather("key", "value") %>%
  group_by(key, value) %>%
  tally() %>%
  filter(is.na(value))
# Check if theyre all the same Ids
hp_training_data_char %>%
  select(Id, starts_with("Garage")) %>%
  gather("key", "value", -1) %>%
  filter(is.na(value)) %>%
  group_by(Id) %>%
  tally()
# Create mapping
hp_training_data_fin <- hp_training_data_fin %>%
  # GarageCond - Ordinal
  mutate(GarageCond = case_when(is.na(GarageCond) ~ 0,
                                GarageCond == "Po" ~ 1,
                                GarageCond == "Fa" ~ 2,
                                GarageCond == "TA" ~ 3,
                                GarageCond == "Gd" ~ 4,
                                GarageCond == "Ex" ~ 5)) %>%
  # GarageFinish - Ordinal
  mutate(GarageFinish = case_when(is.na(GarageFinish) ~ 0,
                                  GarageFinish == "Unf" ~ 1,
                                  GarageFinish == "RFn" ~ 2,
                                  GarageFinish == "Fin" ~ 3)) %>%
  # GarageQual - Ordinal
  mutate(GarageQual = case_when(is.na(GarageQual) ~ 0,
                                GarageQual == "Po" ~ 1,
                                GarageQual == "Fa" ~ 2,
                                GarageQual == "TA" ~ 3,
                                GarageQual == "Gd" ~ 4,
                                GarageQual == "Ex" ~ 5)) %>%
  # GarageType - Nominal - Turn to dummy
  mutate(GarageType = as.factor(GarageType))

# _7. MasVnrType ----
hp_training_data_char %>%
  group_by(MasVnrType) %>%
  tally()
# 5 NAs
hp_training_data %>%
  filter(is.na(MasVnrType)) %>%
  select(starts_with("MasVnr"))

# Change them to None, but MasVnr Type will have to be nominal as no order
hp_training_data_fin <- hp_training_data_fin %>%
  replace_na(list(MasVnrType = "None")) %>%
  mutate(MasVnrType = as.factor(MasVnrType))

# _8. MiscFeature ----
# NAs means none.
hp_training_data_char %>%
  group_by(MiscFeature) %>%
  tally()
# Nominal, so just ignore
hp_training_data_fin <- hp_training_data_fin %>%
  mutate(MiscFeature = as.factor(MiscFeature))
# _9. PoolQC -----
# NAs is no pool.
hp_training_data_char %>%
  group_by(PoolQC) %>%
  tally()
hp_training_data_fin <- hp_training_data_fin %>%
  # PoolQC - Ordinal
  mutate(PoolQC = case_when(is.na(PoolQC) ~ 0,
                            PoolQC == "Fa" ~ 1,
                            PoolQC == "TA" ~ 2,
                            PoolQC == "Gd" ~ 3,
                            PoolQC == "Ex" ~ 4))

# NA Numeric ----
# We have 43 variables containing characters. How shhp_data we convert them to numeric?
# Missing values in the numeric columns
hp_training_data_num <- hp_training_data %>%
  select(Id, numeric_columns) %>%
  gather("key", "value", -Id) %>%
  filter(is.na(value)) %>%
  group_by(key) %>%
  tally()

# _1. GarageYrBlt ----
# Not got a garage? Select all garages, check NAs for the char features
hp_training_data %>%
  select(Id, starts_with("Garage")) %>%
  filter(is.na(GarageYrBlt))
GarageYrBlt_mapping <- hp_training_data %>%
  distinct(GarageYrBlt) %>%
  arrange(GarageYrBlt) %>%
  filter(!is.na(GarageYrBlt)) %>%
  mutate(GarageYrBlt_ord = 1:nrow(.)) %>%
  add_row(GarageYrBlt = NA, GarageYrBlt_ord = 0)

hp_training_data_fin <- hp_training_data_fin %>%
  left_join(GarageYrBlt_mapping, by = "GarageYrBlt") %>%
  select(-GarageYrBlt)
# Therefore, no garage, what do you put for the year? 0 
# _2. LotFrontage* ----
# Assume its Zero
hp_training_data_fin <- hp_training_data_fin %>%
  mutate(LotFrontage = case_when(is.na(LotFrontage) ~ 0,
                                 TRUE ~ LotFrontage))

# _3. MasVnrArea ----
# Assume its Zero
hp_training_data_fin <- hp_training_data_fin %>%
  mutate(MasVnrArea = case_when(is.na(MasVnrArea) ~ 0,
                                TRUE ~ MasVnrArea))

# Characters ----
# Need to re-evaluate the columns which are character types
remaining_char_columns <- names(which(sapply(hp_training_data_fin %>% select(-Id), is.character)))

# _1. Nominal Character Features ----
hp_training_data_fin <- hp_training_data_fin %>%
  mutate(MSZoning = as.factor(MSZoning),
         Street = as.factor(Street),
         LandContour = as.factor(LandContour),
         LotConfig = as.factor(LotConfig),
         Neighborhood = as.factor(Neighborhood),
         Condition1 = as.factor(Condition1),
         Condition2 = as.factor(Condition2),
         BldgType = as.factor(BldgType),
         HouseStyle = as.factor(HouseStyle),
         RoofStyle = as.factor(RoofStyle),
         RoofMatl = as.factor(RoofMatl),
         Exterior1st = as.factor(Exterior1st),
         Exterior2nd = as.factor(Exterior2nd),
         Foundation = as.factor(Foundation),
         Heating = as.factor(Heating),
         SaleType = as.factor(SaleType),
         SaleCondition = as.factor(SaleCondition))
# _2. Ordinal Character Features ----
hp_training_data_fin <- hp_training_data_fin %>%
  mutate(LotShape = case_when(LotShape == "IR3" ~ 0,
                              LotShape == "IR2" ~ 1,
                              LotShape == "IR1" ~ 2,
                              LotShape == "Reg" ~ 3)) %>%
  mutate(Utilities = case_when(Utilities == "ELO" ~ 0,
                               Utilities == "NoSeWa" ~ 1,
                               Utilities == "NoSewr" ~ 2,
                               Utilities == "AllPub" ~ 3)) %>%
  mutate(LandSlope = case_when(LandSlope == "Sev" ~ 0,
                               LandSlope == "Mod" ~ 1,
                               LandSlope == "Gtl" ~ 2)) %>%
  mutate(ExterQual = case_when(ExterQual == "Po" ~ 0,
                               ExterQual == "Fa" ~ 1,
                               ExterQual == "TA" ~ 2,
                               ExterQual == "Gd" ~ 3,
                               ExterQual == "Ex" ~ 4)) %>%
  mutate(ExterCond = case_when(ExterCond == "Po" ~ 0,
                               ExterCond == "Fa" ~ 1,
                               ExterCond == "TA" ~ 2,
                               ExterCond == "Gd" ~ 3,
                               ExterCond == "Ex" ~ 4)) %>%
  mutate(HeatingQC = case_when(HeatingQC == "Po" ~ 0,
                               HeatingQC == "Fa" ~ 1,
                               HeatingQC == "TA" ~ 2,
                               HeatingQC == "Gd" ~ 3,
                               HeatingQC == "Ex" ~ 4)) %>%
  mutate(CentralAir = case_when(CentralAir == "N" ~ 0,
                                CentralAir == "Y" ~ 1)) %>%
  mutate(KitchenQual = case_when(KitchenQual == "Po" ~ 0,
                                 KitchenQual == "Fa" ~ 1,
                                 KitchenQual == "TA" ~ 2,
                                 KitchenQual == "Gd" ~ 3,
                                 KitchenQual == "Ex" ~ 4)) %>%
  mutate(Functional = case_when(Functional == "Sal" ~ 0,
                                Functional == "Sev" ~ 1,
                                Functional == "Maj2" ~ 2,
                                Functional == "Maj1" ~ 3,
                                Functional == "Mod" ~ 4,
                                Functional == "Min2" ~ 5,
                                Functional == "Min1" ~ 6,
                                Functional == "Typ" ~ 7)) %>%
  mutate(PavedDrive = case_when(PavedDrive == "N" ~ 0,
                                PavedDrive == "P" ~ 1,
                                PavedDrive == "Y" ~ 2))

# Check the feature types ----
# Find out how many are factors and then run dummy vars through them.
str(hp_training_data_fin)
# Converting to numeric values for the regression
dmy <- dummyVars(" ~ .",
                 data = hp_training_data_fin,
                 na.action = 0, 
                 sep = "_")
hp_training_data_fin <- data.frame(predict(dmy,
                                           newdata = hp_training_data_fin)) %>%
  as_tibble() %>%
  replace(is.na(.), 0)
