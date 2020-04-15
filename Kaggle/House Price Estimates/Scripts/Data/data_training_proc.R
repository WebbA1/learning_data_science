# Create Modelling Dataset ----
# Mapping table for GarageYrBlt
GarageYrBlt_mapping <- hp_data %>%
  distinct(GarageYrBlt) %>%
  arrange(GarageYrBlt) %>%
  filter(!is.na(GarageYrBlt)) %>%
  mutate(GarageYrBlt_ord = 1:nrow(.)) %>%
  add_row(GarageYrBlt = NA, GarageYrBlt_ord = 0)


hp_training_data_fin <- hp_training_data %>%
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
                              BsmtQual == "Ex" ~ 5)) %>%
  # Fence - Ordinal
  mutate(Fence = case_when(is.na(Fence) ~ 0,
                           Fence == "MnWw" ~ 1,
                           Fence == "GdWo" ~ 2,
                           Fence == "MnPrv" ~ 3,
                           Fence == "GdPrv" ~ 4)) %>%
  # FireplaceQu - Ordinal
  mutate(FireplaceQu = case_when(is.na(FireplaceQu) ~ 0,
                                 FireplaceQu == "Po" ~ 1,
                                 FireplaceQu == "Fa" ~ 2,
                                 FireplaceQu == "TA" ~ 3,
                                 FireplaceQu == "Gd" ~ 4,
                                 FireplaceQu == "Ex" ~ 5)) %>%
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
  # PoolQC - Ordinal
  mutate(PoolQC = case_when(is.na(PoolQC) ~ 0,
                            PoolQC == "Fa" ~ 1,
                            PoolQC == "TA" ~ 2,
                            PoolQC == "Gd" ~ 3,
                            PoolQC == "Ex" ~ 4)) %>%
  # LotFrontage - Numeric, NAs are 0
  mutate(LotFrontage = case_when(is.na(LotFrontage) ~ 0,
                                 TRUE ~ LotFrontage)) %>%
  # MasVnrArea - Numeric, NAs are 0
  mutate(MasVnrArea = case_when(is.na(MasVnrArea) ~ 0,
                                TRUE ~ MasVnrArea)) %>%
  # LotShape
  mutate(LotShape = case_when(LotShape == "IR3" ~ 0,
                              LotShape == "IR2" ~ 1,
                              LotShape == "IR1" ~ 2,
                              LotShape == "Reg" ~ 3)) %>%
  # Utilities
  mutate(Utilities = case_when(Utilities == "ELO" ~ 0,
                               Utilities == "NoSeWa" ~ 1,
                               Utilities == "NoSewr" ~ 2,
                               Utilities == "AllPub" ~ 3)) %>%
  # LandSlope
  mutate(LandSlope = case_when(LandSlope == "Sev" ~ 0,
                               LandSlope == "Mod" ~ 1,
                               LandSlope == "Gtl" ~ 2)) %>%
  # ExterQual
  mutate(ExterQual = case_when(ExterQual == "Po" ~ 0,
                               ExterQual == "Fa" ~ 1,
                               ExterQual == "TA" ~ 2,
                               ExterQual == "Gd" ~ 3,
                               ExterQual == "Ex" ~ 4)) %>%
  # ExterCond
  mutate(ExterCond = case_when(ExterCond == "Po" ~ 0,
                               ExterCond == "Fa" ~ 1,
                               ExterCond == "TA" ~ 2,
                               ExterCond == "Gd" ~ 3,
                               ExterCond == "Ex" ~ 4)) %>%
  # HeatingQC
  mutate(HeatingQC = case_when(HeatingQC == "Po" ~ 0,
                               HeatingQC == "Fa" ~ 1,
                               HeatingQC == "TA" ~ 2,
                               HeatingQC == "Gd" ~ 3,
                               HeatingQC == "Ex" ~ 4)) %>%
  # CentralAir
  mutate(CentralAir = case_when(CentralAir == "N" ~ 0,
                                CentralAir == "Y" ~ 1)) %>%
  # KitchenQual
  mutate(KitchenQual = case_when(KitchenQual == "Po" ~ 0,
                                 KitchenQual == "Fa" ~ 1,
                                 KitchenQual == "TA" ~ 2,
                                 KitchenQual == "Gd" ~ 3,
                                 KitchenQual == "Ex" ~ 4)) %>%
  # Functional
  mutate(Functional = case_when(Functional == "Sal" ~ 0,
                                Functional == "Sev" ~ 1,
                                Functional == "Maj2" ~ 2,
                                Functional == "Maj1" ~ 3,
                                Functional == "Mod" ~ 4,
                                Functional == "Min2" ~ 5,
                                Functional == "Min1" ~ 6,
                                Functional == "Typ" ~ 7)) %>%
  # PavedDrive
  mutate(PavedDrive = case_when(PavedDrive == "N" ~ 0,
                                PavedDrive == "P" ~ 1,
                                PavedDrive == "Y" ~ 2)) %>%
  replace_na(list(MasVnrType = "None")) %>%
  mutate(Alley = as.factor(Alley), 
         Electrical = as.factor(Electrical), 
         GarageType = as.factor(GarageType), 
         # MasVnrType - Change them to None, but MasVnr Type will have to be nominal as no order
         MasVnrType = as.factor(MasVnrType),
         MiscFeature = as.factor(MiscFeature), 
         MSZoning = as.factor(MSZoning),
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
         SaleCondition = as.factor(SaleCondition)) %>%
  left_join(GarageYrBlt_mapping, by = "GarageYrBlt") %>%
  select(-GarageYrBlt)

# Create dummy variables for final dataset ----
dmy <- dummyVars(" ~ .",
                 data = hp_training_data_fin,
                 na.action = 0, 
                 sep = "_")
hp_training_data_fin <- data.frame(predict(dmy,
                                           newdata = hp_training_data_fin)) %>%
  as_tibble() %>%
  replace(is.na(.), 0)
