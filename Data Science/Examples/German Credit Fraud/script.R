library(tidyverse)
library(caret)
library(GGally)
library(LogicReg)
library(Amelia)
# Data Import and Partition ----
# Can we detect fraud in the dataset
# In the online example, 1 is good, while 0 is bad
data("GermanCredit")
str(GermanCredit)
# Create Data partition, training and testing
partition_index <- createDataPartition(GermanCredit$Class, 
                                       times = 1, 
                                       p = 0.75, 
                                       list = FALSE)
GermanCredit_training <- GermanCredit[partition_index, ]
GermanCredit_testing  <- GermanCredit[-partition_index, ]

str(GermanCredit_training)
# Data Review ----
# Change Class to numeric, where Good = 0, Bad = 1
GermanCredit_training_modded <- GermanCredit_training %>%
  mutate(Class_new = as.character(Class),
         Class_new = case_when(Class_new == "Good" ~ 0,
                               Class_new == "Bad" ~ 1),
         Class_new = as.factor(Class_new)) %>%
  select(-Class, -Duration, -Amount)
# Look at Class
ggplot(GermanCredit_training_modded) +
  geom_bar(aes(x = Class_new), stat = "count")
# Create a correlation matrix to see if there are any interesting features
correlation_matrix <- cor(GermanCredit_training_modded %>%
                            select(-Class_new),
                          GermanCredit_training_modded %>%
                            select(Class_new))
# Do a box plot for the continuous variables
GermanCredit_training_modded %>%
  select(Duration, Amount, InstallmentRatePercentage, Age, Class_new) %>%
  gather("Key", "Value", -c("Class_new")) %>%
  ggplot() +
  geom_boxplot(aes(x = Class_new, y = Value, fill = Class_new)) +
  facet_grid(Key ~ ., scales = "free")
# Do a bar chart for the discrete variables in the data
GermanCredit_training_modded %>%
  select(-Duration, -Amount, -InstallmentRatePercentage, -Age) %>%
  gather("Key", "Value", -c("Class_new")) %>%
  group_by(Class_new, Key) %>%
  summarise(Value = sum(Value)) %>%
  ggplot() +
  geom_col(aes(x = Class_new, y = Value, fill = Class_new)) +
  facet_wrap(~Key)

# Missing Values
Amelia::missmap(GermanCredit_training_modded, main = "Missing values vs observed")

# Controls on training -----
# Cross Validation
folds <- 5
cvIndex <- createFolds(factor(GermanCredit_training_modded$Class_new), 
                       folds,
                       returnTrain = T)
trControl <- trainControl(method = "cv",
                          index = cvIndex,
                          number = folds)
# Train a logisitic model -----
logistic_regression_german_credit <- train(Class_new ~ ForeignWorker + Purpose.NewCar, 
                                           data = GermanCredit_training_modded,
                                           trControl = trControl,
                                           tuneLength = 1,
                                           method = "logreg")
