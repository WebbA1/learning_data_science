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

# Data Review ----
# Change Class to numeric, where Good = 0, Bad = 1
GermanCredit_training <- GermanCredit_training %>%
  mutate(Class = case_when(Class == "Good" ~ 0,
                           Class == "Bad" ~ 1),
         Class = as.factor(Class))
# Look at Class
ggplot(GermanCredit_training) +
  geom_bar(aes(x = Class), stat = "count")
# Create a correlation matrix to see if there are any interesting features
correlation_matrix <- cor(GermanCredit_training %>%
                            select(-Class),
                          GermanCredit_training %>%
                            select(Class))
# Do a box plot for the continuous variables
GermanCredit_training %>%
  select(Duration, Amount, InstallmentRatePercentage, Age, Class) %>%
  gather("Key", "Value", -c("Class")) %>%
  ggplot() +
  geom_boxplot(aes(x = Class, y = Value, fill = Class)) +
  facet_grid(Key ~ ., scales = "free")
# Do a bar chart for the discrete variables in the data
GermanCredit_training %>%
  select(-Duration, -Amount, -InstallmentRatePercentage, -Age) %>%
  gather("Key", "Value", -c("Class")) %>%
  group_by(Class, Key) %>%
  summarise(Value = sum(Value)) %>%
  ggplot() +
  geom_col(aes(x = Class, y = Value, fill = Class)) +
  facet_wrap(~Key)

# Missing Values
Amelia::missmap(GermanCredit_training, main = "Missing values vs observed")

# Controls on training -----
# Cross Validation
folds <- 5
cvIndex <- createFolds(factor(GermanCredit_training$Class), 
                       folds,
                       returnTrain = T)
trControl <- trainControl(method = "cv",
                          index = cvIndex,
                          number = folds)
# Train a logisitic model -----
logistic_regression_german_credit <- train(Class ~ ., 
                                           data = GermanCredit_training,
                                           trControl = trControl,
                                           tuneLength = 20,
                                           method = "logreg")
