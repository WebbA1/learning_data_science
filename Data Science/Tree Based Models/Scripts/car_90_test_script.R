library(tidyverse)
library(rpart)
library(rpart.plot)

# Clean the transmission data
data <- rpart::car90 %>%
  filter(!is.na(Price)) %>%
  select(-Rim, -Tires, -Model2, -Disp)
str(data)

# Build Model
treeModel_info <- rpart::rpart(Reliability ~ Price + Country + Mileage + Type, cu.summary, parms = list(split = "information"))
treeModel_gini <- rpart::rpart(Reliability ~ Price + Country + Mileage + Type, cu.summary, parms = list(split = "gini"))
# Complexity Parameter for the decision tree, 'xerror' is the cross validation
printcp(treeModel_info)
printcp(treeModel_gini)
# Cross Validation Results
rpart.plot(treeModel_info)
rpart.plot(treeModel_gini)

# Create a plot of the tree
plot(treeModel, margin = 0.1) 
text(treeModel, pretty = T, use.n = T)
rpart.plot(treeModel)
# In case we need to cut down the number of branches in order to reduce
# the chances of overfitting the data, we want to select a tree size that minimises
# the cross validated error.
treeModel <- rpart::rpart(Price ~ ., data, control = rpart.control(cp = 0.00001))
printcp(treeModel)
rpart.plot(treeModel, digits = 1)
treeModel_constraint <- prune(treeModel, cp = 0.045)
printcp(treeModel_constraint)
rpart.plot(treeModel_constraint, digits = 1)
