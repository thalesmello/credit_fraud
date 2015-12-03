require(readr)
require(dplyr)
require(ggplot2)
require(caret)
require(rpart.plot)
require(doMC)

registerDoMC(4)
set.seed(1283)
default <- read_csv('Default.csv') %>% select(default:income)
train_factor <- 0.7

in_train <- createDataPartition(default$default, p = train_factor, list = F)

train_set <- slice(default, in_train)
test_set <- slice(default, -in_train)
qplot(balance, income, color = default, shape = student, data = train_set)

# ------------------------
# Decision Trees
# ------------------------
fit_decision_tree <- train(default ~ ., method = 'rpart', data = train_set)
prp(fit_decision_tree$finalModel)
split <- fit_decision_tree$finalModel$splits %>%
  subset(., row.names(.) == 'balance') %>%
  .[, 'index'] %>%
  max
qplot(balance, income, color = default, shape = student, data = train_set) +
  geom_vline(xintercept = split)
prediction_decision_tree <- predict(fit_decision_tree, newdata = test_set)
confusionMatrix(prediction_decision_tree, test_set$default)

# ------------------------
# Random Forests
# ------------------------
fit_random_forest <- train(default ~ ., method = 'rf', data = train_set)
prediction_rf <- predict(fit_decision_tree, newdata = test_set)
confusionMatrix(prediction_rf, test_set$default)

# ------------------------
# Logarithmic Regression
# ------------------------
fit_logistic <- train(default ~ ., method = 'glm', data = train_set)
prediction_rf <- predict(fit_logistic, newdata = test_set)
confusionMatrix(prediction_rf, test_set$default)
cf <- coef(fit_logistic$finalModel)
slope <- cf[3]/(-cf[4])
intercept <- cf[1]/(-cf[4])
qplot(balance, income, color = default, shape = student, data = train_set) +
  geom_abline(intercept = intercept, slope = slope)
