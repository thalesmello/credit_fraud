require(readr)
require(dplyr)
require(ggplot2)
require(caret)
require(rpart.plot)

set.seed(1283)
default <- read_csv('Default.csv') %>% select(default:income)
train_factor = 0.8

in_train <- createDataPartition(default$default, p = train_factor, list = F)

train_set <- slice(default, in_train)
test_set <- slice(default, -in_train)
qplot(balance, income, color = default, shape = student, data = train_set)

fit <- train(default ~ ., method = 'rpart', data = train_set)
prp(fit$finalModel)


split <- fit$finalModel$splits %>%
  subset(., row.names(.) == 'balance') %>%
  .[, 'index'] %>%
  max

qplot(balance, income, color = default, shape = student, data = train_set) +
  geom_vline(xintercept = split)

prediction <- predict(fit, newdata = test_set)
confusionMatrix(prediction, test_set$default)
