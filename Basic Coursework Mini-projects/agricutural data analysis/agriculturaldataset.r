data <- read.csv(file.choose()) #read the yield data
print(data)
str(data)
library("Metrics")
library("gmodels")
library("caret")
library("forcats")

data$Area.Code <- as.factor(data$Area.Code)
data$Item.Code <- as.factor(data$Item.Code)
data$Year.Code <- as.factor(data$Year.Code)


str(data)


trainIndex <- createDataPartition(data$Value, p = 0.8, list = FALSE)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

glmModel <- glm(Value ~ Area.Code + Item.Code + Year.Code, data = trainData, family = gaussian)
predictions <- predict(glmModel, newdata = testData)
predicted_labels <- ifelse(predictions < 0.5, 0, 1)
View(predicted_labels)
length(predicted_labels)

dim(predicted_labels)
nrow(actual_labels)
testData$norm_value <- scale(testData$Value, center = FALSE, scale = max(testData$Value))
actual_labels <- testData$norm_value
actual_labels <- ifelse(actual_labels < 0.5, 0, 1)
actual_labels 

mae(testData$norm_value, predicted_labels)
mse(testData$norm_value, predicted_labels)
rmse(testData$norm_value, predicted_labels)


actual_labels <- testData$norm_value
actual_labels <- ifelse(actual_labels < 0.5, 0, 1)

confusion_matrix <- table(actual_labels, predicted_labels)
print(confusion_matrix)

acc <- 1287/11341
acc

errorrate <- 10054/11341
errorrate

precesion <- 2/10054
precesion

recall <- 2/2
recall

specificity <- 1285/11339
specificity

f1score <- 2 *(precesion * recall)/ (precesion + recall)
f1score


