data <- read.csv(file.choose())
print(data)
str(data)
library("Metrics")
library("gmodels")
library("caret")

View(data)
train_index <- createDataPartition(data$Outcome, p = 0.7, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

View(train_data)
# Create the logistic regression model
model <- glm(formula = Outcome ~ Glucose + BloodPressure +Age +DiabetesPedigreeFunction,
             data = train_data,
             family = binomial(link = "logit")
             
             )

predictions <- predict(model, newdata = test_data)

mae(test$Outcome, predictions)
mse(test$Outcome, predictions)
rmse(test$Outcome, predictions)

length(predictions)
nrow(test_data)
predicted_labels <- ifelse(predictions >= 0.5, "0", "1")
actual_labels <- test_data$Outcome

length(test$)
length(actual_labels)
View(actual_labels)
length(predicted_labels)

confusion_matrix <- table(actual_labels, predicted_labels)
print(confusion_matrix)


accuracy <- 91/230 #sum of diagonals / n
accuracy
precesion <- 58/176 #tp/tp+fp
precesion
recall <- 58/79 #tp/tp+fn
recall
specificity <- 33/151 #tn/tn+fp
specificity

f1_score = 2*precesion*recall/(precesion + recall)
f1_score
