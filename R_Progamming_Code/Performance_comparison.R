# Required Libraries
library(class)
library(readxl)
library(ggplot2)
library(GGally)
library(caTools)
library(glmnet)
library(randomForest)
library(rpart)
library(rpart.plot)
library(e1071)
# Importing the Dataset (Use the directory in which file is stored as the current working directory)
kerala <- read.csv("kerala.csv")

# Extracting a Part of the Dataset 
ker_new <- kerala[, 3:16]

# Encoding Categorical FLOODS Data
ker_new$FLOODS <- factor(ker_new$FLOODS, levels = c('YES', 'NO'), labels = c(1, 0))

# Splitting the Dataset into Training and Test 
split <- sample.split(ker_new$FLOODS, SplitRatio = 0.8)
training_data <- subset(ker_new, split == TRUE)
testing_data <- subset(ker_new, split == FALSE)

# Feature Scaling the Data 
training_data[, 1:13] <- scale(training_data[, 1:13])
testing_data[, 1:13] <- scale(testing_data[, 1:13])

# Prepare the Data
x_train <- training_data[, -14]
y_train <- training_data$FLOODS
x_test <- testing_data[, -14]
y_test <- testing_data$FLOODS

# Build the Logistic Regression Model for rainy , summer and winter season
classifier_lr_rainy <- glm(formula = FLOODS ~JUL+AUG+SEP, family = binomial, data = training_data)
classifier_lr_summer <- glm(formula = FLOODS ~MAR+APR+MAY, family = binomial, data = training_data)
classifier_lr_winter <- glm(formula = FLOODS ~NOV+DEC+JAN, family = binomial, data = training_data)

# Build the k-Nearest Neighbors Model
k <- 15
classifier_knn <- knn(train = x_train, test = x_test, cl = y_train, k = k)

# Build the Decision Tree Model
classifier_dt <- rpart(FLOODS ~ ., data = training_data, method = "class")

# Build the Random Forest Model
classifier_rf <- randomForest(FLOODS ~ ., data = training_data, ntree = 100)

# Build the Naive Bayes Model
classifier_nb <- naiveBayes(x = x_train, y = y_train)

# Build the Support Vector Machine Model
classifier_svm <- svm(formula = FLOODS ~ ., data = training_data, type = 'C-classification', kernel = 'radial')

# Perform Predictions
y_pred_lr_rainy <- ifelse(predict(classifier_lr_rainy, newdata = testing_data, type = 'response') > 0.5, 1, 0)
y_pred_lr_summer <- ifelse(predict(classifier_lr_summer, newdata = testing_data, type = 'response') > 0.5, 1, 0)
y_pred_lr_winter <- ifelse(predict(classifier_lr_winter, newdata = testing_data, type = 'response') > 0.5, 1, 0)
y_pred_knn <- knn(train = x_train, test = x_test, cl = y_train, k = k)
y_pred_dt <- predict(classifier_dt, newdata = testing_data, type = "class")
y_pred_rf <- predict(classifier_rf, newdata = testing_data)
y_pred_nb <- predict(classifier_nb, newdata = testing_data)
y_pred_svm <- predict(classifier_svm, newdata = testing_data)

# Calculate Accuracy for each Model
accuracy_lr_rainy <- sum(y_pred_lr_rainy == y_test) / length(y_test)
accuracy_lr_summer <- sum(y_pred_lr_summer == y_test) / length(y_test)
accuracy_lr_winter <- sum(y_pred_lr_winter == y_test) / length(y_test)
accuracy_knn <- sum(y_pred_knn == y_test) / length(y_test)
accuracy_dt <- sum(y_pred_dt == y_test) / length(y_test)
accuracy_rf <- sum(y_pred_rf == y_test) / length(y_test)
accuracy_nb <- sum(y_pred_nb == y_test) / length(y_test)
accuracy_svm <- sum(y_pred_svm == y_test) / length(y_test)

# Create a DataFrame for Performance Comparison
results <- data.frame(
  Model = c("Logistic Regression(Rainy)","Logistic Regression(Summer)","Logistic Regression(Winter)", "k-Nearest Neighbors", "Decision Trees", "Random Forests", "Naive Bayes", "Support Vector Machines"),
  Accuracy = c(accuracy_lr_rainy,accuracy_lr_summer,accuracy_lr_winter, accuracy_knn, accuracy_dt, accuracy_rf, accuracy_nb, accuracy_svm)
)

# Print the Results
print(results)

# Plot the Results
ggplot(results, aes(x = Model, y = Accuracy)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  xlab("Machine Learning Algorithm") +
  ylab("Accuracy") +
  ggtitle("Performance Comparison of Machine Learning Algorithms") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Clear the Environment
rm(list = ls())

