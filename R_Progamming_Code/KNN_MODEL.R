##### LIBRARIES
library(class)
library(readxl)
library(ggplot2)
library(GGally)
library(caTools)
library(glmnet)

##### IMPORTING THE DATASET
kerala <- read_excel("Documents/PS1/kerala.xlsx")

##### EXTRACTING A PART OF THE DATASET 
ker_new <- kerala[,3:16]
View(ker_new)

###### ENCODING CATEGORICAL FLOODS DATA
ker_new$FLOODS <- factor(ker_new$FLOODS,levels = c('YES','NO'),labels = c(1,0))

###### SPLITING THE DATASET INTO TRAINING AND TEST 
split <- sample.split(ker_new$FLOODS,SplitRatio = 0.8)
training_data <- subset(ker_new,split==TRUE)
testing_data <- subset(ker_new,split==FALSE)
View(training_data)
View(testing_data)

###### FEATURE SCALING THE DATA 
training_data[,1:13] <- scale(training_data[,1:13])
testing_data[,1:13] <- scale(testing_data[,1:13])

##### PREPARE THE DATA
x_train <- training_data[, -14]
y_train <- training_data$FLOODS
x_test <- testing_data[, -14]
y_test <- testing_data$FLOODS

##### PERFORM KNN CLASSIFICATION
k <- 15
y_pred <- knn(train = x_train, 
              test = x_test, 
              cl = y_train,
              k = k)

##### PRINT THE PREDICTED LABELS
print(y_pred)

##### CALCULATE THE ACCURACY
accuracy <- sum(y_pred == y_test) / length(y_test)

##### PRINT THE ACCURACY
print(accuracy)


##### OPTIMIZATION LOOP
k_values <- 1:30
accuracy_values <- numeric(length(k_values))
for (i in k_values) {
  # Perform kNN classification
  y_pred <- knn(train = x_train, test = x_test, cl = y_train, k = i)
  
  # Calculate accuracy
  accuracy_values[i] <- sum(y_pred == y_test) / length(y_test)
  
  # Print the accuracy for current K value
  cat("K =", i, "Accuracy =", accuracy_values[i], "\n")
}

##### PLOT THE ACCURACY VALUES FOR EACH K VALUE
plot(k_values, accuracy_values, type = "b", xlab = "K-Value", 
     ylab = "Accuracy level")

##### TO CLEAR THE ENVIRONMENT
rm(list = ls())

