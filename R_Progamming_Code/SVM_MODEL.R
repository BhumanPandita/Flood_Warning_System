##### LIBRARIES
library(class)
library(readxl)
library(ggplot2)
library(GGally)
library(caTools)
library(glmnet)
library(e1071)

##### IMPORTING THE DATASET
kerala <- read_excel("Documents/PS1/kerala.xlsx")

##### EXTRACTING A PART OF THE DATASET 
ker_new <- kerala[,3:16]
View(ker_new)

##### ENCODING CATEGORICAL FLOODS DATA
ker_new$FLOODS <- factor(ker_new$FLOODS,levels = c('YES','NO'),labels = c(1,0))

##### SPLITING THE DATASET INTO TRAINING AND TEST 
split <- sample.split(ker_new$FLOODS,SplitRatio = 0.8)
training_data <- subset(ker_new,split==TRUE)
testing_data <- subset(ker_new,split==FALSE)
View(training_data)
View(testing_data)

##### FEATURE SCALING THE DATA 
training_data[,1:13] <- scale(training_data[,1:13])
testing_data[,1:13] <- scale(testing_data[,1:13])

##### BUILDING THE SVM MODEL
classifier <- svm(formula = FLOODS~.,
                  data = training_data,
                  type = 'C-classification',
                  kernel = 'radial')
summary(classifier)

##### PREDICT USING THE MODEL
y_pred <- predict(classifier, newdata = testing_data)

##### CONFUSION MATRIX
cm <- table(testing_data$FLOODS, y_pred)
print(cm)

##### CALCULATE ACCURACY
accuracy <- sum(y_pred == testing_data$FLOODS) / length(testing_data$FLOODS)
print(paste("Accuracy:", accuracy))

##### TO CLEAR THE ENVIRONMENT
rm(list = ls())
