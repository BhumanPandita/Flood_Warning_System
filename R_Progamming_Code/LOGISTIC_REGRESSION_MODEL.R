##### LIBRARIES
library(class)
library(readxl)
library(ggplot2)
library(GGally)
library(caTools)
library(glmnet)
library(coefplot)

##### IMPORTING THE DATASET
kerala <- read_excel("kerala.xlsx")

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

###### BUILD THE LOGISTIC REGRESSION MODEL
classifier <- glm(formula = FLOODS~JUL+AUG+SEP,
                  family = binomial,
                  data = training_data,
                  control = glm.control(maxit = 100))


##### MAKE PREDICTIONS USING THE REGRESSION MODEL
prob_predict_LR <- predict(classifier,type='response',newdata = testing_data[,1:13])

##### CONVERT PREDICTED PROBABILITIES TO 0 OR 1
y_predict_LR <- ifelse(prob_predict_LR>0.5,1,0)

##### CONFUSION MATRIX
cm <- table(testing_data$FLOODS, y_predict_LR)
print(cm)

##### CALCULATING ACCURACY
accuracy <- sum(y_predict_LR == testing_data$FLOODS) / length(testing_data$FLOODS)

##### PRINT ACCURACY
print(accuracy)

##### PLOT THE COEFFICIENTS
coefplot(classifier)

# TO CLEAR THE ENVIRONMENT
rm(list = ls())

