# Install required libraries (if not already installed)
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("caret")) install.packages("caret")
if (!require("pROC")) install.packages("pROC")
if (!require("e1071")) install.packages("e1071", dependencies = TRUE)  # For logistic regression
if (!require("kernlab")) install.packages("kernlab")  # For SVM
if (!require("randomForest")) install.packages("randomForest")  # For Random Forest

library(tidyverse)
library(caret)
library(pROC)
library(e1071)
library(kernlab)
library(randomForest)

# Load the previously created data (assuming data2 is available)

# Check for missing values in data2
s <- sum(is.na(data2))
cat("Number of missing values in data2:", s, "\n")

# Alternative check for missing values
is.null(data2)  # This returns TRUE if the entire object is NULL

# Set a random seed for reproducibility
set.seed(10)

# Get column names of data2
colnames(data2)

# Create a training and testing set split (70% training, 30% testing)
inTrainRows <- createDataPartition(data2$target, p = 0.7, list = FALSE)
trainData <- data2[inTrainRows, ]
testData <- data2[-inTrainRows, ]

# Check the split ratio (should be around 70/30)
cat("Ratio of training to testing data:", nrow(trainData) / (nrow(testData) + nrow(trainData)), "\n")

# Initialize empty lists to store AUC and Accuracy values for each model
AUC <- list()
Accuracy <- list()

# Logistic Regression Model
# Install e1071 package if not already installed (needed for logistic regression)

# Train the logistic regression model
logRegModel <- train(target ~ ., data = trainData, method = 'glm', family = 'binomial')

# Predict class labels on the test set
logRegPrediction <- predict(logRegModel, testData)

# Predict class probabilities on the test set
logRegPredictionprob <- predict(logRegModel, testData, type = 'prob')[2]

# Create confusion matrix to evaluate model performance
logRegConfMat <- confusionMatrix(logRegPrediction, testData[, "target"])

# Calculate AUC using ROC curve
library(pROC)
AUC$logReg <- roc(as.numeric(testData$target), as.numeric(as.matrix((logRegPredictionprob))))

# Extract accuracy from confusion matrix
logReg <- logRegConfMat$overall['Accuracy']

# Support Vector Machine (SVM) Model
# Install kernlab package if not already installed (needed for SVM)

# Set up control parameters for cross-validation (repeated CV with 10 folds, 10 repeats)
fitControl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 10,
  classProbs = TRUE,  # Estimate class probabilities
  summaryFunction = twoClassSummary  # Evaluate performance using twoClassSummary
)

# Train the SVM model with radial kernel
set.seed(10)
svmModel <- train(
  target ~ .,
  data = trainData,
  method = "svmRadial",
  trControl = fitControl,
  preProcess = c("center", "scale"),  # Center and scale features
  tuneLength = 8,  # Tune hyperparameters for 8 iterations
  metric = "ROC"  # Use ROC AUC for evaluation
)

# Predict class labels on the test set
svmPrediction <- predict(svmModel, testData)

# Predict class probabilities on the test set
svmPredictionprob <- predict(svmModel, testData, type = 'prob')[2]

# Create confusion matrix to evaluate model performance
svmConfMat <- confusionMatrix(svmPrediction, testData[, "target"])

# Calculate AUC using ROC curve
AUC$svm <- roc(as.numeric(testData$target), as.numeric(as.matrix((svmPredictionprob))))

# Extract accuracy from confusion matrix
svm <- svmConfMat$overall['Accuracy']

# Random Forest Model
# Install randomForest package if not already installed (needed for Random Forest)

# Train the random forest model with 200 trees and variable importance calculation
set.seed(10)
RFModel <- randomForest(
  target ~ .,
  data = trainData,
  importance = TRUE,
  ntree = 200
)

# Predict class labels on the test set
RFPrediction <- predict(RFModel, testData)

# Predict class probabilities on the test set
RFPredictionprob <- predict(RFModel, testData, type = "prob")[, 2]

# Create confusion matrix to evaluate model performance
RFConfMat <- confusionMatrix(RFPrediction, testData[, "target"])

# Calculate AUC using ROC curve
AUC$RF <- roc(as.numeric(testData$target), as.numeric(as.matrix((RFPredictionprob))))

# Extract accuracy from confusion matrix
RF <- RFConfMat$overall['Accuracy']

# Combine AUC and Accuracy values for comparison
row.names <- names(Accuracy)
col.names <- c("AUC", "Accuracy")
cbind(as.data.frame(matrix(c(AUC, Accuracy), nrow = 3, ncol = 2, dimnames = list(row.names, col.names))))

# Print model coefficients (for logistic regression)
summary(logRegModel)$coeff

# Print confusion matrices for each model
summary(logRegModel)$coeff

RFConfMat
svmConfMat