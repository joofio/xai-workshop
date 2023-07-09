# Load the required libraries
library(lightgbm)
library(fairmodels)
library(pROC)
library(caret)
library(dplyr)
library(ggplot2)

# Load the data
df <- read.csv('https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data', header = FALSE)

# Add headers to the data frames
names(df) <- c("age", "sex", "chest_pain_type", "trestbps", "chol", "fasting_blood_sugar", "rest_ecg", "thalach", "exercise_induced_angina", "oldpeak", "slope", "vessels_colored_by_flourosopy", "thalassemia", "diagnosis")

# Replace sex values
df$sex <- ifelse(df$sex == 1, "Male", "Female")

# Create target variable
df$target <- ifelse(df$diagnosis == 0, 0, 1)
df$diagnosis <- NULL

# Check target variable distribution
table(df$target)

# One-Hot Encode the categorical features
df <- model.matrix(~.-1, data = df)

# Split the data into training and test sets
set.seed(42)
trainIndex <- createDataPartition(df$target, p = .7, list = FALSE, times = 1)
X_train <- df[trainIndex,]
X_test <- df[-trainIndex,]

# Train the model
clf <- lightgbm(data = as.matrix(X_train[-ncol(X_train)]), 
                label = X_train$target, 
                nrounds = 100, 
                objective = "binary", 
                verbose = -1)

# Predict on test data
y_pred <- predict(clf, as.matrix(X_test[-ncol(X_test)]))
y_pred <- ifelse(y_pred > 0.5, 1, 0)

# Compute metrics
roc_obj <- roc(X_test$target, y_pred)
auc(roc_obj)

# Compute fairness metrics
fair_obj <- fairness_check(y_pred, X_test$target, X_test$sex)
print(fair_obj)

# Plot ROC curve
roc_obj <- roc(X_test$target, y_pred)
roc_obj$plot()

# Plot fairness metrics
fair_obj$plot()
