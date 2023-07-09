# Load the required libraries
library(randomForest)
library(pdp)
library(DALEX)
library(lime)
library(shapR)
library(diceR)
library(caret)
# Load the data
df <- read.csv("https://raw.githubusercontent.com/joofio/xai-workshop/main/diabetes.csv")

df$Outcome<-as.factor(df$Outcome)
# Split the data into training and testing sets
set.seed(42)
trainIndex <- createDataPartition(df$Outcome, p = .8, list = FALSE)
train <- df[ trainIndex,]
test  <- df[-trainIndex,]

# Fit a random forest model
mc_clf <- randomForest(Outcome ~ ., data = train)

# Partial Dependence Plots
partial(mc_clf, pred.var = c("BMI", "Glucose", "BloodPressure"), plot = TRUE)

# Accumulated Local Effects Plots
ale_plot(mc_clf, var = "BMI")
ale_plot(mc_clf, var = c("Glucose", "Insulin"))

# Breakdown
explainer <- explain(mc_clf, data = train[,-9], y = train$Outcome)
my_sample <- test[1,]
prediction <- predict(explainer, newdata = my_sample)
bd <- broken(explainer, new_observation = my_sample)
plot(bd)

# Permutation Feature Importance
vip <- varImp(mc_clf)
plot(vip)

# SHAP
shap_values <- shap.values(mc_clf, X = train[,-9])
shap.plot.summary(shap_values)

# LIME
explainer <- lime(train[,-9], mc_clf)
explanation <- explain(test[1,-9], explainer, n_labels = 1, n_features = 5)
plot_features(explanation)

# Trust Scores
# There is no direct equivalent for Trust Scores in R

# Counterfactuals
# There is no direct equivalent for DiCE in R, but you can use the 'diceR' package for a similar purpose
