# Load the required libraries
library(ggplot2)
library(readr)
library(corrplot)
library(reshape2)

# Load the data
df <- read_csv("../diabetes.csv")

# View the data
head(df)

# Histogram
par(mfrow=c(4,3))
for(i in 1:ncol(df)) {
  hist(df[,i], main=names(df)[i], col='lightblue', bins=15)
}

# Pairplot
pairs(df, pch = 21, bg = c("#1b9e77", "#d95f02", "#7570b3")[unclass(df$Outcome)])

# Correlation plot
corr <- cor(df)
corrplot(corr, method="circle")

# Bar plot of Outcome
barplot(table(df$Outcome), main="Value counts of the target variable", xlab="Outcome", ylab="Count")

# Boxplots of features by Outcome
features <- names(df)[1:(ncol(df)-1)]
par(mfrow=c(2,4))
for(feature in features) {
  boxplot(df[,feature] ~ df$Outcome, main=feature, xlab="Outcome", ylab=feature)
}
