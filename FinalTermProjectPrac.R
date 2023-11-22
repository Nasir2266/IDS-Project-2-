MyData <- read.csv("E:/BrainStroke.csv", header=TRUE,sep=",")
options(max.print=9999)
MyData


convertCategoricalToNumeric <- function(MyData)
{
  categorical_cols <- c("gender", "ever_married", "work_type", "Residence_type", "smoking_status")
  for(col in categorical_cols)
  {
    MyData[[col]] <- as.numeric(factor(MyData[[col]]))
  }
  return(MyData)
}

MyData <- data.frame(MyData)

MyData <- convertCategoricalToNumeric(MyData)
MyData

colSums(is.na(MyData))

z_scores <- scale(MyData$age)
age_outlier_indices <- which(abs(z_scores) > 2)
age_outlier_values <- MyData$age[age_outlier_indices]
age_outlier_values

MyData$age[age_outlier_indices] <- NA
MyData

which(is.na(MyData$age))

most_frequent_age <- names(which.max(table(MyData$age)))
MyData$age[is.na(MyData$age)] <- most_frequent_age
most_frequent_age
myFrequencyAgeData = MyData
myFrequencyAgeData




MyData$age <- as.numeric(MyData$age)
MyData$stroke <- as.numeric(MyData$stroke)
MyData$stroke <- as.numeric(MyData$hypertension)


MyData


names(MyData)
class(MyData$age)
class(MyData$stroke)
class(MyData$hypertension)

MyData2 <-data.frame(ageRelation = MyData$age, strokeRelatrion = MyData$stroke )
MyData2

correlation_coefficient <- cor(MyData2$ageRelation, MyData2$strokeRelatrion)
correlation_coefficient



correlation_matrix <- round(cor(MyData, method = "pearson"),digits = 2)
correlation_matrix

install.packages("corrplot")
library(corrplot)
corrplot(correlation_matrix, method = "color")
corrplot

#
# Load required packages
library(class)

#
install.packages("caTools")
library(caTools)
#set.seed(20)
# Creating training and testing datasets
split <- sample.split(myFrequencyAgeData$stroke, SplitRatio = 0.8)


train_data <- myFrequencyAgeData[split, ]
nrow(train_data)
test_data <- myFrequencyAgeData[!split, ]
nrow(test_data)
#

# Build KNN model
train_data <- MyData[c("age", "stroke")]
test_data <- data.frame(age = MyData$age, stroke = MyData$stroke)  # Example test data

# Set the number of neighbors for KNN
k <- 3

# Predict using KNN
knn_pred <- knn(train = train_data, test = test_data, cl = MyData$stroke, k = k)

# Print KNN predictions
print(knn_pred)

# 10 fold cross validation
num_folds <- 10

fold_indices <- sample(1:num_folds, size = nrow(MyData), replace = TRUE)
fold_indices
 

for (fold in 1:num_folds) {
  train_indices <- which(fold_indices != fold)
  test_indices <- which(fold_indices == fold)
  
  train_data <- MyData[train_indices, c("age", "stroke")]
  test_data <- MyData[test_indices, c("age", "stroke")]
  
  knn_pred <- knn(train = train_data, test = test_data, cl = MyData$stroke[train_indices], k = k)
  
  correct_predictions <- sum(knn_pred == MyData$stroke[test_indices])
  total_predictions <- length(test_indices)
  
  accuracy_scores[fold] <- correct_predictions / total_predictions
}

# Print accuracy scores for each fold
print(accuracy_scores)

#confusion matrix
conf_matrix <- table(Actual = test_data$stroke, Predicted = knn_pred)
print(conf_matrix)

install.packages("caret")
install.packages("irr")
library(caret)
library(irr)

kappa_result <- kappa2(data.frame(Actual = test_data$stroke, Predicted = knn_pred))
print(paste("Cohen's Kappa:", kappa_result$value))

# Calculate accuracy, precision, recall, and F1-score
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
precision <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
recall <- conf_matrix[2, 2] / sum(conf_matrix[2, ])

print(paste("Accuracy:", accuracy))
print(paste("Precision:", precision))
print(paste("Recall:", recall))



#


