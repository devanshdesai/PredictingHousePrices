###############################
# Devansh Desai
# 03/31/2017
# STAT 1291
#
# Models on Final Project Data
# Bagging, Boosting, and Random
# Forests
#
# This is the code I used for my homework 07 submission
###############################
library(randomForest)
library(gbm)
library(ggplot2)
set.seed(1291)
train = read.csv("../Data/trainy_processed.csv")
train = train[, -1]
test = read.csv("../Data/test_processed.csv")
test = test[, -1]

bag = randomForest(train$y ~., data = train, mtry = 212, trees = 20000, importance = TRUE)
predBag = exp(predict(bag, test))
write.csv(predBag, file = "Prediction CSVs/BaggingPredictions.csv")



# Here, we try 8 different mtry values for random forests using 20,000 trees each
mtry_values = seq(25, 200, 25)
for (i in 1:8) {
    rf = randomForest(train$y ~., data = train, mtry = 50, trees = 20000, importance = TRUE)
    predRF = exp(predict(rf, test))
    filename = paste("Prediction CSVs/RandomForestPredictions", toString(mtry_values[i]), sep = "")
    write.csv(predRF, file = paste(filename, ".csv", sep = ""))
}
kaggle_scores = c(0.14247, 0.14302, 0.14197, 0.14214, 0.14218, 0.14195, 0.14275, 0.14219)
pdf("Random Forest Kaggle Score Plot.pdf")
qplot(mtry_values, kaggle_scores, main = "Random Forest Model - Kaggle Scores vs Mtry Parameter", ylab = "Kaggle Score (RMSE)", xlab = "Mtry Parameter")
dev.off()


for (i in 1:10) {
    boost = gbm(train$y ~., data = train, distribution = "gaussian", n.trees = i*2000, shrinkage = 0.1)
    predBoost = exp(predict(boost, test, n.trees = i*2000))
    filename = paste("Prediction CSVs/BoostingPredictions", toString(i*2000), sep = "")
    write.csv(predBoost, file = paste(filename, ".csv", sep = ""))
}
