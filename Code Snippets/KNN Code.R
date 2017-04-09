library(class)
train <- read.csv("trainy_processed.csv")
test <- read.csv("test_processed.csv")

knn.pred=knn(train[, 1:213], test, train$y, k=10)

for (i in 1:10) {
  knn.pred= exp(knn.reg(train[, 1:213], test, train$y, k=i*2)$pred)
  filename = paste("KNN Predictions", toString(i*2), sep = "")
  write.csv(knn.pred, file = paste(filename, ".csv", sep = ""))
}

plot(knn.pred, type = "o", col="lightblue")

k_values=seq(2, 20, 2)
kaggle_scores=c(0.35927, 0.36552, 0.34928, 0.34711, 0.34132, 0.33861, 0.33943, 0.33810, 18, 0.33765)
qplot(k_values, kaggle_scores, main = "KNN Predictions - Kaggle Scores vs K Values", ylab = "Kaggle Score (RMSE)", xlab = "K Values")