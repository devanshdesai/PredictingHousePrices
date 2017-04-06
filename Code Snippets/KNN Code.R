library(class)
train <- read.csv("trainy_processed.csv")
test <- read.csv("test_processed.csv")

knn.pred=knn(train[, 1:213], test, train$y, k=10)

for (i in 1:10) {
  knn.pred= exp(knn.reg(train[, 1:213], test, train$y, k=i*2)$pred)
  filename = paste("KNN Predictions", toString(i*2), sep = "")
  write.csv(knn.pred, file = paste(filename, ".csv", sep = ""))
}