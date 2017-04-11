library(glmnet)
data <- read.csv("../Data/trainy_processed.csv")
test <- read.csv('../Data/test_processed.csv')
x <- data[,1:213]
y <- data[,214]

library(glmnet)
train = read.csv("../Data/trainy_processed.csv")
train = train[,-1]
test = test[, -1]
grid = 10^seq(10,-2,length=100)
cvmodel = cv.glmnet(as.matrix(train[, 1:212]), as.double(train[,213]), alpha = 1, lambda = grid)
model = glmnet(as.matrix(train[, 1:212]), as.double(train[,213]), alpha = 1, lambda = cvmodel$lambda.min)
lasso.pred=predict(model,as.matrix(test))
SalePrice=exp(lasso.pred)
ID=1461:2919
c=cbind(ID,SalePrice)
write.csv(c,file = "lassopred1.csv")

