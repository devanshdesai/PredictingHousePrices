library(glmnet)
data <- read.csv("trainy_processed.csv")
test <- read.csv('test_processed.csv')
x <- data[,c(1:213)]
y <- data[,214]
lasso.mod=glmnet(x,y,alpha=1,lambda=grid)
