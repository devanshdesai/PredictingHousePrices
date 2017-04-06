library(glmnet)
data <- read.csv("trainy_processed.csv")
test <- read.csv('test_processed.csv')
x <- data[,c(1:213)]
y <- data[,214]
fc <- sapply(names(x),function(x){class(x)})
fc
x <- lapply(x,function(x) as.numeric(as.character(x)))
fc <- sapply(names(x),function(x){class(x)})
fc
grid=10^seq(10,-2,length=100)
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)
dim(coef(ridge.mod))
ridge.mod$lambda [50]
coef(ridge.mod)[,50]
ridge.mod=glmnet(x[housedata,],y[housedata],alpha=0,lambda=grid, thresh =1e -12)
ridge.mod=glmnet(x[housedata,],y[housedata],alpha=0, lambda=grid,thresh=1e-12)
