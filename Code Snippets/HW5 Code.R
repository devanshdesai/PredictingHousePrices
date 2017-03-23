########################## HW5 HOUSE DATA #####################################

###################################A
train.house = read.csv("train.csv")
miss <- rep(0,81)
for (i in 1:81) {
	miss[i] <- sum(is.na(train.house[,i]))
}
which(miss!=0)
train.NoMiss <- train.house[,miss==0]

ind <- sample(1:dim(train.NoMiss)[1],200,replace=F)

train.new <- train.NoMiss[-ind,]
test.new <- train.NoMiss[ind,]
train.new <- train.new[,-13]
test.new <- test.new[,-13]


##############B
train.house1 <- na.omit(train.house)
test.house1 <- na.omit(test.house)

train.house $OverallQual <- as.factor(train.house $OverallQual)
train.house $OverallCond <- as.factor(train.house $OverallCond)
train.house $LowQualFinSF <- as.factor(train.house $LowQualFinSF)
train.house $BsmtFullBath <- as.factor(train.house $BsmtFullBath)
train.house $BsmtHalfBath <- as.factor(train.house $BsmtHalfBath)
train.house $FullBath <- as.factor(train.house $FullBath)
train.house $HalfBath <- as.factor(train.house $HalfBath)
train.house $BedroomAbvGr <- as.factor(train.house $BedroomAbvGr)
train.house $KitchenAbvGr <- as.factor(train.house $KitchenAbvGr)
train.house $TotRmsAbvGrd <- as.factor(train.house $TotRmsAbvGrd)
train.house $Fireplaces <- as.factor(train.house $Fireplaces)
train.house $GarageCars <- as.factor(train.house $GarageCars)
train.house $ScreenPorch <- as.factor(train.house $ScreenPorch)

fit.lm <- lm(SalePrice ~ ., data=train.new)
pred.lm <- predict(fit.lm, test.new)
mean((pred.lm - test.new$SalePrice)^2)

miss <- rep(0,81)
for (i in 1:81) {
	miss[i] <- sum(is.na(train.house[,i]))
}
which(miss!=0)
train.NoMiss <- train.house[,miss==0]

miss.test <- miss[1:80]
test.NoMiss <- test.house[,miss.test==0]

miss.test2 <- rep(0,61)
for (i in 1:61) {
	miss.test2[i] <- sum(is.na(test.NoMiss[,i]))
}

##############C
train.mat <- model.matrix(SalePrice ~ ., data=train.new)
test.mat <- model.matrix(SalePrice ~ ., data=test.new)
grid <- 10^seq(10,-2,length=100)
ridge.fit <- glmnet(train.mat, train.new$SalePrice, alpha=0, lambda=grid, thresh=1e-12)
ridge.cv <- cv.glmnet(train.mat, train.new$SalePrice, alpha=0, lambda=grid, thresh=1e-12)
bestlam.ridge <- ridge.cv$lambda.min
bestlam.ridge

ridge.pred <- predict(ridge.fit, s=bestlam.ridge, newx=test.mat)
mean((ridge.pred - test.new$SalePrice)^2)

##############D
lasso.fit <- glmnet(train.mat, train.new$SalePrice, alpha=1, lambda=grid, thresh=1e-12)
lasso.cv <- cv.glmnet(train.mat, train.new$SalePrice, alpha=1, lambda=grid, thresh=1e-12)
bestlam.lasso <- lasso.cv$lambda.min
bestlam.lasso

lasso.pred <- predict(lasso.fit, s=bestlam.lasso, newx=test.mat)
mean((lasso.pred - test.new$SalePrice)^2)

lasso.coef = predict(lasso.fit, type="coefficients", s=bestlam.lasso)
lasso.coef
lasso.coef[lasso.coef!=0]

##############E
library(pls)
pcr.fit <- pcr(SalePrice ~ ., data=train.new, scale=T, validation="CV")
validationplot(pcr.fit, val.type="MSEP")

pcr.pred <- predict(pcr.fit, test.new, ncomp=10)
mean((pcr.pred - test.new$SalePrice)^2)

##############F
pls.fit <- plsr(SalePrice ~ ., data=train.new, scale=T, validation="CV")
validationplot(pls.fit, val.type="MSEP")

pls.pred <- predict(pls.fit, test.new, ncomp=10)
mean((pls.pred - test.new$SalePrice)^2)

##############G
test.avg <- mean(test.new$SalePrice)
lm.r2 <- 1 - mean((pred.lm - test.new$SalePrice)^2) / mean((test.avg - test.new$SalePrice)^2)
ridge.r2 <- 1 - mean((ridge.pred - test.new$SalePrice)^2) / mean((test.avg - test.new$SalePrice)^2)
lasso.r2 <- 1 - mean((lasso.pred - test.new$SalePrice)^2) / mean((test.avg - test.new$SalePrice)^2)
pcr.r2 <- 1 - mean((pcr.pred - test.new$SalePrice)^2) / mean((test.avg - test.new$SalePrice)^2)
pls.r2 <- 1 - mean((pls.pred - test.new$SalePrice)^2) / mean((test.avg - test.new$SalePrice)^2)

