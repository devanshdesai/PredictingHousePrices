library(class)
train <- read.csv("trainy_processed.csv")
test <- read.csv("test_processed.csv")

knn.pred=knn(train[, 1:213], test, train$y, k=10)

for (i in 1:10) {
  knn.pred= exp(knn.reg(train[, 1:213], test, train$y, k=i*5)$pred)
  filename = paste("KNN Predictions", toString(i*5), sep = "")
  write.csv(knn.pred, file = paste(filename, ".csv", sep = ""))
}

plot(knn.pred, type = "o", col="lightblue")

k_values=seq(2, 20, 2)
kaggle_scores=c(0.35927, 0.36552, 0.34928, 0.34711, 0.34132, 0.33861, 0.33943, 0.33810, 0.33806, 0.33765)
qplot(k_values, kaggle_scores, main = "KNN Predictions - Kaggle Scores vs K Values", ylab = "Kaggle Score (RMSE)", xlab = "K Values")

plot(train$YearBuilt, train$y, main = "Year Built vs Sale Price", xlab = "Year Built", ylab = "Sale Price (Normalized)", col = "darkblue")

agg.data <-aggregate(train$y, by=list(train$YrSold), FUN=mean, na.rm=TRUE)

all.data <- rbind(train[,c(1:213)], test) 

plot(agg.data1$Group.1, agg.data1$x, main = "Avg. Sale Price vs Year Built", xlab = "Year Built", ylab = "Avg. Sale Price", col = "darkblue")

library(gam)
gam1=lm(y ~ ns(YrSold)+ns(YearBuilt)+LotArea,data=train)
gam.m3=gam(y ~ s(YrSold)+s(YearBuilt)+LotArea,data=train)
par(mfrow=c(1,3))
plot(gam.m3, se=TRUE,col="blue")
plot.gam(gam1, se=TRUE, col="red")
gam.m1=gam(y ~ s(YearBuilt,5)+LotArea,data=train)
gam.m2=gam(y ~ YrSold +s(YearBuilt,5)+LotArea,data=train)
anova(gam.m1,gam.m2,gam.m3,test="F")
summary(gam.m3)
preds=predict(gam.m2,newdata=train)
gam.lo=gam(y ~ s(YrSold)+lo(YearBuilt,span=0.7)+LotArea,data=train)
plot.gam(gam.lo, se=TRUE, col="green")

