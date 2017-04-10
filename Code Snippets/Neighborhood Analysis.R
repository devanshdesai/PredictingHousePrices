library(ggplot2)
set.seed(1291)
train = read.csv("../Data/train.csv")
attach(train)
train = train[, -1]
test = read.csv("../Data/test.csv")
test = test[, -1]

train[,"TotalSF"] = X1stFlrSF + X2ndFlrSF + TotalBsmtSF
train[,"TotalBath"] = FullBath + HalfBath + BsmtFullBath + BsmtHalfBath
df = data.frame(aggregate(cbind(SalePrice, TotalSF, LotArea, OverallQual, Fireplaces, BedroomAbvGr, CentralAir, GarageCars, TotalBath, YearBuilt) ~ Neighborhood, data = train, FUN = mean, na.rm = TRUE))
df = na.omit(df[order(-df$SalePrice),])
df$Neighborhood = as.vector(df$Neighborhood)
df$Neighborhood = factor(df$Neighborhood, df$Neighborhood)
