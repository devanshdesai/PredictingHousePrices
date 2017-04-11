library(ggplot2)
set.seed(1291)
train = read.csv("../../Data/train.csv")
attach(train)
train = train[, -1]
test = read.csv("../../Data/test.csv")
test = test[, -1]

train[,"TotalSF"] = X1stFlrSF + X2ndFlrSF + TotalBsmtSF
train[,"TotalBath"] = FullBath + HalfBath + BsmtFullBath + BsmtHalfBath
df = data.frame(aggregate(cbind(SalePrice, TotalSF, LotArea, OverallQual, Fireplaces, BedroomAbvGr, CentralAir, GarageCars, TotalBath, YearBuilt) ~ Neighborhood, data = train, FUN = mean, na.rm = TRUE))
df = na.omit(df[order(df$SalePrice),])
df$Neighborhood = as.vector(df$Neighborhood)
df$Neighborhood = factor(df$Neighborhood, df$Neighborhood)

qplot(df$Neighborhood, df$SalePrice, ylab = "Average Sale Price", xlab = "Neighborhood (Asc by Average Sale Price)", main = "Average Sale Price vs Neighborhood") +  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) + geom_point(aes(colour = df$SalePrice)) + labs(colour = "Sale Price")

qplot(df$Neighborhood, df$TotalSF, ylab = "Average Square Footage", xlab = "Neighborhood (Asc by Average Sale Price)", main = "Average Square Footage vs Neighborhood") +  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) + geom_point(aes(colour = df$TotalSF)) + labs(colour = "Square Footage")
cor(df$SalePrice, df$TotalSF)

qplot(df$Neighborhood, df$LotArea, ylab = "Average Lot Area", xlab = "Neighborhood (Asc by Average Sale Price)", main = "Average Lot Area vs Neighborhood") +  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) + geom_point(aes(colour = df$LotArea)) + labs(colour = "Lot Area")
cor(df$SalePrice, df$LotArea)

qplot(df$Neighborhood, df$OverallQual, ylab = "Average Overall Quality", xlab = "Neighborhood (Asc by Average Sale Price)", main = "Average Overall Quality vs Neighborhood") +  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) + geom_point(aes(colour = df$OverallQual)) + labs(colour = "Overall Quality")
cor(df$SalePrice, df$OverallQual)

qplot(df$Neighborhood, df$Fireplaces, ylab = "Average Fireplaces", xlab = "Neighborhood (Asc by Average Sale Price)", main = "Average Fireplaces vs Neighborhood") +  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) + geom_point(aes(colour = df$Fireplaces)) + labs(colour = "Fireplaces")
cor(df$SalePrice, df$Fireplaces)

qplot(df$Neighborhood, df$BedroomAbvGr, ylab = "Average Bedrooms", xlab = "Neighborhood (Asc by Average Sale Price)", main = "Average Bedrooms vs Neighborhood") +  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) + geom_point(aes(colour = df$BedroomAbvGr)) + labs(colour = "Bedrooms")
cor(df$SalePrice, df$BedroomAbvGr)

qplot(df$Neighborhood, df$GarageCars, ylab = "Average Garage Size", xlab = "Neighborhood (Asc by Average Sale Price)", main = "Average Garage Size vs Neighborhood") +  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) + geom_point(aes(colour = df$GarageCars)) + labs(colour = "Garage Size")
cor(df$SalePrice, df$GarageCars)

qplot(df$Neighborhood, df$TotalBath, ylab = "Average Bathrooms", xlab = "Neighborhood (Asc by Average Sale Price)", main = "Average Bathrooms vs Neighborhood") +  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) + geom_point(aes(colour = df$TotalBath)) + labs(colour = "Bathrooms")
cor(df$SalePrice, df$TotalBath)

qplot(df$Neighborhood, df$YearBuilt, ylab = "Average Year Built", xlab = "Neighborhood (Asc by Average Sale Price)", main = "Average Year Built vs Neighborhood") +  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) + geom_point(aes(colour = df$YearBuilt)) + labs(colour = "Year Built")
cor(df$SalePrice, df$YearBuilt)
