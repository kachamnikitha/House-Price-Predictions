setwd("C:\\Users\\Nikitha\\Desktop\\GMU\\Spring - 2017\\STAT515\\project")
library(MASS)


df = read.csv('train.csv')

na_count <-sapply(df, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)



# getting columns with NA values
coln = c('LotFrontage', 'Alley',
         'BsmtQual', 'BsmtCond', 'BsmtExposure', 'BsmtFinType1', 'BsmtFinType2',
         'Electrical', 'FireplaceQu', 'GarageType', 'GarageYrBlt', 'GarageFinish',
         'GarageQual', 'GarageCond', 'PoolQC', 'Fence', 'MiscFeature')

# removing NA columns
train = df[ , !(names(df) %in% coln)]
# test = test[, !(names(test) %in% coln)]
trDF= train


# correlation
library(corrplot)

train[] <- lapply(train, function(x) as.numeric(x))
df1 <- train[1:20]
cordf <-  cor(df1)



# corplot of Imp Variables
corrplot(cordf,method = c("circle"),mar=c(1,1,1,1),title = 'Correlation Matrix')






# Feature Importance ------------------------------------------------------

# random forest
df2= df[ , colSums(is.na(df)) == 0]
library(randomForest)
model <- randomForest(SalePrice ~ . , data = df2, ntree=500, importance = T)
# Variable Importance
varImpPlot(model, sort = T, n.var = 5, scale = TRUE, title=('Importance Matrix'))
# error rate plot
plot(model)


# Regression --------------------------------------------------------------
test = train[1:200,]
train = train[201:1460,]

fit <- lm(SalePrice ~ . , data = train)
fit1 <- lm(SalePrice ~  ., data = train)
fit2 <- lm(SalePrice ~ GrLivArea , data = train)
fit3 <- lm(SalePrice ~ MSSubClass + LotArea + Street + LotShape + LandContour + 
             Utilities + LandSlope + Neighborhood + Condition2 + BldgType + 
             HouseStyle + OverallQual + OverallCond + YearBuilt + RoofStyle + 
             RoofMatl + Exterior1st + MasVnrType + MasVnrArea + ExterQual + 
             BsmtFinSF1 + HeatingQC + X1stFlrSF + X2ndFlrSF + BsmtFullBath + 
             FullBath + BedroomAbvGr + KitchenAbvGr + KitchenQual + TotRmsAbvGrd + 
             Functional + Fireplaces + GarageCars + WoodDeckSF + ScreenPorch + 
             YrSold + SaleCondition, data = train)
# Step Regression
upfit <- step(fit, direction = 'backward', steps = 100) # we see that Upfit has better Adj-R squared

modelCol = c('MSZoning', 'LotArea', 'Street', 'LandContour', 'Utilities', 'LotConfig', 
'LandSlope', 'Neighborhood', 'Condition1',
  'Condition2', 'BldgType', 'OverallQual', 'OverallCond', 'YearBuilt',
  'YearRemodAdd', 'RoofStyle', 'RoofMatl',  'Exterior1st', 'MasVnrType',  
  'MasVnrArea',  'ExterQual', 'Foundation',  'BsmtFinSF1', 'BsmtFinSF2',  
  'BsmtUnfSF',  'X1stFlrSF',  'X2ndFlrSF',  'FullBath',  'BedroomAbvGr',  
  'KitchenAbvGr',  'KitchenQual',  'Functional',  'Fireplaces',  'GarageCars',  
  'GarageArea',  'WoodDeckSF', 'OpenPorchSF', 'ScreenPorch', 'PoolArea', 
  'MoSold', 'SaleType')


# Ploting Models ----------------------------------------------------------
par(mfrow=c(2,2))
plot(fit)
plot(upfit)

# Anova -------------------------------------------------------------------
# checking best Model for prediction, put this inn your reports it is very good to have
anova(fit,upfit)

# getting the best columns from test dataset based on Step Model
#test = test[, (names(test) %in% modelCol)]

# predictions
actual = test$SalePrice
test = test[,1:63]
pred1 = predict(fit, test)
pred2 = predict(upfit, test)
test1 = df[1:200,1:80]
pred3 = predict(model, test1)

# prediction Results 
result = data.frame(actual, pred1, pred2, pred3)
result$err1 = (actual-pred1)*100/actual
result$err2 = (actual-pred2)*100/actual
result$err3 = (actual-pred3)*100/actual

# RMSE Section ------------------------------------------------------------

library(caret)
postResample(pred1,actual) # Based on Non Missing Data fed Model
postResample(pred2,actual) # Best Because of Backward Regression


postResample(pred3,actual)  # low because Random forest is done on full dataset


# GGPlot ------------------------------------------------------------------

barplot(table(df$SaleCondition), xlab= 'sale condition', ylab = 'id')

barplot(table(df$RoofStyle))

# basic graph 2
library(ggplot2)
windows(width=10, height=7)
q= ggplot(data=df, aes(x= SaleCondition , y= (SalePrice)/1000, fill = SaleType)) +  facet_grid(. ~ df$YrSold) +
  xlab('Type of Sale conditions') + ylab('Sale Price in 100K') + 
  ggtitle('House price predictions - sale conditions vs sale price') +
  geom_bar( position = "dodge", stat="identity") + theme(legend.position="top")

q + theme(axis.text.x = element_text(angle = 45, hjust = 0.8))

#windows(width=6, height=6)
#q= ggplot(data=df, aes(x= df$SaleType , y= (SalePrice)/100000, fill = df$Condition1)) + 
 # xlab('Type of Sale') + ylab('Sale Price in 100K') +
#  ggtitle('House Price Predictions - sale type vs sale price') +
 # geom_bar( position = "dodge", stat="identity") + theme(legend.position="top")
#q + theme(axis.text.x = element_text(angle = 45, hjust = 1))

#basic ggplot 

windows(width=10, height=7)
q= ggplot(data=df, aes(x=df$BldgType, y= (SalePrice)/1000, fill = Condition1)) +  facet_grid(. ~ df$YrSold) +
  xlab('Building types') + ylab('Sale price in $100k') +
  
ggtitle('House price predictions - building type vs conditions') +
  geom_bar( position = "dodge", stat="identity",color = gray(0.2)) + theme(legend.position="top")

q + theme(axis.text.x = element_text(angle = 45, hjust = 0.8))

#windows(width=6, height=6)
#q= ggplot(data=df, aes(x= df$Neighborhood , y= (SalePrice)/100000, fill = df$YearBuilt)) + 
 # xlab('Neighborhood') + ylab('Sale Price in 100K') +
  #ggtitle('House Price Predictions - neighborhood vs salepice') +
  #geom_bar(position = "dodge", stat="identity") + theme(legend.position="top")
#q + theme(axis.text.x = element_text(angle = 45, hjust = 1))


#plot
library(ggplot2)
hw <- theme_gray()+ theme(
  plot.title=element_text(hjust=0.5),
  plot.subtitle=element_text(hjust=0.5),
  plot.caption=element_text(hjust=-.5),
  
  strip.text.y = element_blank(),
  strip.background=element_rect(fill=rgb(.9,.95,1),
                                colour=gray(.5), size=.2),
  
  panel.border=element_rect(fill=FALSE,colour=gray(.70)),
  panel.grid.minor.y = element_blank(),
  panel.grid.minor.x = element_blank(),
  panel.spacing.x = unit(0.10,"cm"),
  panel.spacing.y = unit(0.05,"cm"),
  
  axis.ticks=element_blank(),
  axis.text=element_text(colour="black"),
  axis.text.y=element_text(margin=margin(0,3,0,3)),
  axis.text.x=element_text(margin=margin(-1,0,3,0))
)


scat <- ggplot(trDF, aes( x= LotArea , y=(SalePrice)/1000 ))+ xlim(1000,10000)


scat + geom_point() + stat_smooth() +hw
#Superposed scatterplot with point color encoding  Building type 

scat + geom_point( aes( color = BldgType) ) + hw

# 4.5 Juxtaposed scatterplots using facet_wrap

scat + geom_point(aes(color=BldgType))+ facet_wrap(~BldgType) + hw +
xlim(1000,10000)+ ylim(0,500)+
  labs(x="LotArea for Choosing a House in square feet",
       y="HousePrice in $100k",
       title=paste("House Price Predictions",sep="\n"),
       color="Building type") + hw
  
#kernel density plot
ggplot(data=trDF, aes(x=(SalePrice)/1000))+ 
  geom_density(fill="cyan",color="black")+ hw +
  labs(x="HousePrice in $100k",
       y="Density",
       title=paste("House Price Predictions",sep="\n")) + hw
#superposed density plot
ggplot(data=trDF, aes(x=LotArea,group=ExterQual,fill= ExterQual))+ 
  geom_density(color="black",adjust=2,alpha=.5)+
  xlim(1000,10000) + labs(x="Lot Area in square feet",
                          y="Density",
                          title=paste("House Price Predictions",sep="\n")) + hw

#HIST
ggplot(data=trDF, aes(x=trDF$GrLivArea))+ 
  geom_histogram(fill="cyan",color="black",bins=50) + labs(x="Ground Living Area in Square Feet",
                                                           y="Count",
                                                           title=paste("House Price Predictions",sep="\n"))+ hw



