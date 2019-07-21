##### Random Forest practice ####
library(reshape2)
library(ggplot2)
library(boot)
library(dplyr)
library(boot)
library(randomForest)
library(xgboost)
library(corrplot)
library(rms)
library(car) 


train = read.csv('/Users/eqiao14/Desktop/Rpractice/housing/train.csv')
test = read.csv('test.csv')

# train = read.csv("C:/Users/Edmund/Desktop/housing/train.csv")
# test = read.csv("C:/Users/Edmund/Desktop/housing/test.csv")

head(train)
attach(train)

trained = train[c('Id','LotArea', 'LandContour','Neighborhood','BldgType','HouseStyle',
                  'OverallCond','YearBuilt','YearRemodAdd', 'ExterCond', 'TotalBsmtSF','BsmtFinSF1',
                  'BsmtFinType1', 'X1stFlrSF', 'X2ndFlrSF','GrLivArea', 'BsmtFullBath', 'BsmtHalfBath',
                  'FullBath','HalfBath','BedroomAbvGr','KitchenAbvGr', 'KitchenQual', 'GarageArea', 
                  'GarageCond', 'EnclosedPorch',
                  'GarageCars','WoodDeckSF','OpenPorchSF', 'TotRmsAbvGrd', 'Fireplaces',
                  'YrSold','SaleType','SaleCondition', 'SalePrice')]

trained = replace_na_mode(trained)

head(trained)
summary(trained)

colnames(train)

ggplot(data = melt(trained), mapping = aes(x = value)) + 
  geom_histogram(bins = 30) + facet_wrap(~variable, scales = 'free_x')

###Choose numeric vectors
numbers = which(sapply(train, is.numeric))
numbers_tbl = train[,numbers]

cor_numbers = cor(numbers_tbl, use="pairwise.complete.obs")

#sort on decreasing correlations with SalePrice
cor_sorted = as.matrix(sort(cor_numbers[,'SalePrice'], decreasing = TRUE))

#select only high corelations
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.3)))
cor_numVar <- cor_numbers[CorHigh, CorHigh]

corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")

numsaleprice = numbers_tbl[CorHigh]

# ###Convert pool area to binary###
# 
# PoolArea = ifelse(PoolArea>0,1,0)
# PoolArea = as.factor(PoolArea)
#only 7 houses had pools 

# #Getting colnames
# categories = unique(Neighborhood)
# cat_housing = data.frame(Neighborhood)
# 
# #Fill dataframe with 0s and space for colname
# for (cat in categories) {
#   
#   cat_housing[,cat] = rep(0, times= nrow(cat_housing))
# }
# 
# #Loop through rows of Neighborhood
# #Grab the name of the neighborhood and set corresponding row to 1
# for(i in 1:length(cat_housing$Neighborhood)){
#   cat = as.character(cat_housing$Neighborhood[i])
#   cat_housing[,cat][i] = 1
# }
# 
# head(cat_housing)
# 
# #Delete first column 
# cat_columns = names(cat_housing)
# keep_columns = cat_columns[cat_columns != 'Neighborhood']
# cat_housing = select(cat_housing,one_of(keep_columns))
# 
# tail(cat_housing)

###Data exploration for categorical variables

#create 2x2 plots
par(mfrow=c(2,2))

cats = train[,-numbers]

plot(cats[1], xlab  =names(cats[1]))

catsaleprice = explorer(cats, c(2,2), train$SalePrice)
catsaleprice = cbind(catsaleprice, train$MiscFeature, train$SaleType, train$SaleCondition)
colnames(catsaleprice)[colnames(catsaleprice)=="train$MiscFeature"] <- "MiscFeature"
colnames(catsaleprice)[colnames(catsaleprice)=="train$SaleType"] <- "SaleType"
colnames(catsaleprice)[colnames(catsaleprice)=="train$SaleCondition"] <- "SaleCondition"

##Deal with categorical NAs, mostly replacing w/ mode or deleting if majority NA
summary(catsaleprice)
catsaleprice$MasVnrType = replace_na_mode(catsaleprice$MasVnrType)
catsaleprice$BsmtCond = replace_na_mode(catsaleprice$BsmtCond)
catsaleprice$BsmtQual = replace_na_mode(catsaleprice$BsmtQual)
catsaleprice$BsmtFinType1 = replace_na_mode(catsaleprice$BsmtFinType1)
catsaleprice$BsmtFinType2 = replace_na_mode(catsaleprice$BsmtFinType2)
catsaleprice$BsmtExposure = replace_na_mode(catsaleprice$BsmtExposure)
catsaleprice$Electrical = replace_na_mode(catsaleprice$Electrical)
catsaleprice$GarageType = replace_na_mode(catsaleprice$GarageType)
catsaleprice$GarageCond = replace_na_mode(catsaleprice$GarageCond)
catsaleprice$GarageQual = replace_na_mode(catsaleprice$GarageQual)
catsaleprice$GarageFinish = replace_na_mode(catsaleprice$GarageFinish)
catsaleprice$Alley=NULL
catsaleprice$Utilities=NULL
catsaleprice$FireplaceQu = NULL
catsaleprice$PoolQC = NULL

levels = levels(catsaleprice$Fence)
levels[length(catsaleprice$Fence) + 1] <- "None"

# refactor Species to include "None" as a factor level
# and replace NA with "None"
catsaleprice$Fence = factor(catsaleprice$Fence, levels = levels)
catsaleprice$Fence[is.na(catsaleprice$Fence)] = "None"

levels = levels(catsaleprice$MiscFeature)
levels[length(catsaleprice$MiscFeature) + 1] <- "None"

# refactor Species to include "None" as a factor level
# and replace NA with "None"
catsaleprice$MiscFeature = factor(catsaleprice$MiscFeature, levels = levels)
catsaleprice$MiscFeature[is.na(catsaleprice$MiscFeature)] = "None"


##Replace basement NAs w/ other
# catsaleprice$BsmtCond[which(is.na(catsaleprice$BsmtCond))] = as.factor('other')


##Test colinearity of categorical variables w/ vif function 

combined = cbind(numsaleprice,catsaleprice)

vif_model = lm(combined$SalePrice~., data=combined)
alias(vif_model)
###ElectrcalMix is perf colinear, will remove
combined$Electrical = NULL

vif_model = lm(combined$SalePrice~., data=combined)
car::vif(vif_model)

##VIF cats to remove: TotalBsmtSF = 8.7, Condition2 = 8.5, RoofMat1 = 9.4, Heating = 7.3
combined$TotalBsmtSF = NULL; combined$Condition2 = NULL; combined$RoofMatl = NULL; combined$Heating = NULL

combined = split_data_table(combined)

#####Continue on with unscaled model first#############

##Create the test data with same cats

trainnames = colnames(cbind(numsaleprice, catsaleprice))
trainnames = trainnames[-1]
tested = test[,trainnames]
tested$Electrical = NULL; tested$TotalBsmtSF = NULL; tested$Condition2 = NULL; tested$RoofMatl = NULL; tested$Heating = NULL
summary(tested)

levels = levels(tested$Fence)
levels[length(tested$Fence) + 1] <- "None"

# refactor Species to include "None" as a factor level
# and replace NA with "None"
tested$Fence = factor(tested$Fence, levels = levels)
tested$Fence[is.na(tested$Fence)] = "None"

levels = levels(tested$MiscFeature)
levels[length(tested$MiscFeature) + 1] <- "None"

# refactor Species to include "None" as a factor level
# and replace NA with "None"
tested$MiscFeature = factor(tested$MiscFeature, levels = levels)
tested$MiscFeature[is.na(tested$MiscFeature)] = "None"

tested$MasVnrType = replace_na_mode(tested$MasVnrType)
tested$BsmtCond = replace_na_mode(tested$BsmtCond)
tested$BsmtQual = replace_na_mode(tested$BsmtQual)
tested$BsmtFinType1 = replace_na_mode(tested$BsmtFinType1)
tested$BsmtFinType2 = replace_na_mode(tested$BsmtFinType2)
tested$BsmtExposure = replace_na_mode(tested$BsmtExposure)
tested$GarageCond = replace_na_mode(tested$GarageCond)
tested$GarageQual = replace_na_mode(tested$GarageQual)
tested$GarageFinish = replace_na_mode(tested$GarageFinish)
tested$GarageCars = replace_na_mode(tested$GarageCars)
tested$GarageArea = replace_na_mode(tested$GarageArea) ##shoudl have replaced w/ mean but it was only 1 value
tested$GarageYrBlt = replace_na_mode(tested$GarageYrBlt)
tested$MasVnrArea = replace_na_avg(tested$MasVnrArea)

tested$BsmtFinSF1 = replace_na_avg(tested$BsmtFinSF1)
##too many NAs in LotFrontage
combined$LotFrontage = NULL; tested$LotFrontage = NULL; 
tested$MSZoning = replace_na_mode(tested$MSZoning)
tested$Exterior2nd = replace_na_mode(tested$Exterior2nd)
tested$Functional = replace_na_mode(tested$Functional)
tested$SaleType = replace_na_mode(tested$SaleType)

summary(tested)

tested = split_data_table(tested)

clean_train = as.matrix(combined)
clean_test = as.matrix(tested)

##add labels to test data
salevar =rep(0,1459)
x = cbind(as.data.frame(clean_test), salevar)
clean_test = as.matrix(x)

names(x)[names(x) == "salevar"] = "SalePrice"

y = as.data.frame(clean_train)

##Reordering dataframes
x = x[,order(names(x))]
y = y[,order(names(y))]

rbind(x,y)

z = rbind (x,y)

which(z$SalePrice == 0)

a = z[1:1459,]
b = z[1460:nrow(z),]
a$SalePrice = NA 

clean_test = as.matrix(a); clean_train = as.matrix(b)