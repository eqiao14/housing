##### XGBoost and Random Forest practice ####
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

################Comtinuing on in file xgboostpt2.R###############################

##use split func from helpers 
detach(train)
attach(trained)
Neighborhood_split = split(Neighborhood)
SaleCondition_split = split(SaleCondition)
SaleType_split = split(SaleType)
YrSold_split = split(as.factor(YrSold))
#MoSold_split = split(as.factor(MoSold))
#Replace variables not in test
HouseStyle = replace_any_mode(HouseStyle, '2.5Fin')
HouseStyle_split = split(HouseStyle)
BldgType_split = split(BldgType)
YearBuilt_split = split(as.factor(YearBuilt))
YearRemodAdd_split = split(as.factor(YearRemodAdd))

LandContour_split = split(LandContour)
ExterCond_split  = split(ExterCond)
BsmtFintyp1_split = split(BsmtFinType1)
KitchenQual_split = split(KitchenQual)
GarageCond_split = split(GarageCond)


#Create dataframe w/ only the numericals 
include = c('LotArea','OverallCond','FullBath','HalfBath','BedroomAbvGr','KitchenAbvGr','GarageCars',
'WoodDeckSF','OpenPorchSF', 'TotalBsmtSF', 'BsmtFinSF1', 'X1stFlrSF', 'X2ndFlrSF',
'GrLivArea', 'BsmtFullBath', 'BsmtHalfBath', 'GarageArea', 'EnclosedPorch', 'TotRmsAbvGrd',
'Fireplaces')
trained_num =  trained[ , (names(trained) %in% include)]

scaled_trained_num = scale(trained_num)
head(scaled_trained_num)

clean_train = cbind(scaled_trained_num, Neighborhood_split, SaleCondition_split,SaleType_split,
      YrSold_split, HouseStyle_split, BldgType_split, LandContour_split, ExterCond_split,
      BsmtFintyp1_split, KitchenQual_split, GarageCond_split, SalePrice)

###transform test data too#############
detach(trained)
attach(test)

tested = test[c('Id','LotArea', 'LandContour','Neighborhood','BldgType','HouseStyle',
                'OverallCond','YearBuilt','YearRemodAdd', 'ExterCond', 'TotalBsmtSF','BsmtFinSF1',
                'BsmtFinType1', 'X1stFlrSF', 'X2ndFlrSF','GrLivArea', 'BsmtFullBath', 'BsmtHalfBath',
                'FullBath','HalfBath','BedroomAbvGr','KitchenAbvGr', 'KitchenQual', 'GarageArea', 
                'GarageCond', 'EnclosedPorch',
                'GarageCars','WoodDeckSF','OpenPorchSF', 'TotRmsAbvGrd', 'Fireplaces',
                'YrSold','SaleType','SaleCondition')]

tested = replace_na_mode(tested)

##replace some variables not found in training model
tested = replace_any_mode(tested, '2.5Fin')

detach(test)
attach(tested)

Neighborhood_split = split(Neighborhood)
SaleCondition_split = split(SaleCondition)
SaleType_split = split(SaleType)
YrSold_split = split(as.factor(YrSold))
#MoSold_split = split(as.factor(MoSold))
HouseStyle_split = split(tested$HouseStyle)
BldgType_split = split(BldgType)
YearBuilt_split = split(as.factor(YearBuilt))
YearRemodAdd_split = split(as.factor(YearRemodAdd))
#New predictors
LandContour_split = split(LandContour)
ExterCond_split  = split(ExterCond)
BsmtFintyp1_split = split(BsmtFinType1)
KitchenQual_split = split(KitchenQual)
GarageCond_split = split(GarageCond)




include = c('LotArea','OverallCond','FullBath','HalfBath','BedroomAbvGr','KitchenAbvGr','GarageCars',
            'WoodDeckSF','OpenPorchSF', 'TotalBsmtSF', 'BsmtFinSF1', 'X1stFlrSF', 'X2ndFlrSF',
            'GrLivArea', 'BsmtFullBath', 'BsmtHalfBath', 'GarageArea', 'EnclosedPorch', 'TotRmsAbvGrd',
            'Fireplaces')
tested_num =  tested[ , (names(tested) %in% include)]

scaled_tested_num = scale(tested_num)
head(scaled_tested_num)

clean_test = cbind(scaled_tested_num, Neighborhood_split, SaleCondition_split,SaleType_split,
                   YrSold_split, HouseStyle_split, BldgType_split, LandContour_split, ExterCond_split,
                   BsmtFintyp1_split, KitchenQual_split, GarageCond_split)


###random forest

set.seed(1738)

####Make any illegal names legal
####ie years like '2008'
names(clean_train) = make.names(names(clean_train))
names(clean_test) = make.names(names(clean_test))


rf_model = randomForest(clean_train$SalePrice~., data = clean_train, ntree=500,
                        importance = TRUE)

#mse of training data
oob_prediction = predict(rf_model)
train_mse = mean(as.numeric((oob_prediction - trained$SalePrice)^2))
oob_rmse = sqrt(train_mse)
oob_rmse


rf_predict = predict(rf_model, newdata = clean_test)

rf_model$importance

write.csv(rf_predict, 'housing_predictions.csv')

###xgboost

set.seed(2995)

# put our testing & training data into two seperates Dmatrixs objects
clean_train = as.matrix(clean_train)
clean_test = as.matrix(clean_test)

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

#splitting up labels and training data
labels = b[,which(colnames(z) == 'SalePrice')]
clean_train = b[,-(which(colnames(z) == 'SalePrice'))]
clean_train = as.matrix(clean_train)
clean_test = a[,-(which(colnames(z) == 'SalePrice'))]
clean_test = as.matrix(clean_test)

###Create new train/test to run internally
set.seed(2995)
sample = sample.int(n = nrow(clean_train),
                    size = floor(0.8*nrow(clean_test)), replace = FALSE) 

sample_train = cbind(clean_train[sample,], labels[sample])
sample_test = cbind(clean_train[-sample,], labels[-sample])

#Label is what we're training target of interest is
dtrain = xgb.DMatrix(data = clean_train, label = labels)
dtest = xgb.DMatrix(data = clean_test)

# dtrain = xgb.DMatrix(data = sample_train, label = sample_train[,which(colnames(sample_test) == '')])
# dtest = xgb.DMatrix(data = sample_test, label = sample_test[,which(colnames(sample_test) == '')])

# train a model using our training data
model <- xgboost(data = dtrain, # the data   
                 nround = 400,  # max number of boosting iterations
                 early_stopping_rounds = 5
                ) 

pred = predict(model, dtest)

train_mse = mean((pred - sample_test[,which(colnames(sample_test) == '')])^2)
oob_rmse = sqrt(train_mse)
oob_rmse

#Training error of 4336.48 correlates with error of 0.221 on the scoreboard

csv = as.data.frame(cbind(1461:2919, pred))
colnames(csv) = c('Id', 'SalePrice')

write.csv(csv, 'housing_predictions.csv')




