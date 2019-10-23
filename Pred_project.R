# installing Packages
install.packages("readr")
library(readr)
install.packages("ggplot2")
library(ggplot2)
install.packages("corrplot")
library(corrplot)
install.packages("forecast")
library(forecast)
install.packages("randomForest")
library(randomForest)
install.packages("caret")
library(caret)
install.packages("xgboost")
library(xgboost)
install.packages("plyr")
library(plyr)
install.packages("randomForest")
library(randomForest)
install.packages("forecast")
library(forecast)

# Loading Data
train = read.csv(file.choose(),header=T,stringsAsFactors = F)
str(train)
summary(train)

# Checking data for NA's
nas_present = which(colSums(is.na(train)) > 0)
colSums(sapply(train[nas_present], is.na))

# Exploratory Data Analysis
hist(train$SalePrice,breaks=40, col="blue",xlab ="SalePrice",main="Histogram of Sales Price")
hist(log(train$SalePrice),breaks=40, col="blue",xlab ="SalePrice",main="Histogram of log(Sales Price)")

#Correlation Matrix
numericv <-(which(sapply(train, is.numeric)) )
numeric_train<-train[,numericv]
cor(numeric_train)

#visualization of high correlated variables
corpairnumVar <- cor(numeric_train, use="pairwise.complete.obs")
cor_sorted <- as.matrix(sort(corpairnumVar[,'SalePrice'], decreasing = TRUE))
varhighcor <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5)))
corpairnumVar <- corpairnumVar[varhighcor, varhighcor]
corrplot.mixed(corpairnumVar,addCoef.col = "maroon", tl.col="maroon", tl.pos = "lt", plotCI = c("n"))

#SalesPrice vs High Correlated Variables
ggplot(train, aes(x=train$OverallQual, y=SalePrice))+geom_point(colour="blue")
ggplot(train, aes(x=train$GrLivArea, y=SalePrice))+geom_point(colour="blue")
ggplot(train, aes(x=train$GarageCars, y=SalePrice))+geom_point(colour="blue")
ggplot(train, aes(x=train$GarageArea, y=SalePrice))+geom_point(colour="blue")
ggplot(train, aes(x=train$TotalBsmtSF, y=SalePrice))+geom_point(colour="blue")
ggplot(train, aes(x=train$X1stFlrSF, y=SalePrice))+geom_point(colour="blue")
ggplot(train, aes(x=train$FullBath, y=SalePrice))+geom_point(colour="blue")
ggplot(train, aes(x=train$TotalBsmtSF, y=SalePrice))+geom_point(colour="blue")
ggplot(train, aes(x=train$YearBuilt, y=SalePrice))+geom_point(colour="blue")
ggplot(train, aes(x=train$YearRemodAdd, y=SalePrice))+geom_point(colour="blue")


#Imputing NA values for all the columns

# Lot Frontage

for (i in 1:nrow(train)){
if(is.na(train$LotFrontage[i])){
train$LotFrontage[i] <- as.numeric(median(train$LotFrontage[train$Neighborhood==train$Neighborhood[i]], na.rm=TRUE)) 
  }
}

# Alley
train$Alley[is.na(train$Alley)] = 'None'
train$Alley =as.factor(train$Alley)
table(train$Alley)

# Mason Veneer Type
length(which(is.na(train$MasVnrType)&is.na(train$MasVnrArea)))
train[is.na(train$MasVnrType) & !is.na(train$MasVnrArea), c('MasVnrType', 'MasVnrArea')]

train$MasVnrType[is.na(train$MasVnrType)] = 'None'
table(train$MasVnrType)

# Mason Veneer Area
train$MasVnrArea[is.na(train$MasVnrArea)] = 0

# Basement Quality
train$BsmtQual[is.na(train$BsmtQual)] = names(sort(table(train$BsmtQual)))[4]
table(train$BsmtQual)

# Basement Condition
train$BsmtCond[is.na(train$BsmtCond)] = names(sort(table(train$BsmtCond)))[4]
table(train$BsmtCond)    

# Basement Exposure
train$BsmtExposure[is.na(train$BsmtExposure)]=names(sort(table(train$BsmtExposure)))[4]
table(train$BsmtExposure)

# Basement Finish Type 1
train$BsmtFinType1[is.na(train$BsmtFinType1)] =names(sort(table(train$BsmtFinType1)))[6]
table(train$BsmtFinType1)

# Basement Finish Type 2
train$BsmtFinType2[is.na(train$BsmtFinType2)] = names(sort(table(train$BsmtFinType2)))[6]
table(train$BsmtFinType2)

# Electrical
train$Electrical[is.na(train$Electrical)] =  names(sort(table(train$Electrical)))[5]
table(train$Electrical)  

# Fire Place Quality
length(which(is.na(train$FireplaceQu)&(is.na(train$Fireplaces))))
table(train$FireplaceQu)
train$FireplaceQu[is.na(train$FireplaceQu)] = 'None'
table(train$FireplaceQu)

# Garage Type
train$GarageType[is.na(train$GarageType)] = 'No Garage'
table(train$GarageType)

# Garage Year Built
train$GarageYrBlt[is.na(train$GarageYrBlt)] = train$YearBuilt[is.na(train$GarageYrBlt)]

# Garage Finish
train$GarageFinish[is.na(train$GarageFinish)] = 'None'
table(train$GarageFinish)

# Garage Quality
train$GarageQual[is.na(train$GarageQual)] = 'None'
table(train$GarageQual)

# Garage Condition
train$GarageCond[is.na(train$GarageCond)] = 'None'
table(train$GarageCond)

# Pool Quality
train$PoolQC[is.na(train$PoolQC)] = 'None'
table(train$PoolQC)

# Fence
train$Fence[is.na(train$Fence)] = 'No Fence'
table(train$Fence)

# Miscellaneous Features
train$MiscFeature[is.na(train$MiscFeature)] = 'None'
table(train$MiscFeature)

summary(train)

# Converting Categorical Variables into Fcators for analysis
train$MSZoning = as.factor(train$MSZoning)
table(train$MSZoning)

train$Street=as.numeric(factor(train$Street, levels=c("Grvl","Pave"),labels=c(1,2),ordered = T))
table(train$Street)

train$Alley = as.factor(train$Alley)

train$LotShape=as.numeric(factor(train$LotShape, levels = c('IR3', 'IR2', 'IR1', 'Reg'),labels=c(1,2,3,4),ordered=T))
table(train$LotShape)
train$LotShape

str(train$LandContour)
train$LandContour = as.factor(train$LandContour)
table(train$LandContour)

str(train$Utilities)
train$Utilities = as.factor(train$Utilities)
table(train$Utilities)

train$LotConfig = as.factor(train$LotConfig)
table(train$LotConfig)

train$LandSlope<-as.numeric(factor(train$LandSlope, levels=c('Sev', 'Mod', 'Gtl'),labels=c(1,2,3),ordered = T))
table(train$LandSlope)

train$Neighborhood = as.factor(train$Neighborhood)
table(train$Neighborhood)

train$Condition1 = as.factor(train$Condition1)
table(train$Condition1)

train$Condition2 = as.factor(train$Condition2)
table(train$Condition2)

train$BldgType = as.factor(train$BldgType)
table(train$BldgType)

train$HouseStyle = as.factor(train$HouseStyle)
table(train$HouseStyle)

train$RoofStyle = as.factor(train$RoofStyle)
table(train$HouseStyle)

train$RoofMatl = as.factor(train$RoofMatl)
table(train$RoofMatl)

train$Exterior1st = as.factor(train$Exterior1st)
table(train$Exterior1st)

train$Exterior2nd = as.factor(train$Exterior2nd)
table(train$Exterior2nd)

train$MasVnrType = as.factor(train$MasVnrType)
table(train$MasVnrType)

train$ExterQual = as.numeric(factor(train$ExterQual,levels = c("Ex", "Fa","Gd", "TA","Po"),labels = c(5,2,4,3,1) ,ordered = T))
table(train$ExterQual)

train$ExterCond =  as.numeric(factor(train$ExterCond,levels = c("Ex", "Fa","Gd", "TA","Po"),labels = c(5,2,4,3,1) ,ordered = T))
table(train$ExterCond)

train$Foundation = as.factor(train$Foundation)
table(train$Foundation)

train$BsmtQual = as.numeric(factor(train$BsmtQual,levels = c("Ex", "Fa","Gd", "TA","Po"),labels = c(5,2,4,3,1) ,ordered = T))
table(train$BsmtQual)

train$BsmtCond = as.numeric(factor(train$BsmtCond,levels = c("Ex", "Fa","Gd", "TA","Po"),labels = c(5,2,4,3,1) ,ordered = T))
table(train$BsmtCond)


train$BsmtExposure = as.factor(train$BsmtExposure)
table(train$BsmtExposure)

train$BsmtFinType1 = as.factor(train$BsmtFinType1)
table(train$BsmtFinType1)

train$BsmtFinType2 = as.factor(train$BsmtFinType2)
table(train$BsmtFinType2)

train$Heating = as.factor(train$Heating)
table(train$Heating)

train$HeatingQC =  as.numeric(factor(train$HeatingQC,levels = c("Ex", "Fa","Gd", "TA","Po"),labels = c(5,2,4,3,1) ,ordered = T))
table(train$HeatingQC)

train$CentralAir = as.numeric(factor(train$CentralAir,levels=c("N","Y"),labels=c(0,1),ordered = T))
table(train$CentralAir)

train$Electrical = as.factor(train$Electrical)
table(train$Electrical)

train$KitchenQual = as.numeric(factor(train$KitchenQual,levels = c("Ex", "Fa","Gd", "TA","Po"),labels = c(5,2,4,3,1) ,ordered = T))
table(train$KitchenQual)

train$Functional = as.factor(train$Functional)
table(train$Functional)

train$FireplaceQu = as.numeric(factor(train$FireplaceQu,levels = c("Ex", "Fa","Gd", "TA","Po","None"),labels = c(6,3,5,4,2,1) ,ordered = T))
table(train$FireplaceQu)

train$GarageType = as.factor(train$GarageType)
table(train$GarageType)

train$GarageFinish = as.factor(train$GarageFinish)
table(train$GarageFinish)

train$GarageQual =  as.numeric(factor(train$GarageQual,levels = c("Ex", "Fa","Gd", "TA","Po","None"),labels = c(6,3,5,4,2,1) ,ordered = T))
table(train$GarageQual)

train$GarageCond =  as.numeric(factor(train$GarageCond,levels = c("Ex", "Fa","Gd", "TA","Po","None"),labels = c(6,3,5,4,2,1) ,ordered = T))
table(train$GarageCond)

train$PavedDrive = as.factor(train$PavedDrive)
table(train$PavedDrive)

Qualities = c('None' = 0, 'Fa' = 2, 'Gd' = 4, 'Ex' = 5)
train$PoolQC = as.integer(revalue(train$PoolQC, Qualities))
table(train$PoolQC)

train$Fence = as.factor(train$Fence)
table(train$Fence)

train$MiscFeature = as.factor(train$MiscFeature)
table(train$MiscFeature)

train$SaleType = as.factor(train$SaleType)
table(train$SaleType)

train$SaleCondition = as.factor(train$SaleCondition)
table(train$SaleCondition)

# Log transforming the Sale Price
Model = train[,-1]
Model$SalePrice = log(Model$SalePrice)

# Splitting Data
index = sample(2,nrow(Model), replace = T, prob=c(0.7,0.3))
Model_train = Model[index==1,]
Model_test = Model[index==2,]

#knn
model_knn <- train(
  SalePrice~., data = Model_train, method = "knn",
  trControl = trainControl("cv", number = 10),
  preProcess = c("center","scale"),
  tuneLength = 10
)

plot(model_knn)
model_knn$bestTune
predictions <- model_knn %>% predict(Model_test)
head(predictions)
# Compute the prediction error RMSE
RMSE(predictions, Model_test$SalePrice)


# Random Forest
rf = randomForest(SalePrice~.,data=Model_train,ntree=500,nodesize=7,na.action=na.roughfix,importance=TRUE)
rf
plot(rf)
pred1 = predict(rf,Model_test,type="response")
accuracy(pred1, (Model_test$SalePrice))
imp_rf = importance(rf)
varImpPlot(rf)  

# One hot encoding
library(data.table)
library(mltools)
Model_train_1h = one_hot(as.data.table(Model_train))
Model_test_1h = one_hot(as.data.table(Model_test))

# Xtreme Gradient boosting hyper parameter tuning
tune_grid = expand.grid(
  nrounds = 1000,
  eta = c(0.1,0.3, 0.05, 0.01),
  max_depth = c(2, 3, 4, 5, 6),
  gamma = 0,
  colsample_bytree=1,
  min_child_weight=c(1, 2, 3, 4 ,5),
  subsample=1)
ControlParameters = trainControl(method = "cv",
                                  number = 5)

modelxgboost = train(SalePrice~., 
                      data = Model_train_1h,
                      method = "xgbTree",
                      trControl = ControlParameters,
                      tuneGrid=tune_grid)
modelxgboost$bestTune

# Xtreme Gradient Boosting
dtrain = xgb.DMatrix(data = as.matrix(Model_train_1h[,c(1:252)]),label=as.matrix(Model_train_1h[,253]))
dtest = xgb.DMatrix(data = as.matrix(Model_test_1h[,c(1:252)]),label=as.matrix(Model_test_1h[,253]))

# Xtreme Gradient Boosting with default parameters
default_param<-list(
  objective = "reg:linear",
  booster = "gbtree",
  eta=0.3, #default = 0.3
  gamma=0,
  max_depth=6, #default=6
  min_child_weight=1, #default=1
  subsample=1,
  colsample_bytree=1,eval_metric="rmse"
)

xgbcv = xgb.cv( params = default_param, data = dtrain, nrounds = 1000, nfold = 5, showsd = T, stratified = T, print_every_n = 40, early_stopping_rounds = 10, maximize = F)
xgb_mod = xgb.train(data = dtrain, params=default_param, nrounds = xgbcv$best_iteration)
xgbpred = predict(xgb_mod, dtest,type="response")
head(xgbpred)
accuracy(xgbpred,Model_test_1h$SalePrice)


# Xtreme Gradient Boosting with Hyper Tuned Parameters
Controlled_param =list(
  objective = "reg:linear",
  booster = "gbtree",
  eta=0.05, #default = 0.3
  gamma=0,
  max_depth=3, #default=6
  min_child_weight=3, #default=1
  subsample=1,
  colsample_bytree=1,eval_metric="rmse"
)

xgbcv1 = xgb.cv( params = Controlled_param, data = dtrain, nrounds = 1000, nfold = 5, showsd = T, stratified = T, print_every_n = 40, early_stopping_rounds = 10, maximize = F)
xgb_mod1 = xgb.train(data = dtrain, params=Controlled_param, nrounds = xgbcv1$best_iteration)
xgbpred1 = predict(xgb_mod1,dtest,type="response")
head(xgbpred1)
accuracy(xgbpred1,Model_test_1h$SalePrice)

