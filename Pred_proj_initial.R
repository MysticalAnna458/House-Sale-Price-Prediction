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
install.packages("plyr")
library(plyr)
install.packages("data.table")
library(data.table)
library(dplyr)

# Loading Data
train = read.csv(file.choose(),header=T,stringsAsFactors = F)
str(train)
summary(train)

# Removing NA values
training = train[,colSums(is.na(train)) == 0]
summary(training)

# Considering Important variables among the rest
var_required = c("LotArea","OverallQual","OverallCond","YearBuilt","ExterQual","ExterCond","TotalBsmtSF","HeatingQC","CentralAir","GrLivArea","BedroomAbvGr","KitchenAbvGr","KitchenQual","TotRmsAbvGrd","GarageArea","Fireplaces","OpenPorchSF","PoolArea","YrSold","SalePrice")
Model = training[,var_required]
Model
summary(Model)
Model$ExterQual = as.numeric(factor(Model$ExterQual,levels = c("Ex", "Fa","Gd", "TA","Po"),labels = c(5,2,4,3,1) ,ordered = T))
Model$ExterCond = as.numeric(factor(Model$ExterCond,levels = c("Ex", "Fa","Gd", "TA","Po"),labels = c(5,2,4,3,1) ,ordered = T))
Model$HeatingQC = as.numeric(factor(Model$HeatingQC,levels = c("Ex", "Fa","Gd", "TA","Po"),labels = c(5,2,4,3,1) ,ordered = T))
Model$KitchenQual = as.numeric(factor(Model$KitchenQual,levels = c("Ex", "Fa","Gd", "TA","Po"),labels = c(5,2,4,3,1) ,ordered = T))
Model$CentralAir = as.numeric(factor(Model$CentralAir,levels=c("N","Y"),labels=c(0,1),ordered = T))

#correlation between the variables
correl=cor(Model)
corrplot(correl)

#Splitting Data

index = sample(2,nrow(Model), replace = T, prob=c(0.7,0.3))
Model_train = Model[index==1,]
Model_test = Model[index==2,]

# Linear Regression Model
lin_model = lm(log(SalePrice)~., data=Model_train)
summary(lin_model)
pred = predict(lin_model,Model_test,type="response")
pred
accuracy(pred,log(Model_test$SalePrice))


# Random Forest
rf = randomForest(log(SalePrice)~.,data=Model_train,ntree=500,nodesize=7,na.action=na.roughfix,importance=TRUE)
rf
summary(rf)
plot(rf)
pred1 = predict(rf,Model_test,type="response")
accuracy(pred1, log(Model_test$SalePrice))
imp_rf = importance(rf)
varImpPlot(rf)  
imp_rf

#knn 
model_knn <- train(
  log(SalePrice)~., data = Model_train, method = "knn",
  trControl = trainControl("cv", number = 10),
  preProcess = c("center","scale"),
  tuneLength = 10
)

plot(model_knn)
model_knn$bestTune
predictions <- model_knn %>% predict(Model_test)
head(predictions)
# Compute the prediction error RMSE
RMSE(predictions, log(Model_test$SalePrice))
