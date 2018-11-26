library(caret)
library(mice)
library(glmnet)
library(dplyr)
setwd("E:/Analytics/Analytics Vidhya Datahack/24 Case Study Challenge/Walmart Data")
train<-read.csv("Train_UWu5bXk.csv")
train[train==""]<-NA
ss<-read.csv("SampleSubmission_TmnO39y.csv")
train_2<-read.csv("Train_UWu5bXk.csv")

#make updates to the table colums
train$Item_Fat_Content[train$Item_Fat_Content=="LF"]<-"Low Fat"
train$Item_Fat_Content[train$Item_Fat_Content=="low fat"]<-"Low Fat"
train$Item_Fat_Content[train$Item_Fat_Content=="reg"]<-"Regular"

train$Outlet_Establishment_Year<-as.integer(format(Sys.Date(),"%Y"))-train$Outlet_Establishment_Year

#convert the train categorical variables into dummy variables
train$Item_Fat_Content<-as.factor(as.numeric(train$Item_Fat_Content,na.rm=T))
train$Item_Type<-as.factor(as.numeric(train$Item_Type,na.rm=T))
train$Outlet_Size<-as.factor(as.numeric(train$Outlet_Size,na.rm=T))
train$Outlet_Size<-as.factor(as.numeric(train$Outlet_Size,na.rm=T))
train$Outlet_Location_Type<-as.factor(as.numeric(train$Outlet_Location_Type,na.rm=T))
train$Outlet_Location_Type<-as.factor(as.numeric(train$Outlet_Location_Type,na.rm=T))
train$Outlet_Type<-as.factor(as.numeric(train$Outlet_Type,na.rm=T))
train_identifer<-train[,c(1,7)] #select the outlet and item identifier so that they can be removed
train<-train[,-c(1,7)]

micemod<-mice(as.data.frame(train),method = "rf")
train<-complete(micemod)
dmy<-dummyVars("~.",data=train)
train<-data.frame(predict(dmy,newdata=train))
train<-train[,-c(3,20,25,28,32)]

# train_tf<-train
# train_tf$Item_Weight<-log10(train_tf$Item_Weight)
# train_tf$Item_MRP<-log10(train_tf$Item_MRP)
# train_tf$Outlet_Establishment_Year<-log10(train_tf$Outlet_Establishment_Year)
# train_tf$Item_Outlet_Sales<-log10(train_tf$Item_Outlet_Sales)

#convert the test categorical variables into dummy variables
test<-read.csv("Test_u94Q5KV.csv")
test[test==""]<-NA
#make updates to colums
test$Item_Fat_Content[test$Item_Fat_Content=="LF"]<-"Low Fat"
test$Item_Fat_Content[test$Item_Fat_Content=="low fat"]<-"Low Fat"
test$Item_Fat_Content[test$Item_Fat_Content=="reg"]<-"Regular"
test$Outlet_Establishment_Year<-test$Outlet_Establishment_Year-min(test$Outlet_Establishment_Year)

test$Item_Fat_Content<-as.factor(as.numeric(test$Item_Fat_Content,na.rm=T))
test$Item_Type<-as.factor(as.numeric(test$Item_Type,na.rm=T))
test$Outlet_Size<-as.factor(as.numeric(test$Outlet_Size,na.rm=T))
test$Outlet_Size<-as.factor(as.numeric(test$Outlet_Size,na.rm=T))
test$Outlet_Location_Type<-as.factor(as.numeric(test$Outlet_Location_Type,na.rm=T))
test$Outlet_Location_Type<-as.factor(as.numeric(test$Outlet_Location_Type,na.rm=T))
test$Outlet_Type<-as.factor(as.numeric(test$Outlet_Type,na.rm=T))
test_identifer<-test[,c(1,7)] #select the outlet and item identifier so that they can be removed
test<-test[,-c(1,7)]




micemod<-mice(as.data.frame(test),method = "rf")
test<-complete(micemod)

dmy<-dummyVars("~.",data=test)
test<-data.frame(predict(dmy,newdata=test))

test<-test[,-c(6,23,28,31,35)]
test$Outlet_Establishment_Year<-as.integer(format(Sys.Date(),"%Y"))-test$Outlet_Establishment_Year
################################################################################################################
#Non_penalized linear regression
trainlm<-lm(Item_Outlet_Sales~.,data = tr_data)
coef(trainlm)
summary(trainlm) #R^2 adjusted value of 0.5635

###############################################################################################
#Computing Ridge Regression
training.sample<-train$Item_Outlet_Sales %>%
  createDataPartition(p=0.8,list = F)
tr_data<-train[training.sample,]
ts_data<-train[-training.sample,]


#predictor variables
x <- model.matrix(Item_Outlet_Sales~., tr_data)[,-1]#removing the intercept colum from the matrix
y <- tr_data$Item_Outlet_Sales
#lambda <- 10^seq(10, -2, length = 100)


# find the best lambda using cross-validaton
set.seed(123)
cv<-cv.glmnet(x,y,alpha = 0)

cv$lambda.min

#fit the model on the training data
model<-glmnet(x,y,alpha = 0,lambda=cv$lambda.min)
#coef(model)

x.test<-model.matrix(Item_Outlet_Sales~.,ts_data)[,-1]
predictions<-model %>% predict(x.test)%>% as.vector()

data.frame(
  RMSE=RMSE(predictions,ts_data$Item_Outlet_Sales),
  Rsquared=R2(predictions,ts_data$Item_Outlet_Sales)
) #RMSE - 1145.96; RSQUARED - 0.5477
##############################################################################################
#computing Lasso Regression
set.seed(123)
cv<-cv.glmnet(x,y,alpha = 1)

cv$lambda.min

# Fit the final model on the training data
model <- glmnet(x, y, alpha = 1, lambda = cv$lambda.min)
# Dsiplay regression coefficients
coef(model)

x.test <- model.matrix(Item_Outlet_Sales ~., ts_data)[,-1]
predictions <- model %>% predict(x.test) %>% as.vector()
# Model performance metrics
data.frame(
  RMSE = RMSE(predictions, ts_data$Item_Outlet_Sales),
  Rsquare = R2(predictions, ts_data$Item_Outlet_Sales)
)#RMSE - 1138.25   Rsquare - 0.5527555


################################################################################################
#Build the model using the training set
set.seed(123)
model<-train(Item_Outlet_Sales~., data = tr_data, method = "glmnet",
             trControl = trainControl("cv",number = 10), tuneLength = 10)
# Best tuning parameter
model$bestTune

coef(model$finalModel, model$bestTune$lambda)

# Make predictions on the test data
x.test <- model.matrix(Item_Outlet_Sales ~., ts_data)[,-1]
predictions <- model %>% predict(x.test)
# Model performance metrics
data.frame(
  RMSE = RMSE(predictions, ts_data$Item_Outlet_Sales),
  Rsquare = R2(predictions, ts_data$Item_Outlet_Sales)
)

############################################################################################
lambda <- 10^seq(-3, 3, length = 100)
set.seed(123)
ridge <- train(
  Item_Outlet_Sales ~., data = tr_data, method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneGrid = expand.grid(alpha = 0, lambda = lambda)
)
# Model coefficients
coef(ridge$finalModel, ridge$bestTune$lambda)
# Make predictions
predictions <- ridge %>% predict(ts_data)
# Model prediction performance
data.frame(
  RMSE = RMSE(predictions, ts_data$Item_Outlet_Sales),
  Rsquare = R2(predictions, ts_data$Item_Outlet_Sales)
)
ridge$bestTune$lambda

set.seed(123)
lasso <- train(
  Item_Outlet_Sales ~., data = tr_data, method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneGrid = expand.grid(alpha = 1, lambda = lambda)
)
# Model coefficients
coef(lasso$finalModel, lasso$bestTune$lambda)
# Make predictions
predictions <- lasso %>% predict(ts_data)
# Model prediction performance
data.frame(
  RMSE = RMSE(predictions, ts_data$Item_Outlet_Sales),
  Rsquare = R2(predictions, ts_data$Item_Outlet_Sales)
)
lasso$bestTune$lambda

set.seed(123)
elastic <- train(
  Item_Outlet_Sales ~., data = tr_data, method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneLength=10
)
# Model coefficients
coef(elastic$finalModel, elastic$bestTune$lambda)
# Make predictions
predictions <- elastic %>% predict(ts_data)
# Model prediction performance
data.frame(
  RMSE = RMSE(predictions, ts_data$Item_Outlet_Sales),
  Rsquare = R2(predictions, ts_data$Item_Outlet_Sales)
)
elastic$bestTune$lambda

models <- list(ridge = ridge, lasso = lasso, elastic = elastic)
resamples(models) %>% summary( metric = "RMSE")
###############################################################################################################
test<-
set.seed(123)
elastic<-train(
  Item_Outlet_Sales~.,data=train,method = "glmnet",
  trControl = trainControl("cv",number=10),
  tuneLength = 10
)
coef(elastic$finalModel, elastic$bestTune$lambda)
# Make predictions

test$Item_Outlet_Sales<-sample(test,nrow(test))
test<-model.matrix(Item_Outlet_Sales~.,data=test)[,-1]
predictions <- elastic %>% predict(test)


