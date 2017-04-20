credit<-read.csv("../dataset_modelling.csv",header=T,na.strings=c("", ".", "NA", "99999"))
mydat <- read.csv("../dataset_imputed.csv", header = TRUE)

# seperate Liangliangs imputed dataset into original data and holdout data
dat <- mydat[1:9962,]
dat <- dat[,-1]
dat$GOOD <- credit$GOOD

require(dplyr)
library(gdata)

#Introduce a new class for the NAs as -1
dat$GOOD[which(is.na(dat$GOOD))] <- -1
dat$GOOD <- as.factor(dat$GOOD)

# Divide the data into training and test set
library(caret)
library(e1071)
trainIndex = createDataPartition(dat$GOOD, p=0.7, list=FALSE, times=1)
train <- dat[trainIndex, ]
test <- dat[-trainIndex, ]
train_data <- data.frame(x=subset(train, select=-c(GOOD)), y=train$GOOD)
test_data <- data.frame(x=subset(test, select=-c(GOOD)), y=test$GOOD)

#---------- Radial Kernel Evaluation
# Test different configurations for the radial kernel 
tune.out<- tune(svm,y~.,data=train_data, kernel="radial", 
                ranges=list(cost=c(0.1,0.5,1,2), gamma=c(0.1, 0.01, 1, 2)))

summary(tune.out)
bestModel<- tune.out$best.model
summary ( bestModel )

# Predict the training set with the best model
test_y = test_data[, "y"]
prediccion = predict(bestModel, test_data)
xtab <- table(true=test_y, pred=prediccion)
confusionMatrix(xtab)

#---------- Polynomial Kernel Evaluation
# Test different configurations for the radial kernel 
tune.out<- tune(svm,y~.,data=train_data, kernel="polynomial", 
                ranges=list(cost=c(0.1,0.5,1,2), gamma=c(0.1, 0.01, 1, 2), degree=c(2,3)))

summary(tune.out)
bestModel<- tune.out$best.model
summary ( bestModel )

# Predict the training set with the best model
test_y = test_data[, "y"]
prediccion = predict(bestModel, test_data)
xtab <- table(true=test_y, pred=prediccion)
confusionMatrix(xtab)

#---------- Linear Kernel Evaluation
# Test different configurations for the radial kernel 
tune.out<- tune(svm,y~.,data=train_data, kernel="linear", 
                ranges=list(cost=c(0.1,0.5,1,2)))

summary(tune.out)
bestModel<- tune.out$best.model
summary ( bestModel )

# Predict the training set with the best model
test_y = test_data[, "y"]
prediccion = predict(bestModel, test_data)
xtab <- table(true=test_y, pred=prediccion)
confusionMatrix(xtab)

#---------- Sigmoid Kernel Evaluation
# Test different configurations for the radial kernel 
tune.out<- tune(svm,y~.,data=train_data, kernel="sigmoid", 
                ranges=list(cost=c(0.1,0.5,1,2), gamma=c(0.1, 0.01, 1, 2)))

summary(tune.out)
bestModel<- tune.out$best.model
summary ( bestModel )

# Predict the training set with the best model
test_y = test_data[, "y"]
prediccion = predict(bestModel, test_data)
xtab <- table(true=test_y, pred=prediccion)
confusionMatrix(xtab)
