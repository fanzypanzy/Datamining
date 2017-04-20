orgdata <- read.csv("../dataset_modelling.csv", header = T, na.strings=c(".","","99999"))
holdout <- read.csv("../holdout_data.csv", header = T, na.strings=c(".","","99999"))
orgdata$occ_code <- as.factor(orgdata$occ_code)
orgdata$res_indicator <- as.factor(orgdata$res_indicator)
orgdata$CA_01 <- as.factor(orgdata$CA_01)
orgdata$GOOD <- as.factor(orgdata$GOOD)
orgdata <- orgdata[,-3]
orgdata[is.na(orgdata)] <- -999

holdout$occ_code <- as.factor(holdout$occ_code)
holdout$res_indicator <- as.factor(holdout$res_indicator)
holdout$CA_01 <- as.factor(holdout$CA_01)
holdout <- holdout[,-2]
holdout[is.na(holdout)] <- -999

library(gdata)
orgdata[,2] <- NAToUnknown(x = orgdata[,2], unknown = "2")
orgdata[,8] <- NAToUnknown(x = orgdata[,8], unknown = "other")
holdout[,3] <- NAToUnknown(x = holdout[,3], unknown = "other")
holdout[,6] <- NAToUnknown(x = holdout[,6], unknown = "other")



library(caret)
trainIndex = createDataPartition(orgdata$GOOD, p=0.7, list=FALSE, times=1)
train <- orgdata[trainIndex,]
test <- orgdata[-trainIndex,]

library(randomForest)
set.seed(123)
rf1 <- randomForest(factor(GOOD)~., data = train[,-3], 
                    ntree = 482, 
                    mtry = 7, 
                    nodesize = 51,
                    cutoff = c(0.21,0.23,0.56),
                    importance = T)
rf1
preds.test <- predict(rf1,test[,-3])
confusionMatrix(test$GOOD, preds.test)
# 82.29% without cutoff

prediction <- predict(rf1, test[,-3], type='prob')
p <- as.numeric(predict(rf1, test[,-3], type = 'response'))

library(pROC)
d <- multiclass.roc(test$GOOD, p)
a <- multiclass.roc(test$GOOD, prediction[,1])
b <- multiclass.roc(test$GOOD, prediction[,2])
c <- multiclass.roc(test$GOOD, prediction[,3])
r4 <- d$rocs
coords(r4[[3]],"best" )
r1 <-a$rocs
r2 <-b$rocs
r3 <-c$rocs
coords(r1[[1]],"best")
coords(r2[[1]],"best")
coords(r3[[2]],"best")
plot.roc(r[[1]],col = "red")
lines.roc(r[[2]],col = "green")
lines.roc(r[[3]], col='blue')


# To find the best hypeparameter
library(mlr)
trainTask <- makeClassifTask(data = train[,-3], target = "GOOD")

getParamSet("classif.randomForest")
rf <- makeLearner("classif.randomForest", 
                  predict.type = "response", 
                  par.vals = list(ntree = 482, mtry = 7, nodesize= 51))
rf$par.vals <- list(importance = TRUE)

rf_param <- makeParamSet(makeNumericParam("cutoff", lower = .2, upper = .5, trafo = function(x) c(0.21, x, 0.79-x)))
rancontrol <- makeTuneControlRandom(maxit = 10L)
set_cv <- makeResampleDesc("CV",iters = 3L)

rf_tune <- tuneParams(learner = rf, 
                      resampling = set_cv, 
                      task = trainTask, 
                      par.set = rf_param, 
                      control = rancontrol, 
                      measures = acc)