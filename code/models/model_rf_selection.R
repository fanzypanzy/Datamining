#Input data
orgdata <- read.csv("../dataset_modelling.csv", header = T, na.strings=c(".","","99999"))
holdout <- read.csv("../holdout_data.csv", header = T, na.strings=c(".","","99999"))
app_id_t <- orgdata[,1]
app_id_h <- holdout[,1]
app_id <- rbind(as.matrix(app_id_t),as.matrix(app_id_h))
newdata <- rbind(orgdata[,-c(1,2,3,4)], holdout[,-c(1,2)])

#Impute missing value without response colum
library(missForest)
set.seed(123)
newdata.imp <- missForest(newdata)
ximp <- newdata.imp$ximp
totaldata_mf <- cbind(app_id = app_id, ximp)
write.csv(totaldata_mf,"../dataset_imputed.csv")

#create another level for response without GOOD or BAD
library(gdata)
orgdata$GOOD <- as.factor(orgdata$GOOD)
orgdata[,2] <- NAToUnknown(x = orgdata[,2], unknown = "other")

totaldata <- read.csv("../dataset_imputed.csv", header = T)
#Split data
train.temp <- cbind(Res = orgdata[,2], totaldata[1:9962,-1])
test.temp <- totaldata[9963:14943,-1]

# Delete colums from Fanny's results 
#"D_02"  "I_01"  "I_02"  "D_01"  "S_01"  "CA_02" "I_04"  "ER_02" "ER_01" "I_03"  "I_05" removed
train <- train.temp[,-c(8,9,10,11,12,13,14,15,16,19,21)]

# To find the best hypeparameter
library(mlr)
trainTask <- makeClassifTask(data = train, target = "Res")

getParamSet("classif.randomForest")
rf <- makeLearner("classif.randomForest", 
                  predict.type = "response", 
                  par.vals = list(ntree = 400, mtry = 30, nodesize= 60))
rf$par.vals <- list(importance = TRUE)

rf_param1 <- makeParamSet(makeIntegerParam("ntree", lower = 400, upper = 600))
rf_param2 <- makeParamSet(makeIntegerParam("mtry", lower = 5, upper = 30))
rf_param3 <- makeParamSet(makeIntegerParam("nodesize", lower = 10, upper = 100))

rf_param <- makeParamSet(makeIntegerParam("ntree", lower = 400, upper = 600),
                         makeIntegerParam("mtry", lower = 20, upper = 35),
                         makeIntegerParam("nodesize", lower = 45, upper = 65))
rancontrol <- makeTuneControlRandom(maxit = 20L)
gridcontrol <- makeTuneControlGrid(resolution = 20L)
set_cv <- makeResampleDesc("CV",iters = 3L)

rf_tune <- tuneParams(learner = rf, 
                      resampling = set_cv, 
                      task = trainTask, 
                      par.set = rf_param, 
                      control = rancontrol, 
                      measures = acc)
### ntree=482, mtry=21,nodesize=51 

# Split the origianl data to train and test set to check accuracy
library(caret)
trainIndex = createDataPartition(train$Res, p=0.7, list=FALSE, times=1)
train_org <- train[trainIndex,]
test_org <- train[-trainIndex,]


library(randomForest)
set.seed(123)
rf1 <- randomForest(factor(Res)~., data = train_org, 
                    ntree = 482, 
                    mtry = 21,
                    nodesize = 51, 
                    importance = T,
                    proximity=T)
preds.test <- predict(rf1,test_org)
preds.test.prob <- predict(rf1, test_org, type="prob")
confusionMatrix(test_org$Res, preds.test)
importance(rf1,type = 2)
varImpPlot(rf1)
# 82.19%

n <- length(names(train_org))
set.seed(123)
err <- NULL
for (i in 1 :(n -1)) {
  mtry_fit <- randomForest(factor(Res)~., data = train_org, 
                           mtry = i)
  err[i] <- mean(mtry_fit$err.rate)
}
mtry <- which.min(err)
#7
#0.2922686, 0.2522567, 0.2490089, 0.2472005, 0.2507395, 0.2494922, 0.2438584, 0.2450415, 0.2476577, 0.2447465, 0.2447186

