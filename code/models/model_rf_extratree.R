orgdata <- read.csv("../dataset_modelling.csv", header = T, na.strings=c(".","","99999"))
holdout <- read.csv("../holdout_data.csv", header = T, na.strings=c(".","","99999"))

library(gdata)
orgdata$GOOD <- as.factor(orgdata$GOOD)
orgdata[,2] <- NAToUnknown(x = orgdata[,2], unknown = "2")

totaldata <- read.csv("../dataset_imputed.csv", header = T)
train <- totaldata[1:9962,-1]
train <- cbind(Res=orgdata[,2], train)
test <- totaldata[9963:14943,-1]

library(mlr)
occ.code<-data.frame(var=train$occ_code)
occ.code<-createDummyFeatures(occ.code,cols='var')
res.indicator<-data.frame(var=train$res_indicator)
res.indicator<-createDummyFeatures(res.indicator,cols='var')
CA.01 <- data.frame(var = as.factor(train$CA_01))
CA.01 <- createDummyFeatures(CA.01,cols="var")
train_dummy <- cbind(train,occ.code,res.indicator,CA.01)

occ.code.test<-data.frame(var=test$occ_code)
occ.code.test<-createDummyFeatures(occ.code.test,cols='var')
res.indicator.test<-data.frame(var=test$res_indicator)
res.indicator.test<-createDummyFeatures(res.indicator.test,cols='var')
CA.01.test <- data.frame(var = as.factor(test$CA_01))
CA.01.test <- createDummyFeatures(CA.01.test,cols="var")
test_dummy <- cbind(test,occ.code.test,res.indicator.test,CA.01.test)



library(caret)
trainIndex = createDataPartition(train_dummy$Res, p=0.7, list=FALSE, times=1)
train_org <- train_dummy[trainIndex,]
test_org <- train_dummy[-trainIndex,]


library(extraTrees)
set.seed(123)
nonsense <- train_org[,-c(1,4,7,22)]
target <- as.factor(train_org$Res)
et <- extraTrees(nonsense, 
                 target, 
                 ntree=482,
                 mtry= 7,
                 nodesize = 51,
                 numRandomCuts = 5)
etpred <- predict(et, test_org[,-c(1,4,7,22)])
table(testClass=factor(test_org$Res),
      predClass=etpred)
confusionMatrix(test_org$Res, etpred)
# 80.31%