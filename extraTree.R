totaldata <- read.csv("missforestimp.csv", header = T)
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
library(rJava)
nonsense <- train_org[,-c(1,4,7)]
target <- as.factor(train_org$Res)
et <- extraTrees(nonsense, 
                 target, 
                 ntree=309,
                 mtry= 6,
                 nodesize = 22,
                 numRandomCuts = 5)
etpred <- predict(et, test_org[,-c(1,4,7)])
table(testClass=factor(test_org$Res),
      predClass=etpred)
confusionMatrix(test_org$Res, etpred)