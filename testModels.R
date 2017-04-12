credit<-read.csv("dataset_modelling.csv",header=T,na.strings=c("", ".", "NA", "99999"))
mydat <- read.csv("trainimp_with_app_id.csv", header = TRUE)

# seperate Liangliangs imputed dataset into original data and holdout data
orig <- mydat[1:9962,]
holdout <- mydat[9963:14943,]

orig$GOOD <- as.factor(credit$GOOD)
orig <- orig[,-1]
holdout <- holdout[,-1]


library(gdata)
orig$GOOD <- NAToUnknown(x = orig$GOOD, unknown = "other")

# get a split into test and training data
require(caret)
trainIndex <- createDataPartition(orig$GOOD, p=0.7, list=FALSE, times=1)

library(randomForest)
train <- orig[trainIndex,]
test <- orig[-trainIndex,]
rf.model <- randomForest(factor(GOOD)~., data = train, 
                         ntree = 443, 
                         mtry = 8, 
                         nodesize = 39, 
                         #cutoff = c(0.465,0.535),
                         importance = T)
rf.predict <- predict(rf.model, test)
(cm <- confusionMatrix(test$GOOD, rf.predict))

