credit<-read.csv("../dataset_modelling.csv",header=T,na.strings=c("", ".", "NA", "99999"))
mydat <- read.csv("../dataset_imputed.csv", header = TRUE)

# seperate Liangliangs imputed dataset into original data and holdout data
orig <- mydat[1:9962,]
holdout <- mydat[9963:14943,]

orig$GOOD <- as.factor(credit$GOOD)
orig <- orig[,-1]
holdout <- holdout[,-1]

# intoroduce a new level for missing values
library(gdata)
orig$GOOD <- NAToUnknown(x = orig$GOOD, unknown = "other")

# get a split into test and training data
require(caret)
trainIndex <- createDataPartition(orig$GOOD, p=0.7, list=FALSE, times=1)

# train a random forest model
library(randomForest)
train <- orig[trainIndex,]
test <- orig[-trainIndex,]
rf.model <- randomForest(factor(GOOD)~., data = train, 
                         ntree = 482, 
                         mtry = 21, 
                         nodesize = 51, 
                         importance = T)
rf.predict <- predict(rf.model, test)
require(e1071)
(cm <- confusionMatrix(test$GOOD, rf.predict))

