# try glms
credit<-read.csv("dataset_modelling.csv",header=T,na.strings=c("", ".", "NA", "99999"))
mydat <- read.csv("trainimp_with_app_id.csv", header = TRUE)

# seperate Liangliangs imputed dataset into original data and holdout data
orig <- mydat[1:9962,]
holdout <- mydat[9963:14943,]

orig$GOOD <- as.factor(credit$GOOD)
orig <- orig[,-1]
holdout <- holdout[,-1]

# separate into training and test data
trainIndex <- createDataPartition(orig$GOOD, p=0.7, list=FALSE, times=1)
train <- orig[trainIndex,]
test <- orig[-trainIndex,]

# fit model (only with two outcomes)
binLogit <- glm(GOOD~ ., data= train,family=binomial)
# predict test data set (predicts probabilities of outcomes)
predLogit <- predict(binLogit, type = "response", newdata = test)
# convert probabilities to actual outcomes
# (taking the mean is a common strategy, although there might be a slightly better value)
val<-(mean(fitted(binLogit))) 
resp<-ifelse(predLogit>val,1,0)
(confMat <- confusionMatrix(resp, test$GOOD))
confMat$overall[1]




