orgdata <- read.csv("../dataset_modelling.csv", header = T, na.strings=c(".","","99999"))
holdout <- read.csv("../holdout_data.csv", header = T, na.strings=c(".","","99999"))
app_id_t <- orgdata[,1]
app_id_h <- holdout[,1]


library(missForest)
train <- rbind(orgdata[,-c(1,2,3,4)], holdout[,-c(1,2)])
set.seed(123)
train.imp <- missForest(train)
train <- train.imp$ximp
app_id <- rbind(as.matrix(app_id_t),as.matrix(app_id_h))
train <- cbind(app_id = app_id, train)
write.csv(train,"../dataset_imputed.csv")

library(gdata)
orgdata$GOOD <- as.factor(orgdata$GOOD)
orgdata[,2] <- NAToUnknown(x = orgdata[,2], unknown = "other")

totaldata <- read.csv("../dataset_imputed.csv", header = T)
train <- totaldata[1:9962,-1]
train <- cbind(Res=orgdata[,2], train)

