# GLMs
credit<-read.csv("../dataset_modelling.csv",header=T,na.strings=c("", ".", "NA", "99999"))
mydat <- read.csv("../dataset_imputed.csv", header = TRUE)

# seperate Liangliangs imputed dataset into original data and holdout data
orig <- mydat[1:9962,]
holdout <- mydat[9963:14943,]


orig$GOOD <- as.factor(credit$GOOD)
orig <- orig[,-1]
holdout <- holdout[,-1]

m <- which(is.na(orig$GOOD))
origResponse <- orig[-m,]

# separate into training and test data
trainIndex <- createDataPartition(origResponse$GOOD, p=0.7, list=FALSE, times=1)
train <- origResponse[trainIndex,]
test <- origResponse[-trainIndex,]

# fit model (only with two outcomes)
binLogit <- glm(GOOD~ ., data= train,family=binomial)

# find the best threshold
library(pROC)
train$prob <-  predict(binLogit,type=c("response"), newdata = train)

g <- roc(GOOD ~ prob, data = train)
best<-coords(g, "best")

# plot ROC curve
df<-data.frame(t(coords(g, seq(0, 1, 0.01))))
require(ggplot2)
p <- ggplot(df)
p <- p + geom_line(aes(1-specificity, sensitivity, colour=threshold), size=3) +
  theme_bw()
p + geom_abline(intercept=0, slope=1)  +
  geom_hline(yintercept=as.numeric(best[3]), colour="darkgrey", linetype="longdash") +
  geom_vline(xintercept = as.numeric(1-best[2]), colour="darkgrey", linetype="longdash") +
  scale_colour_gradient(high="red", low="white") +
  geom_line(aes(1-specificity, sensitivity), colour="blue", alpha=1/3) +
  xlab("1-Specificity (False Positive Rate)") + ylab("Sensitivity (True Positive Rate)") +
  labs(colour="Threshold")



# predict test data set (predicts probabilities of outcomes)
predLogit <- predict(binLogit, type = "response", newdata = test)
# convert probabilities to actual outcomes
# (taking the optimal value calculated above)

resp<-ifelse(predLogit>best[1],1,0)
(confMat <- confusionMatrix(resp, test$GOOD))
confMat$overall[1]




