dat <-
  read.csv(
    "../missforestimputed.csv",
    header = T,
    na.strings = c("", ".", "NA", "99999")
  )

train_size <- floor(0.7 * nrow(dat))
index <- sample(1:nrow(dat), size = train_size)
set.seed(123)
train <- dat[index, ]
test <- dat[-index, ]


#######adaboost model
#160025235
levels(train$GOOD)
class(train$GOOD)
train$GOOD <- as.factor(train$GOOD)
test$GOOD <- as.factor(test$GOOD)
install.packages("adabag")
library(adabag)
model.Adboost <-
  boosting(
    GOOD ~ disp_income + occ_code + cust_age + time_emp + res_indicator + I_01 + I_02 + I_03 + I_04 + D_01 + ER_01 + ER_02 + I_05 + D_02 + I_06 + P_01 + S_01 + CA_03 + CA_02 + CA_01 + S_02,
    data = train,
    boos = TRUE,
    mfinal = 10,
    control = rpart.control(minsplit = 0)
  )
results.Adboost <- predict.boosting(model.Adboost, newdata = test)
results.Adboost$confusion
print(1 - results.Adboost$error)

#######naivebayes
#160025235
install.packages("e1071")
library(e1071)
model.bayes <-
  naiveBayes(
    GOOD ~ disp_income + occ_code + cust_age + time_emp + res_indicator + I_01 + I_02 +
      I_03 + I_04 + D_01 + ER_01 + ER_02 + I_05 + D_02 + I_06 +
      P_01 + S_01,
    data = train
  )

results.bayes <- predict(model.bayes, test)
confusionMatrix(test$GOOD, results.bayes)
