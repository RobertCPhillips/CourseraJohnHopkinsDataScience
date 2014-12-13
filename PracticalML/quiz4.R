#quiz 4
library(caret)

#q1
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)

vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)

set.seed(33833)

train1 <- train(y~., data=vowel.train, method="rf")
train2 <- train(y~., data=vowel.train, method="gbm")

predict1 <- predict(train1, vowel.test)
predict2 <- predict(train2, vowel.test)

c1 <- confusionMatrix(predict1,vowel.test$y)
c2 <- confusionMatrix(predict2,vowel.test$y)
a <- sum(predict1 == predict2)/length(vowel.test$y)

data.frame(
  rfa=c1$overall["Accuracy"],
  gbma=c2$overall["Accuracy"],
  aa=a)

#q2
library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

set.seed(62433)
fit1 <- train(diagnosis~., data=training, method="rf")
fit2 <- train(diagnosis~., data=training, method="gbm")
fit3 <- train(diagnosis~., data=training, method="lda")

predict1 <- predict(fit1, testing)
predict2 <- predict(fit2, testing)
predict3 <- predict(fit3, testing)

c1 <- confusionMatrix(predict1,testing$diagnosis)
c2 <- confusionMatrix(predict2,testing$diagnosis)
c3 <- confusionMatrix(predict3,testing$diagnosis)

combined <- data.frame(predict1,predict2,predict3,diagnosis=testing$diagnosis)
combinedFit <- train(diagnosis~., method="rf", data=combined)
combPredict <- predict(combinedFit, combined)
c4 <- confusionMatrix(combPredict,combined$diagnosis)

c4$overall["Accuracy"]

#q3
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

set.seed(233)
fit1 <- train(CompressiveStrength~., data=training, method="lasso")
plot(fit$finalModel)

#q4
library(lubridate)  # For year() function below
dat = read.csv("gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)

library(forecast)
fit1 <- bats(tstrain)
lenTesting <- dim(testing)[1]
f <- forecast(fit1, level=c(95), h=lenTesting)
a <- sum(sapply(1:235,function(x) testing[x,3] >= f$lower[x] & testing[x,3] <= f$upper[x])) / lenTesting

#q5
set.seed(3523)
library(AppliedPredictiveModeling)
library(caret)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

library(e1071)
set.seed(325)

fit <- svm(CompressiveStrength~., data=training)
p <- predict(fit, testing)
err <- testing$CompressiveStrength - p
rmse <- sqrt(mean(err^2))




