#quiz2

#q1
library(AppliedPredictiveModeling)
library(caret)
data(AlzheimerDisease)
adData1 = data.frame(diagnosis,predictors)
testIndex1 = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training1 = adData1[-testIndex1,]
testing1 = adData1[testIndex1,]

#q2
library(AppliedPredictiveModeling)
library(caret)
data(concrete)
set.seed(975)
inTrain2 = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training2 = mixtures[inTrain2,]
testing2 = mixtures[-inTrain2,]
plot(training2$CompressiveStrength)


#q3
library(AppliedPredictiveModeling)
library(caret)
data(concrete)
set.seed(975)
inTrain3 = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training3 = mixtures[inTrain3,]
testing3 = mixtures[-inTrain3,]
par(mfrow=c(1,2))
hist(mixtures$Superplasticizer)
hist(log(mixtures$Superplasticizer+1))

#q4
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData4 = data.frame(diagnosis,predictors)
inTrain4 = createDataPartition(adData4$diagnosis, p = 3/4)[[1]]
training4 = adData4[inTrain4,]
testing4 = adData4[-inTrain4,]
ilcols <- grep("^IL",colnames(training4))
p <- preProcess(training4[,ilcols],method="pca", thresh=.8)
p$numComp

#q5
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData5 = data.frame(diagnosis,predictors)
ilcols5 <- c(1,grep("^IL",colnames(adData5)))

inTrain5 = createDataPartition(adData5$diagnosis, p = 3/4)[[1]]
training5 = adData5[inTrain5, ilcols5]
testing5 = adData5[-inTrain5, ilcols5]

m1 <- train(diagnosis~.,data=training5, method="glm")
p1 <- predict(m1, newdata = testing5)
c1 <- confusionMatrix(p1, testing5$diagnosis)
c1$overall["Accuracy"]

m2 <- train(diagnosis~.,data=training5, method="glm", preProcess="pca",
            trControl = trainControl(preProcOptions = list(thresh = 0.8)))
m2$results$Accuracy
p2 <- predict(m2, newdata = testing5)
c2 <- confusionMatrix(p2, testing5$diagnosis)
c2$overall["Accuracy"]






