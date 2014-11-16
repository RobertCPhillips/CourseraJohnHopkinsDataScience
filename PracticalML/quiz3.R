#quiz3
library(rattle)
library(caret)

#q1
library(AppliedPredictiveModeling)
data(segmentationOriginal)
training = segmentationOriginal[segmentationOriginal$Case == "Train",]
testing = segmentationOriginal[segmentationOriginal$Case == "Test",]
set.seed(125)
modFit1 <- train(Class ~., data=training, method="rpart")
print(modFit1$finalModel)

#n= 1009 

#1) root 1009 373 PS (0.63032706 0.36967294)  
#2) TotalIntenCh2< 45323.5 454  34 PS (0.92511013 0.07488987) *
#  3) TotalIntenCh2>=45323.5 555 216 WS (0.38918919 0.61081081)  
#6) FiberWidthCh1< 9.673245 154  47 PS (0.69480519 0.30519481) *
#  7) FiberWidthCh1>=9.673245 401 109 WS (0.27182045 0.72817955) *
  
fancyRpartPlot(modFit1$finalModel)

#q3
library(pgmm)
data(olive)
olive = olive[,-1]
modFit3 <- train(Area ~., data=olive, method="rpart")
print(modFit3)
testing3 <- as.data.frame(t(colMeans(olive)))
predict(modFit3,testing3)

#q4
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
inTrain4 = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
training4 = SAheart[inTrain4,]
testing4 = SAheart[-inTrain4,]
#Coronary Heart Disease (chd) as the outcome 
#age at onset, current alcohol consumption, obesity levels, cumulative tabacco, type-A behavior, and low density lipoprotein cholesterol as predictors
set.seed(13234)
modFit4 <- train(chd ~age+alcohol+obesity+tobacco+typea+ldl, data=training4, method="glm", family="binomial")
predict4 <- predict(modFit4,testing4)
predict4b <- predict(modFit4,training4)

missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
mctest <- missClass(testing4[,"chd"],predict4)
mctrain <- missClass(training4[,"chd"],predict4b)
  
#q5
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)
training5 <- vowel.train
testing5 <- vowel.test

training5$y <- as.factor(training5$y)
testing5$y <- as.factor(testing5$y)
set.seed(33833)

modFit5 <- train(y ~., data=training5, method="rf",prox=T,importance=T)
varImp(modFit5,type=2)
