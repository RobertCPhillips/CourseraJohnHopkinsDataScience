#Six young health participants were asked to perform one set of 10 repetitions 
#of the Unilateral Dumbbell Biceps Curl in five different fashions: 
# A - exactly according to the specification 
# B - throwing the elbows to the front 
# C - lifting the dumbbell only halfway 
# D - lowering the dumbbell only halfway 
# E - and throwing the hips to the front

#The goal of your project is to predict the manner in which they did the exercise
#using data from accelerometers on the belt, forearm, arm, and dumbell.
library(caret)

trainingFile <- read.csv("pml-training.csv")
testingFile <- read.csv("pml-testing.csv")

set.seed(1002)

#columns of interest
cols <- c("roll_belt","pitch_belt","yaw_belt", 
          "gyros_belt_x","gyros_belt_y","gyros_belt_z", 
          "accel_belt_x","accel_belt_y","accel_belt_z", 
          "magnet_belt_x","magnet_belt_y","magnet_belt_z",
          
          "roll_arm","pitch_arm","yaw_arm",
          "gyros_arm_x","gyros_arm_y","gyros_arm_z",
          "accel_arm_x", "accel_arm_y","accel_arm_z",
          "magnet_arm_x","magnet_arm_y","magnet_arm_z",
          
          "roll_dumbbell","pitch_dumbbell","yaw_dumbbell", 
          "gyros_dumbbell_x","gyros_dumbbell_y","gyros_dumbbell_z", 
          "accel_dumbbell_x","accel_dumbbell_y","accel_dumbbell_z", 
          "magnet_dumbbell_x","magnet_dumbbell_y","magnet_dumbbell_z", 
          
          "roll_forearm","pitch_forearm","yaw_forearm", 
          "gyros_forearm_x","gyros_forearm_y","gyros_forearm_z", 
          "accel_forearm_x","accel_forearm_y","accel_forearm_z", 
          "magnet_forearm_x","magnet_forearm_y","magnet_forearm_z")

partition <- createDataPartition(trainingFile$classe, p = 0.60,list=FALSE)

training <- trainingFile[partition, c(cols,"classe")]
crossValid <- trainingFile[-partition, c(cols,"classe")]
testing <- testingFile[,c(cols)]

#create model with training data
modelFit <- randomForest(classe~., data=training, ntree=50)

#check accuracy with cross validation
cv <- predict(modelFit,crossValid)

c1 <- confusionMatrix(cv, crossValid$classe)
#c1

#predict with testing data
p <- predict(modelFit,testing)

#create files for submission
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

#pml_write_files(p)
#B A B A A E D B A A B C B A E E A B B B

library(ggplot2)
library(gridExtra)

#plots on gyros
p1 <- qplot(classe,gyros_belt_x,data=training, fill=classe,geom=c("boxplot"))
p2 <- qplot(classe,gyros_belt_y,data=training, fill=classe,geom=c("boxplot"))
p3 <- qplot(classe,gyros_belt_z,data=training, fill=classe,geom=c("boxplot"))
grid.arrange(p1,p2,p3)

p1 <- qplot(classe,gyros_arm_x,data=training, fill=classe,geom=c("boxplot"))
p2 <- qplot(classe,gyros_arm_y,data=training, fill=classe,geom=c("boxplot"))
p3 <- qplot(classe,gyros_arm_z,data=training, fill=classe,geom=c("boxplot"))
grid.arrange(p1,p2,p3)

p1 <- qplot(classe,gyros_dumbbell_x,data=training, fill=classe,geom=c("boxplot"))
p2 <- qplot(classe,gyros_dumbbell_y,data=training, fill=classe,geom=c("boxplot"))
p3 <- qplot(classe,gyros_dumbbell_z,data=training, fill=classe,geom=c("boxplot"))
grid.arrange(p1,p2,p3)

p1 <- qplot(classe,gyros_forearm_x,data=training, fill=classe,geom=c("boxplot"))
p2 <- qplot(classe,gyros_forearm_y,data=training, fill=classe,geom=c("boxplot"))
p3 <- qplot(classe,gyros_forearm_z,data=training, fill=classe,geom=c("boxplot"))
grid.arrange(p1,p2,p3)

#plots on accel
p1 <- qplot(classe,accel_belt_x,data=training, fill=classe,geom=c("boxplot"))
p2 <- qplot(classe,accel_belt_y,data=training, fill=classe,geom=c("boxplot"))
p3 <- qplot(classe,accel_belt_z,data=training, fill=classe,geom=c("boxplot"))
grid.arrange(p1,p2,p3)

p1 <- qplot(classe,accel_arm_x,data=training, fill=classe,geom=c("boxplot"))
p2 <- qplot(classe,accel_arm_y,data=training, fill=classe,geom=c("boxplot"))
p3 <- qplot(classe,accel_arm_z,data=training, fill=classe,geom=c("boxplot"))
grid.arrange(p1,p2,p3)

p1 <- qplot(classe,accel_dumbbell_x,data=training, fill=classe,geom=c("boxplot"))
p2 <- qplot(classe,accel_dumbbell_y,data=training, fill=classe,geom=c("boxplot"))
p3 <- qplot(classe,accel_dumbbell_z,data=training, fill=classe,geom=c("boxplot"))
grid.arrange(p1,p2,p3)

p1 <- qplot(classe,accel_forearm_x,data=training, fill=classe,geom=c("boxplot"))
p2 <- qplot(classe,accel_forearm_y,data=training, fill=classe,geom=c("boxplot"))
p3 <- qplot(classe,accel_forearm_z,data=training, fill=classe,geom=c("boxplot"))
grid.arrange(p1,p2,p3)

#plots on roll, pitch, yaw
p1 <- qplot(classe,roll_belt,data=training, fill=classe,geom=c("boxplot"))
p2 <- qplot(classe,pitch_belt,data=training, fill=classe,geom=c("boxplot"))
p3 <- qplot(classe,yaw_belt,data=training, fill=classe,geom=c("boxplot"))
grid.arrange(p1,p2,p3)

p1 <- qplot(classe,roll_arm,data=training, fill=classe,geom=c("boxplot"))
p2 <- qplot(classe,pitch_arm,data=training, fill=classe,geom=c("boxplot"))
p3 <- qplot(classe,yaw_arm,data=training, fill=classe,geom=c("boxplot"))
grid.arrange(p1,p2,p3)

p1 <- qplot(classe,roll_dumbbell,data=training, fill=classe,geom=c("boxplot"))
p2 <- qplot(classe,pitch_dumbbell,data=training, fill=classe,geom=c("boxplot"))
p3 <- qplot(classe,yaw_dumbbell,data=training, fill=classe,geom=c("boxplot"))
grid.arrange(p1,p2,p3)

p1 <- qplot(classe,roll_forearm,data=training, fill=classe,geom=c("boxplot"))
p2 <- qplot(classe,pitch_forearm,data=training, fill=classe,geom=c("boxplot"))
p3 <- qplot(classe,yaw_forearm,data=training, fill=classe,geom=c("boxplot"))
grid.arrange(p1,p2,p3)


