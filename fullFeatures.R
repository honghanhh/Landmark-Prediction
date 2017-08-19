library(ggvis)
library(shapes)
library(matlib)
library(rgeos)
library(plyr)
library(optR)
library(rpart)
library(rpart.plot)
library(randomForest)
library(e1071)
library(caret)
# library(rattle)
# library(help="randomForest")


setwd("C:/Users/Admin/Documents/Intern/Extract feature/TrainingData")
temp = list.files(pattern="*.csv")

#Function to calculate distance to bounding box
intersection_point  = function(a, data){
  if ((data[[2]]-data[[1]]) != 0){ 
    slope = (data[2,2] - data[1,2])/(data[[2]]-data[[1]])
    c1 = - ((data[2,2] - slope*data[2,1]))
    A <- matrix(c(1,slope,0,-1), nrow=2, ncol=2)
    b <- matrix(cbind(a[1],c1), nrow=2, ncol=1)
    Z1<-optR(A, b, method="gauss")
    output1 <- matrix(unlist(Z1[3]), nrow=1 , ncol =2)
  } else{
    A <- matrix(c(1,0,0,-1), nrow=2, ncol=2)
    b <- matrix(cbind(data[[1]],-a[2]), nrow=2, ncol=1)
    Z1<-optR(A, b, method="gauss")
    output1 <- matrix(unlist(Z1[3]), nrow=1 , ncol =2)
  }
  return(output1)
}
distanceToBoundary= function(a, b){
  distance <- sqrt((b[1]- a[1])^2 + (b[2]-a[2])^2)
  return(distance)
}

# initial data
temp1 = data.frame()
for (i in 1:length(temp)){ 
  data = read.csv(temp[i], sep = ',', header = TRUE)
  #calculate center
  data1 <- data.matrix(cbind(data$X, data$Y))
  colnames(data1) <- c("X", "Y")
  center <- colMeans(data1)
  a = data.frame()
  for (j in 1:length(data1[,1])){
    distance <- sqrt((data1[j,1]-center[1])^2 + (data1[j,2]-center[2])^2)
    a <- rbind(a, distance)
  }
  colnames(a) <- c("Distance")
  X <- cbind(data,a)
  
  # initial 4 point of bounding box
  up_left <- data.matrix(cbind(min(data$X), max(data$Y)))
  down_left <- data.matrix(cbind(min(data$X), min(data$Y)))
  up_right <- data.matrix(cbind(max(data$X), max(data$Y)))
  down_right <- data.matrix(cbind(max(data$X), min(data$Y)))
  
  temp_data = matrix(ncol=2)
  temp_data = temp_data[-1,]
  
  upper_boundary=rbind(up_left, up_right)
  rownames(upper_boundary, do.NULL = FALSE)
  rownames(upper_boundary) <- c("UP","")
  temp_data = rbind(temp_data, upper_boundary)
  
  lowwer_boundary=rbind(down_left, down_right)
  rownames(lowwer_boundary, do.NULL = FALSE)
  rownames(lowwer_boundary) <- c("DOWN","")
  temp_data = rbind(temp_data, lowwer_boundary)
  
  left_boundary=rbind(up_left, down_left)
  rownames(left_boundary, do.NULL = FALSE)
  rownames(left_boundary) <- c("LEFT","")
  temp_data = rbind(temp_data, left_boundary)
  
  right_boundary=rbind(up_right, down_right)
  rownames(right_boundary, do.NULL = FALSE)
  rownames(right_boundary) <- c("RIGHT","")
  temp_data = rbind(temp_data, right_boundary)
  
  # plot test
  #test<- rbind(down_left, down_right,up_right, up_left, down_left)
  #plotshapes(test, joinline=1:5)
  #points(data1)
  
  #calculate distance feature
  output1 =  matrix(ncol=4, nrow=1)
  output1 =  output1[-1,]
  for (m in 1:length(data1[,1])){
    point = data1[m,]
    feature_output =  matrix(ncol=1, nrow=1)
    feature_output =  feature_output[-1,]
    
    for (k in 1:(nrow(temp_data))){
      if(k%%2 != 0 ){
        tempx <- data.matrix(temp_data[k:(k+1),])
        inter_point <- intersection_point(point,tempx)
        z <- data.matrix(inter_point)
        temp5<-distanceToBoundary(point,z )
        temp5 =  data.matrix(temp5)
        rownames(temp5, do.NULL = FALSE)
        rownames(temp5) <- c(row.names(temp_data)[k])
        feature_output <- rbind(feature_output, temp5 )     
      }
    }
    output1 <- rbind(output1, t(feature_output))
  }
  #output
  X <- cbind(X, output1)
  output <-  X[,c("X","Y","isLandmark",colnames(a),colnames(output1)[1],colnames(output1)[2],colnames(output1)[3],colnames(output1)[4], 
                  "SURFfeature1","SURFfeature2","SURFfeature3","SURFfeature4","SURFfeature5","SURFfeature6","SURFfeature7","SURFfeature8",
                  "SURFfeature9","SURFfeature10","SURFfeature11","SURFfeature12","SURFfeature13","SURFfeature14","SURFfeature15","SURFfeature16",
                  "SURFfeature17","SURFfeature18","SURFfeature19","SURFfeature20","SURFfeature21","SURFfeature22","SURFfeature23","SURFfeature24",
                  "SURFfeature25","SURFfeature26","SURFfeature27","SURFfeature28","SURFfeature29","SURFfeature30","SURFfeature31","SURFfeature32",
                  "SURFfeature33","SURFfeature34","SURFfeature35","SURFfeature36","SURFfeature37","SURFfeature38","SURFfeature39","SURFfeature40",
                  "SURFfeature41","SURFfeature42","SURFfeature43","SURFfeature44","SURFfeature45","SURFfeature46","SURFfeature47","SURFfeature48",
                  "SURFfeature49","SURFfeature50","SURFfeature51","SURFfeature52","SURFfeature53","SURFfeature54","SURFfeature55","SURFfeature56",
                  "SURFfeature57","SURFfeature58","SURFfeature59","SURFfeature60","SURFfeature61","SURFfeature62","SURFfeature63","SURFfeature64",
                  "BRISKfeature1","BRISKfeature2","BRISKfeature3","BRISKfeature4","BRISKfeature5","BRISKfeature6","BRISKfeature7","BRISKfeature8",
                  "BRISKfeature9","BRISKfeature10","BRISKfeature11","BRISKfeature12","BRISKfeature13","BRISKfeature14","BRISKfeature15",
                  "BRISKfeature16","BRISKfeature17","BRISKfeature18","BRISKfeature19","BRISKfeature20","BRISKfeature21","BRISKfeature22",
                  "BRISKfeature23","BRISKfeature24","BRISKfeature25","BRISKfeature26","BRISKfeature27","BRISKfeature28","BRISKfeature29",
                  "BRISKfeature30","BRISKfeature31","BRISKfeature32","BRISKfeature33","BRISKfeature34","BRISKfeature35","BRISKfeature36",
                  "BRISKfeature37","BRISKfeature38","BRISKfeature39","BRISKfeature40","BRISKfeature41","BRISKfeature42","BRISKfeature43",
                  "BRISKfeature44","BRISKfeature45","BRISKfeature46","BRISKfeature47","BRISKfeature48","BRISKfeature49","BRISKfeature50",
                  "BRISKfeature51","BRISKfeature52","BRISKfeature53","BRISKfeature54","BRISKfeature55","BRISKfeature56","BRISKfeature57",
                  "BRISKfeature58","BRISKfeature59","BRISKfeature60","BRISKfeature61","BRISKfeature62","BRISKfeature63","BRISKfeature64",
                  "HOGfeature1","HOGfeature2","HOGfeature3","HOGfeature4","HOGfeature5","HOGfeature6","HOGfeature7","HOGfeature8","HOGfeature9",
                  "HOGfeature10","HOGfeature11","HOGfeature12","HOGfeature13","HOGfeature14","HOGfeature15","HOGfeature16","HOGfeature17","HOGfeature18",
                  "HOGfeature19","HOGfeature20","HOGfeature21","HOGfeature22","HOGfeature23","HOGfeature24","HOGfeature25","HOGfeature26","HOGfeature27",
                  "HOGfeature28","HOGfeature29","HOGfeature30","HOGfeature31","HOGfeature32","HOGfeature33","HOGfeature34","HOGfeature35","HOGfeature36",
                  "FREAKfeature1","FREAKfeature2","FREAKfeature3","FREAKfeature4","FREAKfeature5","FREAKfeature6","FREAKfeature7","FREAKfeature8",
                  "FREAKfeature9","FREAKfeature10","FREAKfeature11","FREAKfeature12","FREAKfeature13","FREAKfeature14","FREAKfeature15",
                  "FREAKfeature16","FREAKfeature17","FREAKfeature18","FREAKfeature19","FREAKfeature20","FREAKfeature21","FREAKfeature22",
                  "FREAKfeature23","FREAKfeature24","FREAKfeature25","FREAKfeature26","FREAKfeature27","FREAKfeature28","FREAKfeature29",
                  "FREAKfeature30","FREAKfeature31","FREAKfeature32","FREAKfeature33","FREAKfeature34","FREAKfeature35","FREAKfeature36",
                  "FREAKfeature37","FREAKfeature38","FREAKfeature39","FREAKfeature40","FREAKfeature41","FREAKfeature42","FREAKfeature43",
                  "FREAKfeature44","FREAKfeature45","FREAKfeature46","FREAKfeature47","FREAKfeature48","FREAKfeature49","FREAKfeature50",
                  "FREAKfeature51","FREAKfeature52","FREAKfeature53","FREAKfeature54","FREAKfeature55","FREAKfeature56","FREAKfeature57",
                  "FREAKfeature58","FREAKfeature59","FREAKfeature60","FREAKfeature61","FREAKfeature62","FREAKfeature63","FREAKfeature64")]
  #merge data
  temp1 = rbind(temp1, output)  
}


#Data
cord <-  temp1[,c("X","Y","isLandmark")]
morp <-  temp1[,c("Distance","UP","DOWN","LEFT","RIGHT")]
surf <- temp1[,c("SURFfeature1","SURFfeature2","SURFfeature3","SURFfeature4","SURFfeature5","SURFfeature6",
                 "SURFfeature7","SURFfeature8","SURFfeature9","SURFfeature10","SURFfeature11","SURFfeature12","SURFfeature13",
                 "SURFfeature14","SURFfeature15","SURFfeature16","SURFfeature17","SURFfeature18","SURFfeature19","SURFfeature20",
                 "SURFfeature21","SURFfeature22","SURFfeature23","SURFfeature24","SURFfeature25","SURFfeature26","SURFfeature27",
                 "SURFfeature28","SURFfeature29","SURFfeature30","SURFfeature31","SURFfeature32","SURFfeature33","SURFfeature34",
                 "SURFfeature35","SURFfeature36","SURFfeature37","SURFfeature38","SURFfeature39","SURFfeature40","SURFfeature41",
                 "SURFfeature42","SURFfeature43","SURFfeature44","SURFfeature45","SURFfeature46","SURFfeature47","SURFfeature48",
                 "SURFfeature49","SURFfeature50","SURFfeature51","SURFfeature52","SURFfeature53","SURFfeature54","SURFfeature55",
                 "SURFfeature56","SURFfeature57","SURFfeature58","SURFfeature59","SURFfeature60","SURFfeature61","SURFfeature62",
                 "SURFfeature63","SURFfeature64")]
brisk <- temp1[,c("BRISKfeature1","BRISKfeature2","BRISKfeature3","BRISKfeature4","BRISKfeature5","BRISKfeature6","BRISKfeature7",
                  "BRISKfeature8","BRISKfeature9","BRISKfeature10","BRISKfeature11","BRISKfeature12","BRISKfeature13","BRISKfeature14","BRISKfeature15",
                  "BRISKfeature16","BRISKfeature17","BRISKfeature18","BRISKfeature19","BRISKfeature20","BRISKfeature21","BRISKfeature22","BRISKfeature23",
                  "BRISKfeature24","BRISKfeature25","BRISKfeature26","BRISKfeature27","BRISKfeature28","BRISKfeature29","BRISKfeature30","BRISKfeature31",
                  "BRISKfeature32","BRISKfeature33","BRISKfeature34","BRISKfeature35","BRISKfeature36","BRISKfeature37","BRISKfeature38","BRISKfeature39",
                  "BRISKfeature40","BRISKfeature41","BRISKfeature42","BRISKfeature43","BRISKfeature44","BRISKfeature45","BRISKfeature46","BRISKfeature47",
                  "BRISKfeature48","BRISKfeature49","BRISKfeature50","BRISKfeature51","BRISKfeature52","BRISKfeature53","BRISKfeature54","BRISKfeature55",
                  "BRISKfeature56","BRISKfeature57","BRISKfeature58","BRISKfeature59","BRISKfeature60","BRISKfeature61","BRISKfeature62","BRISKfeature63",
                  "BRISKfeature64")]

freak <- temp1[,c("FREAKfeature1","FREAKfeature2","FREAKfeature3","FREAKfeature4","FREAKfeature5","FREAKfeature6","FREAKfeature7","FREAKfeature8",
              "FREAKfeature9","FREAKfeature10","FREAKfeature11","FREAKfeature12","FREAKfeature13","FREAKfeature14","FREAKfeature15",
              "FREAKfeature16","FREAKfeature17","FREAKfeature18","FREAKfeature19","FREAKfeature20","FREAKfeature21","FREAKfeature22",
              "FREAKfeature23","FREAKfeature24","FREAKfeature25","FREAKfeature26","FREAKfeature27","FREAKfeature28","FREAKfeature29",
              "FREAKfeature30","FREAKfeature31","FREAKfeature32","FREAKfeature33","FREAKfeature34","FREAKfeature35","FREAKfeature36",
              "FREAKfeature37","FREAKfeature38","FREAKfeature39","FREAKfeature40","FREAKfeature41","FREAKfeature42","FREAKfeature43",
              "FREAKfeature44","FREAKfeature45","FREAKfeature46","FREAKfeature47","FREAKfeature48","FREAKfeature49","FREAKfeature50",
              "FREAKfeature51","FREAKfeature52","FREAKfeature53","FREAKfeature54","FREAKfeature55","FREAKfeature56","FREAKfeature57",
              "FREAKfeature58","FREAKfeature59","FREAKfeature60","FREAKfeature61","FREAKfeature62","FREAKfeature63","FREAKfeature64")]

hog <- temp1[,c( "HOGfeature1","HOGfeature2","HOGfeature3","HOGfeature4","HOGfeature5","HOGfeature6","HOGfeature7","HOGfeature8","HOGfeature9",
               "HOGfeature10","HOGfeature11","HOGfeature12","HOGfeature13","HOGfeature14","HOGfeature15","HOGfeature16","HOGfeature17","HOGfeature18",
               "HOGfeature19","HOGfeature20","HOGfeature21","HOGfeature22","HOGfeature23","HOGfeature24","HOGfeature25","HOGfeature26","HOGfeature27",
               "HOGfeature28","HOGfeature29","HOGfeature30","HOGfeature31","HOGfeature32","HOGfeature33","HOGfeature34","HOGfeature35","HOGfeature36")]

#RANDOM FOREST
#d <- data.frame(temp1)
#Morphology
d <- cbind(cord,morp)

#SURFPoints
d <- cbind(cord,surf)

#Morphology + SURF 
d <- cbind(cord,morp,surf)

#BRISKPoints
d <- cbind(cord,brisk)

#FREAK
d <- cbind(cord,freak)

#HOG
d <- cbind(cord,hog)

#Morphology + SURF + BRISK
d <- cbind(cord,morp,surf,brisk)

#Morphology + SURF + BRISK + HOG + FREAK
d <- cbind(cord,morp,surf,brisk,freak,hog)

#--------------------------------------------------------------------------------------------------------------------------
#RANDOM FOREST
names(d)
str(d)
#Percentage of positive/negative samples in our dataset
table(d$isLandmark)/nrow(d)
#30% of the observations has target variable "yes"
#70% observations take value "no".

#Split to Training and Testing data
# ind <- sample(2,nrow(d),replace=TRUE,prob=c(0.7,0.3))
# trainData <- d[ind==1,]
# testData <- d[ind==2,]
trainData <- d[1:5160,]
testData <- d[5161:7214,]
table(trainData$isLandmark)/nrow(trainData)
table(testData$isLandmark)/nrow(testData)

#If target variable is factor, classification decision tree is built.
#We can check the type of response variable.
class(trainData$isLandmark)
trainData$isLandmark <- as.factor(trainData$isLandmark)

#Prepare the formula
varNames <- names(trainData)
#Exclude ID or Response variable
varNames <- varNames[!varNames %in% c("isLandmark")]
#Add + sign between exploratory variables
varNames1 <- paste(varNames, collapse = "+")
#Add response variable and convert to a formula object
rf.form <- as.formula(paste("isLandmark", varNames1, sep ="~"))

#Building Random forest model
#200 decision trees or a forest has been built using the Random Forest algorithm based learning
d_rf <- randomForest(rf.form, trainData, ntree=600, importance = T)

# Print Random Forest to see the importance features
print(d_rf)
plot(d_rf) #error rate across decision trees
# After 55 decision trees, there is not a significant reduction in error rate.
importance(d_rf) 
varImpPlot(d_rf, sort = T, main = "Variable Important")#Variable importance plot
#Decreasing order of importance based on a measure (1 for model accuracy and 2 node impurity)

#Variable Important Table
var_imp <- data.frame(importance(d_rf, type = 2))
#Make row names as columns
var_imp$Variables <- row.names(var_imp)
var_imp[order(var_imp$MeanDecreaseGini,decreasing = T),]

#Predict Response variable value
trainData$predicted_response <- predict(d_rf,trainData)

#Confusion matrix
confusionMatrix(data=trainData$predicted_response, reference=trainData$isLandmark,positive='1' )#,positive='1'
#Accuracy = 1

# Predict response for the testing sample and calculate model accuracy for the sample.
# Predicting response variable

testData$predicted_response <- predict(d_rf, testData)

# Create Confusion Matrix
confusionMatrix(data=testData$predicted_response,reference=testData$isLandmark, positive='1') #positive='1'

#Calculate:
predicted_response = as.character(testData$predicted_response)
predicted_response = as.numeric(predicted_response)
isLandmark = as.character(testData$isLandmark)
isLandmark = as.numeric(isLandmark)
retrieved <- sum(predicted_response)
precision <- sum(predicted_response & isLandmark) / retrieved
recall <- sum(predicted_response & isLandmark) / sum(isLandmark)
F1_score <- 2 * precision * recall / (precision + recall)
#check
precision
recall
F1_score 

# 
# #plot test
# 
# data = testData[2050:2174,c("X","Y","predicted_response")]
# a= subset(data, predicted_response==0)
# a  = cbind(a$X, a$Y)
# b= subset(data, predicted_response==1)
# b= cbind(b$X, b$Y)
# plot(c(1,1),xlim=c(-1500,3000),ylim=c(-1500,3000))
# points(a,pch =2, col="red")
# points(b,pch =2, col="blue")

#---------------------------------------------------------------------------------------------------------------

#SVM
#Explore data
names(d)
d$isLandmark <- as.factor(d$isLandmark)
table(d$isLandmark)
table(d$isLandmark)/length(d$isLandmark)

#Split into development and validation sample
# trainData <- d[1:5160,]
# table(trainData$isLandmark)/length(trainData$isLandmark)
# testData <- d[5161:7214,]
# table(testData$isLandmark)/length(testData$isLandmark)

#36 files train 11 files test
trainData <- d[1:5382,]
testData <- d[5383:7214,]

#If target variable is factor, classification decision tree is built.
#We can check the type of response variable.
class(trainData$isLandmark)
trainData$isLandmark <- as.factor(trainData$isLandmark)
#Prepare the formula
varNames <- names(trainData)
#Exclude ID or Response variable
varNames <- varNames[!varNames %in% c("isLandmark")]
#Add + sign between exploratory variables
varNames1 <- paste(varNames, collapse = "+")
#Add response variable and convert to a formula object
svm.form <- as.formula(paste("isLandmark", varNames1, sep ="~"))
svm.model <- svm(svm.form,kernel="radial", data=trainData)
# svm_tune <- tune(svm, train.x=trainData[,-3], train.y=trainData$isLandmark,
#                  kernel="radial", ranges=list(cost=c(10^seq(3,4, by=0.5)), gamma=seq(0.5,1.5, by=0.5) ))

#Loop to find best gamma and best cost

# gamma.value <- c(10^seq(-5,1, by=1))
# cost.value <-c(10^seq(-3,5, by=1))
# svm.df <-data.frame()
# for(g in gamma.value){
#   for(c in cost.value){
#     # train model
#     svm.model <- svm(svm.form,kernel="radial",
#                      gamma=g,
#                      cost=c,
#                      data=trainData)
#     testData$Predicted_isLandmark <- predict(svm.model, testData[,-3])
#     table.svm <- table(pred = testData$Predicted_isLandmark ,
#                        true = testData$isLandmark)/length(testData$isLandmark)
#     table1 <- table(testData$Predicted_isLandmark , testData$isLandmark)
#     print("at value of gamma and cost = ")
#     print(g)
#     print(c)
#     #print(table.svm)
#     print(table1)
#     print("end")
# 
#   }
# }

#Run SVM model again after finding the best cost and gamma
#Only 6 morphology features
svm.model <- svm(svm.form,kernel="radial",gamma=1,cost=1000, data=trainData)
#SURFPoints
svm.model <- svm(svm.form,kernel="radial",gamma=0.001,cost=10000, data=trainData)
#BRISKPoints
svm.model <- svm(svm.form,kernel="radial",gamma=0.001,cost=10000, data=trainData)
#FREAKPoints
svm.model <- svm(svm.form,kernel="radial",gamma=0.001,cost=10000, data=trainData)
#HOGPoints
svm.model <- svm(svm.form,kernel="radial",gamma=0.01,cost=10000, data=trainData)
#Morpho + SURFPoints
svm.model <- svm(svm.form,kernel="radial",gamma=0.001,cost=1e+05, data=trainData)
#Morpho + SURFPoints+BRISK
svm.model <- svm(svm.form,kernel="radial",gamma=0.001,cost=10000, data=trainData)
#Combine all
svm.model <- svm(svm.form,kernel="radial",gamma=0.001,cost=100, data=trainData)
#SVM + Confusion matrix
testData$Predicted_isLandmark <- predict(svm.model, testData[,-3])
table.svm <- table(pred = testData$Predicted_isLandmark ,
                   true = testData$isLandmark)/length(testData$isLandmark)
table1 <- table(testData$Predicted_isLandmark , testData$isLandmark)
# Create Confusion Matrix
confusionMatrix(data=factor(testData$Predicted_isLandmark), reference=factor(testData$isLandmark),positive='1')

#Calculate:
predicted_response = as.character(testData$Predicted_isLandmark)
predicted_response = as.numeric(predicted_response)
isLandmark = as.character(testData$isLandmark)
isLandmark = as.numeric(isLandmark)
retrieved <- sum(predicted_response)
precision <- sum(predicted_response & isLandmark) / retrieved
recall <- sum(predicted_response & isLandmark) / sum(isLandmark)
F1_score <- 2 * precision * recall / (precision + recall)
#check
precision
recall
F1_score 
#plot test

data = testData[1707:1832,c("X","Y","isLandmark","Predicted_isLandmark")]
data = testData[1579:1706,c("X","Y","isLandmark","Predicted_isLandmark")]
data = testData[1453:1578,c("X","Y","isLandmark","Predicted_isLandmark")]
confusionMatrix(data=factor(data$Predicted_isLandmark), reference=factor(data$isLandmark),positive='1')

a= subset(data, Predicted_isLandmark==0)
a  = cbind(a$X, a$Y)
b= subset(data, Predicted_isLandmark==1)
b= cbind(b$X, b$Y)
c= subset(data, isLandmark==1)
c= cbind(c$X, c$Y)
plot(c(1,1),xlim=c(-0,1800),ylim=c(0,1500))
points(a,pch =1, col="red",cex = 1.5)
points(b,pch =3, col="black",cex = 1.5)
points(c,pch =1, col="blue",cex = 1.5)

