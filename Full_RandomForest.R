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


setwd("C:/Users/Admin/Documents/Intern/Extract feature/Internship/Data/Training_data")
temp = list.files(pattern="*.csv")
# Seperate file into list for training and testing
ind <- sample(2,length(temp),replace=TRUE,prob=c(0.7,0.3))
listTrainFile <- temp[ind==1]
listTestFile <- temp[ind==2]

# Names feature header
init_header = c("X", "Y", "isLandmark")
surf_header = c()
brisk_header = c()
hog_header = c()
freak_header = c()
for(i in 1:64){
  surf_header = c(surf_header, paste0("SURFfeature", as.character(i)))
  brisk_header = c(brisk_header, paste0("BRISKfeature", as.character(i)))
  freak_header = c(freak_header, paste0("FREAKfeature", as.character(i)))
}
for(i in 1:36){
  hog_header = c(hog_header, paste0("HOGfeature", as.character(i)))
}


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

# process train data
trainData = data.frame()
for (i in 1:length(listTrainFile)){ 
  data = read.csv(listTrainFile[i], sep = ',', header = TRUE)
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
  output <-  X[,c(init_header,colnames(a),colnames(output1)[1],
                  colnames(output1)[2],colnames(output1)[3],colnames(output1)[4],
                  surf_header, brisk_header,  hog_header, freak_header)]
  #merge data
  trainData = rbind(trainData, output)  
}


# process test data
testData = data.frame()
for (i in 1:length(listTestFile)){ 
  data = read.csv(listTestFile[i], sep = ',', header = TRUE)
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
  output <-  X[,c(init_header,colnames(a),colnames(output1)[1],
                  colnames(output1)[2],colnames(output1)[3],colnames(output1)[4],
                  surf_header, brisk_header,  hog_header, freak_header)]
  #merge data
  testData = rbind(testData, output)  
}


#RANDOM FOREST

#Oversampling data, 50:50
#trainData
table(trainData$isLandmark)/nrow(trainData)
temp_Landmark= subset(trainData, isLandmark==1)
temp_notLandmark= subset(trainData, isLandmark!=1)
a <- temp_Landmark[sample(nrow(temp_Landmark),
                          size=( nrow(temp_notLandmark) - nrow(temp_Landmark)),
                          replace=TRUE),]
trainData1<-rbind(trainData, a)
table(trainData1$isLandmark)/nrow(trainData1)

#testData
temp_Landmark1= subset(testData, isLandmark==1)
temp_notLandmark1= subset(testData, isLandmark!=1)
a <- temp_Landmark1[sample(nrow(temp_Landmark1),
                           size=( nrow(temp_notLandmark1) - nrow(temp_Landmark1)),
                           replace=TRUE),]
testData1<-rbind(testData, a)
table(testData1$isLandmark)/nrow(testData1)


#Data
#Morphology
trainData <- trainData1[,c(init_header,"Distance","UP","DOWN","LEFT","RIGHT")]
testData <- testData1[,c(init_header,"Distance","UP","DOWN","LEFT","RIGHT")]
##SURFPoints
trainData <- trainData1[,c(init_header, surf_header)]
testData <- testData1[,c(init_header, surf_header)]
#BRISKPoints
trainData <- trainData1[,c(init_header,brisk_header)]
testData <- testData1[,c(init_header,brisk_header)]
#FREAKPoints
trainData <- trainData1[,c(init_header,freak_header)]
testData <- testData1[,c(init_header,freak_header)]
#HOGPoints
trainData <- trainData1[,c(init_header,hog_header)]
testData <- testData1[,c(init_header,hog_header)]
#Morph + SURF
trainData <- trainData1[,c(init_header,"Distance","UP","DOWN","LEFT","RIGHT",surf_header)]
testData <- testData1[,c(init_header,"Distance","UP","DOWN","LEFT","RIGHT",surf_header)]
#Morphology + SURF + BRISK
trainData <- trainData1[,c(init_header,"Distance","UP","DOWN","LEFT","RIGHT",surf_header, brisk_header)]
testData <- testData1[,c(init_header,"Distance","UP","DOWN","LEFT","RIGHT",surf_header, brisk_header)]
#Morphology + SURF + BRISK + HOG + FREAK
trainData <- trainData1[,c(init_header,"Distance","UP","DOWN","LEFT","RIGHT",surf_header, brisk_header, hog_header, freak_header)]
testData <- testData1[,c(init_header,"Distance","UP","DOWN","LEFT","RIGHT",surf_header, brisk_header, hog_header, freak_header)]


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
#decision trees or a forest has been built using the Random Forest algorithm based learning
d_rf <- randomForest(rf.form, trainData, ntree=600, mtry =3, importance = T)

# Print Random Forest to see the importance features
print(d_rf)
plot(d_rf) #error rate across decision trees
# After decision trees, there is not a significant reduction in error rate.
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

#Plot F_ST03_06.22.11_LT_02.tif for illustration

#plot test

setwd("C:/Users/Admin/Documents/Intern/Extract feature/Internship/Data/Training_data")
#data = testData[10466:10570,c("X","Y","isLandmark","predicted_response")]
dat = testData[1:95,c("X","Y","isLandmark","predicted_response")]
confusionMatrix(data=factor(dat$predicted_response), reference=factor(dat$isLandmark),positive='1')
Y = 1296 -  dat[,2] 
dat1 <- dat[,c("X","isLandmark","predicted_response")]
dat <- cbind(dat1,Y)

a= subset(dat, predicted_response==0)
a  = cbind(a$X, a$Y)
b= subset(dat, predicted_response==1)
b= cbind(b$X, b$Y)
c= subset(dat, isLandmark==1)
c= cbind(c$X, c$Y)
#plot(c(1,1),xlim=c(0,2500),ylim=c(0,1500))

library('tiff')
setwd("C:/Users/Admin/Documents/Intern/Extract feature/Internship/Data/IMG")
img = readTIFF('F_ST03_06.22.11_LT_02.tif')
plot(c(1,1),xlim=c(0,1728),ylim=c(0,1296), type='n', xlab="x", ylab="y")
lim <- par()
rasterImage(img, 0, 0, 1728, 1296)
grid()

points(a,pch =1, col="red",cex = 1.5)
points(b,pch =3, col="black",cex = 1.5)
points(c,pch =1, col="blue",cex = 1.5)

#F_ST03_07.21.11_LT_03.tif
setwd("C:/Users/Admin/Documents/Intern/Extract feature/Internship/Data/Training_data")
#data = testData[10466:10570,c("X","Y","isLandmark","predicted_response")]
#dat = testData[1:95,c("X","Y","isLandmark","predicted_response")]
dat = testData[96:259,c("X","Y","isLandmark","predicted_response")]
confusionMatrix(data=factor(dat$predicted_response), reference=factor(dat$isLandmark),positive='1')
Y = 1944 -  dat[,2] 
dat1 <- dat[,c("X","isLandmark","predicted_response")]
dat <- cbind(dat1,Y)

a= subset(dat, predicted_response==0)
a  = cbind(a$X, a$Y)
b= subset(dat, predicted_response==1)
b= cbind(b$X, b$Y)
c= subset(dat, isLandmark==1)
c= cbind(c$X, c$Y)
#plot(c(1,1),xlim=c(0,2500),ylim=c(0,1500))

library('tiff')
setwd("C:/Users/Admin/Documents/Intern/Extract feature/Internship/Data/IMG")
#img = readTIFF('F_ST03_06.22.11_LT_02.tif')
img = readTIFF('F_ST03_07.21.11_LT_03.tif')
plot(c(1,1),xlim=c(0,2592),ylim=c(0,1944), type='n', xlab="x", ylab="y")
lim <- par()
rasterImage(img, 0, 0, 2592, 1944)
grid()

points(a,pch =1, col="red",cex = 1.5)
points(b,pch =3, col="black",cex = 1.5)
points(c,pch =1, col="blue",cex = 1.5)


