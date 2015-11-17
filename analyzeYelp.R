## Elite Yelp Users Project
## Max K. Goff
## November 2015

# This file contains the functions used to analyze the Yelp Academic data set.
# The code presumes you have downloaded and unzipped the data set
# into a subdirectory yelp.  The code also presumes you have created
# a subdirectory ydata.  Intermediate versions of R objects are stored
# in the ydata directory.

# This code was not designed to work as a stand alone package.
# Some of the functions in this file were experimental and not used.
# The creation, testing, and modification of data was done in an
# interative manner using functions like those in this file, with
# R objects stored as intermediate placeholders along the way.
library(caret)
library(caretEnsemble)
library(dplyr)
library(tm)
library(data.table)
library(pROC)
library(mlbench)
#library(koRpus)
library(qdap)
library(gbm)
library(xgboost)
library(caTools)

rm(list=ls())
setwd('/Users/maxgoff/Dropbox/Coursera/Capstone/drill')
load('ydata/yuserDaysYelping.data')
load('ydata/reviewDataTable.data')
# some global vars
testSetG = data.frame()
trainingG = data.frame()
crossvalidationG = data.frame()
greedy_ensemble = 0
glm_ensemble = 0
gbm_ensemble = 0

dataPrep = function(){

set.seed(33883)
vfSamp = sample_n(vf, replace=FALSE, size=(floor(nrow(vf) * 0.05)))
vfSamp$FK = myFK(vfSamp$user_id)
vec = which(lapply(vfSamp[,], class) %in% c("numeric","integer","logical","factor"))
vfSampVector = vfSamp[,vec]
vfSampVector$eliteCount = NULL
vecF = vfSampVector$isElite==FALSE
vecT = vfSampVector$isElite==TRUE
vfSampVector$isElite=as.character(vfSampVector$isElite)
vfSampVector[vecF,]$isElite='no'
vfSampVector[-vecT,]$isElite='yes'
vfSampVector$isElite = as.factor(vfSamp$isElite)

return(vfSampVector)
}

modelPrep = function(){
vec = runif(1000, 1, nrow(vfSampVector))
testSet = vfSampVector[vec,]
trainSet = vfSampVector[-vec,]
inTrain = createDataPartition(trainSet$isElite, p=0.8, list=FALSE)
training = trainSet[inTrain,]
crossvalidation = trainSet[-inTrain,]
fitControl <- trainControl(method = "cv", number = 10, repeats = 2)

my_control <- trainControl(
  method='boot',
  number=25,
  savePredictions=TRUE,
  classProbs=TRUE,
  index=createResample(training$isElite, 50))
model_list = caretList(isElite ~ complimentsCount + FK, data=training, trControl=my_control, methodList=c( 'glm', 'rpart', 'xgbTree', 'dnn'))
testSetG <<- testSet
trainingG <<- training
crossvalidationG <<- crossvalidation
greedy_ensemble <<- caretEnsemble(model_list)
glm_ensemble <<- caretStack(model_list,
                    method='glm', metric='ROC',
                    trControl=trainControl( method='boot',
                            number=20, savePredictions=TRUE, classProbs=TRUE))

gbm_ensemble <<- caretStack( model_list,
                             method='gbm',
                             verbose=FALSE,
                             tuneLength=10,
                             metric='ROC',
                             trControl =trainControl(
                               method='boot',
                               number=10,
                               savePredictions=TRUE,
                               classProbs=TRUE
                             ))
return(model_list)
}

tryAUC = function(){
  library(caTools)
  testing = crossvalidationG
  model_preds <- lapply(model_list, predict, newdata=testing, type='prob')
 
  model_preds <- data.frame(model_preds)
  ens_preds <- predict(greedy_ensemble, newdata=testing)
  model_preds$ensemble <- ens_preds
  colAUC(model_preds, testing$isElite)
  }

tryEStack = function(){
  
  glm_ensemble <- caretStack(
    model_list, 
    method='glm',
    metric='ROC',
    trControl=trainControl(
      method='boot',
      number=10,
      savePredictions=TRUE,
      classProbs=TRUE))
  return(glm_ensemble)
}
xgbFit = train(isElite ~ ., data=training, method='xgbTree', trControl=fitControl)
xgbPred = predict(xgbFit, crossvalidation)
xgbCM = confusionMatrix(xgbPred, crossvalidation$isElite)

knnFit = train(isElite ~., data=training, method="knn")
knnPred = predict(knnFit, crossvalidation)
knnCM = confusionMatrix(knnPred, crossvalidation$isElite)
rpartFit = train(isElite ~ ., data=training, method="rpart")
rpartPred = predict(rpartFit, crossvalidation)
rpartCM = confusionMatrix(rpartPred, crossvalidation$isElite)
rfFit = train(isElite ~ ., data=training, method="rf", trControl = trainControl(method='cv', number=5))
rfPred = predict(rfFit, crossvalidation)
rfCM = confusionMatrix(rfPred, crossvalidation$isElite)
rfFit2 = train(isElite ~ complimentsCount + votesCount + review_count + fans + friendsCount, data=training, method='rf')
rfPred2 = predict(rfFit2, crossvalidation)
rfCM2 = confusionMatrix(rfPred2, crossvalidation$isElite)

glmFit <- train(isElite ~., model="glm", data=training, preProcess=c("center", "scale"))
glmPred = predict(glmFit, crossvalidation)
glmCM = confusionMatrix(glmPred, crossvalidation$isElite)
bagFit <- train(isElite ~., model="treebag", data=training)

bagPred = predict(bagFit, crossvalidation)
bagCM = confusionMatrix(bagPred, crossvalidation$isElite)

dnnFit = train(isElite ~., model="dnn", data=training)
dnnPred = predict(dnnFit, crossvalidation)
dnnCM = confusionMatrix(dnnPred, crossvalidation$isElite)

qrnnFit = train(isElite ~., model="qrnn", data=training)
qrnnPred = predict(qrnnFit, crossvalidation)
qrnnCM = confusionMatrix(qrnnPred, crossvalidation$isElite)

gbmFit = train(isElite ~., model="gbm", data=training)
gbmPred = predict(gbmFit, crossvalidation)
gbmCM = confusionMatrix(gbmPred, crossvalidation$isElite)

ldaFit = train(isElite ~., model="lda", data=training)
ldaPred = predict(ldaFit, crossvalidation)
ldaCM = confusionMatrix(ldaPred, crossvalidation$isElite)

qdaFit = train(isElite ~., model="qda", data=training)
qdaPred = predict(qdaFit, crossvalidation)
qdaCM = confusionMatrix(qdaPred, crossvalidation$isElite)
# Cross validation set accuracy for base Models A & B


myFK = function(userList){
  scores = as.numeric()
  for(i in 1:length(userList)){
    vec = yr$user_id == userList[i]
    txt = as.character(yr[vec,]$text)
    if( length(txt)>1){
    fk = automated_readability_index(sent_detect(replace_abbreviation(txt)))
   scores[i] = fk[,2]$Automated_Readability_Index
    }else {
      scores[i] = as.numeric(0)
    }
    if( (i %% 1000) == 0){print(paste("Pass:",i))}
#   print(paste("pass:",i))
  }
  return(scores)
}

checkList = function(){
  rm(list=ls())
  load('ydata/vfSampVector.data')
  set.seed(33883)
  vec = runif(1000, 1, nrow(vfSampVector))
  testSet = vfSampVector[vec,]
  trainSet = vfSampVector[-vec,]
  inTrain = createDataPartition(trainSet$isElite, p=0.8, list=FALSE)
  training = trainSet[inTrain,]
  crossvalidation = trainSet[-inTrain,]
  v = training$isElite=='yes'
  eliteCompsPerDay = training[v,]$complimentsCount / training[v,]$daysYelping
  regCompsPerDay = training[-v,]$complimentsCount / training[-v,]$daysYelping
  eliteRevPerDay = training[v,]$review_count / training[v,]$daysYelping
  regRevPerDay = training[-v,]$review_count / training[-v,]$daysYelping
  eliteVotesPerDay = training[v,]$votesCount / training[v,]$daysYelping
  regVotesPerDay = training[-v,]$votesCount / training[-v,]$daysYelping
  
  
  x = training$daysYelping
  png(filename='img/daysYelping.png',
      units='in', width='4', height='3', pointsize=12, res=72)
  h<-hist(x, ylim=c(0, 4000),breaks=10, col="red", xlab="Days Yelping", main="")
  xfit<-seq(min(x),max(x),length=40)
  yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
  yfit <- yfit*diff(h$mids[1:2])*(length(x))
  lines(xfit, yfit, col="blue", lwd=2)
  dev.off()
  }