#load library for machine learning
if(!require("mlr")){install.packages("mlr")}
if(!require("FSelector")){install.packages("FSelector")}


##########  partitioning data for training & testing########### 
set.seed(36)
trainIndex  =  createDataPartition(data_model$default.payment.next.month, p = .7, list = FALSE, times = 1)
ntrain_bay = data_model[trainIndex,]
ntest_bay = data_model[-trainIndex,]


######## creating training & testing tasks ########### 
train.task = makeClassifTask(data = ntrain_bay,target = "default.payment.next.month")
test.task =  makeClassifTask(data = ntest_bay,target = "default.payment.next.month")

########## removing zero variance features for model building ##### 
train.task = removeConstantFeatures(train.task)
test.task = removeConstantFeatures(test.task)

########## preparing data for undersampling ############
train.under <- undersample(train.task,rate = 0.5) # keep only 50% of majority class
table(getTaskTargets(train.under))

########## preparing data for oversampling ############
train.over <- oversample(train.task,rate=3) ### making minority class 3 times
table(getTaskTargets(train.over))

########## preparing data for smote ############
train.smote <- smote(train.task,rate = 4,nn = 5) #### considering 5 nearest neighbours for data points
table(getTaskTargets(train.smote))


######################## Data partitioning is done ############## ###############
###### The same partitioned data will be used for building naive bayes & Xg boosting algorithm #########33


################# Naive bayes model building ######################## 
####### constructing naive Bayes classifier ########### 
naive_learner = makeLearner("classif.naiveBayes",predict.type = "response")
naive_learner$par.vals = list(laplace = 1)

########## 10 fold stratified cross validation ########## 
folds = makeResampleDesc("CV",iters=10,stratify = TRUE)

##### cross validation function
 fun_cv <- function(a){
  crv_val <- resample(naive_learner,a,folds,measures = list(acc,tpr,tnr,fpr,fp,fn))
  crv_val$aggr
 }
 
 
 fun_cv (train.task) 
 #acc.test.mean tpr.test.mean tnr.test.mean fpr.test.mean  fp.test.mean  fn.test.mean 
 #0.7517964     0.7937882     0.6010403     0.3989597   176.5000000   327.6000000 
 
 fun_cv(train.under)
 #acc.test.mean tpr.test.mean tnr.test.mean fpr.test.mean  fp.test.mean  fn.test.mean 
 #0.7038134     0.4128374     0.8083486     0.1916514    84.8000000    93.3000000
 
 fun_cv(train.smote)
 #acc.test.mean tpr.test.mean tnr.test.mean fpr.test.mean  fp.test.mean  fn.test.mean 
 #6.371888e-01  2.407140e-01  9.219259e-01  7.807414e-02  1.727000e+02  1.206200e+03 
 
 func_cv(train.over)
 #acc.test.mean tpr.test.mean tnr.test.mean fpr.test.mean  fp.test.mean  fn.test.mean 
 #0.6624216     0.5045339     0.7758137     0.2241863   495.9000000   787.1000000
 
############# it is found that under sampling, over samplling & smote has lesser accuracy & low true positive rate
############# compared to unbalanced data ########
 
 
########  traininig and prediction #########
#### naive baies classifier is built with unbalanced data #########3
nB_model  =  train(naive_learner, train.task)
nB_predict = predict(nB_model,test.task)
 
######### evaluating the testing data ####### 
nB_prediction = nB_predict$data$response
dCM = confusionMatrix(ntest_bay$default.payment.next.month,nB_prediction,positive = "1")

########### Metrics results for test.task data#########

##### model built using unbalanced data #### 
#Accuray: 0.7755 
#Sensitivity : 0.4865          
#Specificity : 0.8724          
#Pos Pred Value : 0.5615          
#Neg Pred Value : 0.8350          
#Prevalence : 0.2513          
#Detection Rate : 0.1223          
#Detection Prevalence : 0.2178          
#Balanced Accuracy : 0.6795          

#'Positive' Class : 1      


#### Model built using under sampling ####
#Accuray: 0.6041 
#Sensitivity : 0.3218          
#Specificity : 0.8863          
#Pos Pred Value : 0.7388          
#Neg Pred Value : 0.5666          
#Prevalence : 0.4999          
#Detection Rate : 0.1609          
#Detection Prevalence : 0.2178          
#Balanced Accuracy : 0.6040          

#'Positive' Class : 1      


#### Model built using over sampling ####
#Accuray: 0.6575 
#Sensitivity : 0.3544          
#Specificity : 0.8845          
#Pos Pred Value : 0.6966          
#Neg Pred Value : 0.6467          
#Prevalence : 0.4281          
#Detection Rate : 0.1517          
#Detection Prevalence : 0.2178          
#Balanced Accuracy : 0.6194          

#'Positive' Class : 1  


#### Model built using smote ####
#Accuray: 0.4236 
#Sensitivity : 0.2584         
#Specificity : 0.8992         
#Pos Pred Value : 0.8807         
#Neg Pred Value : 0.2963         
#Prevalence : 0.7422         
#Detection Rate : 0.1918         
#Detection Prevalence : 0.2178         
#Balanced Accuracy : 0.5788         

#'Positive' Class : 1           