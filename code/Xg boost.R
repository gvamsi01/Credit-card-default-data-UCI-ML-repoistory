############ Data modelling with Xg boost ############
if(!require("xgboost")){install.packages("xgboost")}

########### Setting the value parameters for XG Boosting ########## 
set.seed(2002)
xgb_learner  =  makeLearner("classif.xgboost",predict.type = "response")
xgb_learner$par.vals =  list( objective = "binary:logistic", eval_metric = "error",nrounds = 150, print.every.n = 50)

########## defining hyper parameters for tuning ############# 
xg_ps <- makeParamSet( 
  makeIntegerParam("max_depth",lower=3,upper=10),
  makeNumericParam("lambda",lower=0.05,upper=0.5),
  makeNumericParam("eta", lower = 0.01, upper = 0.5),
  makeNumericParam("subsample", lower = 0.50, upper = 1),
  makeNumericParam("min_child_weight",lower=2,upper=10),
  makeNumericParam("colsample_bytree",lower = 0.50,upper = 0.80))

############ defining search function############ 
rancontrol = makeTuneControlRandom(maxit = 5L) ### 5 iterations 

########### 5 fold cross validation ################
set_cv =  makeResampleDesc("CV",iters = 5L,stratify = TRUE)

######## Parameters tuning ############## 
xgb_tune =  tuneParams(learner = xgb_learner, task = train.task, resampling = set_cv, measures = list(acc,tpr,tnr,fpr,fp,fn), par.set = xg_ps, control = rancontrol)


####### setting optimal parameters for tuning ##########
xgb_new = setHyperPars(learner = xgb_learner, par.vals = xgb_tune$x)

######## Training the model ######
xgmodel = train(xgb_new, train.task)

######## Testing the model ##########
predict.xg = predict(xgmodel, test.task)

#make prediction
xg_prediction = predict.xg$data$response

#make confusion matrix
xg_confused = confusionMatrix(ntest_bay$default.payment.next.month,xg_prediction,positive = "1")

xg_confused

########### Metrics results for test.task data#########

##### model built using unbalanced data #### 
#Accuracy 0.8243
#Sensitivity : 0.68191         
#Specificity : 0.84291         
#Pos Pred Value : 0.36201         
#Neg Pred Value : 0.95299         
#Prevalence : 0.11561         
#Detection Rate : 0.07883         
#Detection Prevalence : 0.21777         
#Balanced Accuracy : 0.76241         

#'Positive' Class : 1  


##### model built using over sampling data #### 
#Accuracy 0.7917
#Sensitivity : 0.55182         
#Specificity : 0.85103         
#Pos Pred Value : 0.43272         
#Neg Pred Value : 0.90216        
#Prevalence : 0.17077         
#Detection Rate : 0.09423         
#Detection Prevalence : 0.2178         
#Balanced Accuracy : 0.70142    


##### model built using under sampling data #### 
#Accuracy 0.7699
#Sensitivity : 0.4740          
#Specificity : 0.8614          
#Pos Pred Value : 0.5140          
#Neg Pred Value : 0.8412          
#Prevalence : 0.2362          
#Detection Rate : 0.1119          
#Detection Prevalence : 0.2178          
#Balanced Accuracy : 0.6677          

#'Positive' Class : 1    


##### model built using smote data #### 
#Accuracy 0.8074
#Sensitivity : 0.58314        
#Specificity : 0.84739        
#Pos Pred Value : 0.40528        
#Neg Pred Value : 0.91935        
#Prevalence : 0.15134        
#Detection Rate : 0.08826        
#Detection Prevalence : 0.21777        
#Balanced Accuracy : 0.71527        

#'Positive' Class : 1   