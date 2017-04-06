############# Installing packages #######################
if(!require("dplyr")){install.packages("dplyr")}
if(!require("ggplot2")){install.packages("ggplot2")}
if(!require("caret")){install.packages("caret")}
if(!require("corrplot")){install.packages("corrplot")}
if(!require("RColorBrewer")){install.packages("RColorBrewer")}
if(!require("gridExtra")){install.packages("gridExtra")}


############## reading the data from csv file #######################
####### skips the first row while reading csv file #########
credit_data = read.csv("default of credit card clients.csv",header = T,skip = 1)
attach(credit_data)
#credit_data = credit_data[,-1]

############# Data Exploration & Imputation #################
round(prop.table(table(default.payment.next.month))*100)
### the data is completely unbalanced with more number of non-defaulters than defaulters 

##### 1. Limit balance variable ########
sum(is.na(LIMIT_BAL))   ### no missing values
summary(LIMIT_BAL)
hist(LIMIT_BAL,col='steelblue', breaks = 30)


##### 3. education ########
sum(is.na(EDUCATION))
str(EDUCATION) ### Education has more levels than described levels 0,4,5,6 are being changed to 0 "unknown"
credit_data$EDUCATION[credit_data$EDUCATION == 4 | credit_data$EDUCATION == 5 | credit_data$EDUCATION == 6]  = 0

##### 4. Marriage ####### 
sum(is.na(MARRIAGE))
str(MARRIAGE) ### Education has more levels than described levels 0,4,5,6 are being changed to 0 "unknown"
credit_data$MARRIAGE[credit_data$MARRIAGE == 0 ]  = 3


##### Correlation plot between the variables ###### 
corrplot.mixed(cor(credit_data), lower="circle", upper="color", 
                               tl.pos="lt", diag="n", order="hclust", hclust.method="complete")


############## Generating new variables for data analysis #############
credit_data = mutate(credit_data,repayment_ratio = apply(credit_data[,c(19:24)],1,sum) / apply(credit_data[,c(13:18)],1,sum))
credit_data = mutate(credit_data, avg_bill = apply(credit_data[,c(13:18)],1,mean))
credit_data = mutate(credit_data, avg_repayment = apply(credit_data[,c(19:24)],1,mean))
credit_data = mutate(credit_data, amt_limit_ratio = avg_bill/LIMIT_BAL)
credit_data = mutate(credit_data, amt_owed = apply(credit_data[,c(13:18)],1,sum) - apply(credit_data[,c(19:24)],1,sum))
credit_data = mutate(credit_data, no_of_missing_payments = rowSums(credit_data[7:12] > 0))


############# Generating new variables for box plots & histogram generation #############
credit_data$workstateH =  ""
credit_data$genderH  =  ""
credit_data$educationH = ""
credit_data$maritalH  =  ""
credit_data$workstate = ""

for (i in 1:nrow(credit_data)) {
  if ((credit_data[i,"PAY_0"] + credit_data[i,"PAY_2"] +credit_data[i,"PAY_3"]
       +credit_data[i,"PAY_4"] +credit_data[i,"PAY_5"]+credit_data[i,"PAY_6"]) <= 0){
    credit_data[i,"workstateH"] = "YES" 
    credit_data[i,"workstate"] = 1
  }
  else {
    credit_data[i,"workstateH"] = "NO"       
    credit_data[i,"workstate"] = 0
  }
}

for (i in 1:nrow(credit_data)) {
  if (credit_data[i,"SEX"] == 1) {
    credit_data[i,"genderH"] <- "Male"  
  }
  else {
    credit_data[i,"genderH"] <- "Female"         
  }
}

for (i in 1:nrow(credit_data)) {
  if (credit_data[i,"EDUCATION"] == 1) {
    credit_data[i,"educationH"] <- "Graduate"
  } else if (credit_data [i,"EDUCATION"] == 2) {
    credit_data[i,"educationH"] <- "University" 
  } else if (credit_data [i,"EDUCATION"] == 3) {
    credit_data[i,"educationH"] <- "High School" 
  } else {
    credit_data[i,"educationH"] <- "Unknown" 
  }
}

for (i in 1:nrow(credit_data)) {
  if(credit_data[i,"MARRIAGE"] == 1) {
    credit_data[i,"maritalH"] <- "Married"
  } else if (credit_data[i,"MARRIAGE"] == 2) {
    credit_data[i,"maritalH"] <- "Single"
  } else {
    credit_data[i,"maritalH"] <- "Other"
  }
}

############## Generating new variables by binning actual variables ############
credit_data = mutate(credit_data,age_bucket=cut(AGE,c(10,20,30,40,50,60,70,80)))

######## Make variables factors into factors
factor_vars = c('SEX','EDUCATION','MARRIAGE','default.payment.next.month','workstate')
credit_data[factor_vars] = lapply(credit_data[factor_vars], function(x) as.factor(x))

################ Relationship between variables ############ 
# Balance limits by gender and education
d1 <- ggplot(credit_data, aes(factor(genderH), (LIMIT_BAL/1000), fill=educationH)) + 
  geom_boxplot() +
  xlab("Gender") + 
  ylab("BLimit(x1000 NT$)") + 
  scale_fill_brewer(palette = "Accent")

# Balance limits by education and gender
d2 <- ggplot(credit_data, aes(factor(educationH), (LIMIT_BAL/1000), fill=genderH)) + 
  geom_boxplot() +
  xlab("Education") + 
  ylab("BLimit(x1000 NT$)") + 
  scale_fill_brewer(palette = "Paired")

# Balance limits by workstate and education
d3 <-ggplot(credit_data, aes(factor(educationH), (LIMIT_BAL/1000), fill=workstateH)) + 
  geom_boxplot() +
  xlab("Education") + 
  ylab("BLimit(x1000 NT$)") 

grid.arrange(d1, d2, d3)


#### Relation Between Marital Status & Balance Limits By Gender
ggplot(credit_data, aes(factor(maritalH), (LIMIT_BAL/1000), fill=genderH)) + 
  geom_boxplot() +
  xlab("Marital Status") + 
  ylab("Balance Limit ( x 1000 NT$)") + 
  coord_cartesian(ylim = c(0,350)) +
  scale_fill_brewer(palette = "Paired")

#### Relation Between Marital Status & default payment status By Gender
ggplot(credit_data, aes(x=default.payment.next.month),aes(y=stat_count(gender))) + 
  geom_bar(aes(fill=factor(credit_data$maritalH))) +
  xlab("Default Payment Status")+ylab("Customer Count") + 
  facet_wrap(~genderH)+
  scale_fill_discrete(name="Marital status")


##### Histogram of Limit Balance & Default Payment
ggplot(aes(x = credit_data$LIMIT_BAL/1000), data = credit_data) +
  geom_histogram(aes(fill = credit_data$default.payment.next.month)) +
  xlab("Balance Limit x 1000") +
  ylab("Count") +
  scale_fill_discrete(name="Default Payment Next Month",
                      breaks=c(0, 1),
                      labels=c("No", "Yes")) +
  xlim(c(0,750)) +
  facet_wrap(~educationH)

#### Relation Between Education & Default Payment
d1 <- ggplot(credit_data, aes(x=default.payment.next.month)) + 
  geom_histogram(stat="count",color='red',fill='orange') +
  xlab("Default Payment Status") + ylab("Customer Count") + 
  facet_wrap(~educationH)

d2 <- ggplot(credit_data, aes(x=default.payment.next.month),aes(y=stat_count(gender))) + 
  geom_bar(aes(fill=factor(credit_data$educationH))) +
  xlab("Default Payment Status")+ylab("Customer Count") + 
  facet_wrap(~genderH)+
  scale_fill_discrete(name="Education")

grid.arrange(d1, d2, ncol=1)

#### Balance Limits By Age Groups & Education
ggplot(data = subset(credit_data,!is.na(age_bucket)), aes(factor(educationH), (LIMIT_BAL/1000), fill=age_bucket)) + 
  geom_boxplot() +
  xlab("Education") + 
  ylab("Balance Limit ( x 1000 NT$)") + 
  coord_cartesian(ylim = c(0,500)) +
  scale_fill_brewer(palette = "Accent")


##### Personal Balance Limits Probabilities & Given Limits By Age
ggplot(aes(x=AGE,y=LIMIT_BAL/1000),data=subset(credit_data,!is.na(age_bucket)))+
  xlab("Age") + 
  ylab("Balance Limit (x1000 NT$)") +
  coord_cartesian(xlim = c(21,60),ylim = c(0,700))+
  scale_color_brewer(palette = "Pastel1")+
  geom_jitter(alpha=0.5, position = position_jitter(h=0), aes(color=age_bucket)) +
  geom_smooth(stat='summary', fun.y=mean) +
  geom_smooth(stat='summary', fun.y=quantile, fun.args = list(probs = 0.1), color = 'black', linetype=2) +
  geom_smooth(stat='summary', fun.y=quantile, fun.args = list(probs = 0.5), color = 'red', linetype=2) +
  geom_smooth(stat='summary', fun.y=quantile, fun.args = list(probs = 0.9), color = 'black', linetype=2)


############# Outlier detection & removal ############ 
##### Removing outliers with bill amount to bill limit 
##### Persons with ratio of billing amount to bill limit greater than 1.25 & with non-default status considered as outliers 
credit_data_outlier = credit_data[which(credit_data$amt_limit_ratio>1.25 ),]
credit_data_outlier = credit_data_outlier[which( credit_data_outlier$default.payment.next.month==0),]

credit_data = credit_data[!(credit_data$ID %in% credit_data_outlier$ID),]

##### Removing outliers wrt to repayment to bill amount ratio
##### 
outlier_age_bin = credit_data[which(credit_data$AGE > 20 & credit_data$AGE <=30 & credit_data$EDUCATION == 3),]
outlier_age_bin = outlier_age_bin[which(outlier_age_bin$repayment_ratio < 0.04 & outlier_age_bin$workstateH == "NO" 
                                        & outlier_age_bin$default.payment.next.month == 0),]
credit_data = credit_data[!(credit_data$ID %in% outlier_age_bin$ID),]

rm(credit_data_outlier,outlier_age_bin,d1,d2,d3,factor_vars,i)
