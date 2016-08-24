# Setting up 
getwd()
library(ggplot2)
library(dplyr)
library(ranger)
library(pROC)
setwd( "C:/Users/Matthew Bartholomew/Documents/RStuff")

# Loading dataframes for rejected, accepted loans 
rejectedq2 = read.csv('rejectstats.csv',header = TRUE)
approvedq2 = read.csv('loanstatsfull.csv',header = TRUE)
approvedq1 = read.csv('loanstatsq1.csv', header = TRUE)


#removing a handful of NAs from end of approved frame 
approvedq2 = approvedq2[1:97854,]

#extracting month of loan from d_issue feature
approvedq2$int_rate = as.numeric(sub("%","", approvedq2$int_rate))
approvedq2$month = substring(as.character(approvedq2$issue_d),1,3)
rejectedq2$month = substring(as.character(rejectedq2$Application.Date),1,3)
approvedq1$month = substring(as.character(approvedq1$issue_d),1,3)

#plotting loan volume by month 
library(scales)
plotpart1 = cbind(approvedq2$month,rep('approved',nrow(approvedq2)))
plotpart2 = cbind(rejectedq2$month,rep('rejected',nrow(rejectedq2)))
plotwhole = as.data.frame(rbind(plotpart1,plotpart2))
names(plotwhole) = c('month','status')

ggplot(data = plotwhole, aes(x=factor(month, levels = c('Apr','May','Jun')),fill = status)) + geom_bar(position = 'dodge') + xlab('month')+ ylab('loans') + scale_y_continuous(labels = comma)
ggplot(data = approvedq2, aes(x = factor(month, levels = c('Apr','May','Jun')))) + geom_bar(fill='coral2')+ xlab('month') + ylab('loans approved')
ggplot(data = rejectedq2, aes(x = factor(month, levels = c('Apr','May','Jun')))) + geom_bar(fill='turquoise3')+ xlab('month') + ylab('loans rejected') + scale_y_continuous(labels = comma)
ggplot(data = approvedq2, aes(x = factor(month, levels = c('Apr','May','Jun')),y = loan_amnt)) + geom_bar(stat='identity', fill = 'violetred4') + xlab('month') + ylab('dollars loaned') +  scale_y_continuous(labels = comma)

#breaking each dataframe into individual months
rejected_apr = filter(rejectedq2, month == 'Apr')
rejected_may = filter(rejectedq2, month == 'May')
rejected_jun = filter(rejectedq2, month == 'Jun')

approved_apr = filter(approvedq2, month == 'Apr')
approved_may = filter(approvedq2, month == 'May')
approved_jun = filter(approvedq2, month == 'Jun')
approved_mar = filter(approvedq1, month == 'Mar')
approved_feb = filter(approvedq1, month == 'Feb')

#plotting assigned ratings by month 

ggplot(data = approved_apr, aes(x=as.numeric(sub_grade),fill=sub_grade)) + geom_histogram(bins=35) + labs(title='April Credit Ratings', x='grade rank')
ggplot(data = approved_may, aes(x=as.numeric(sub_grade),fill=sub_grade)) + geom_histogram(bins=35) + labs(title='May Credit Ratings', x='grade rank')
ggplot(data = approved_jun, aes(x=as.numeric(sub_grade),fill=sub_grade)) + geom_histogram(bins=35) + labs(title='June Credit Ratings', x='grade rank')

#investigating a1 ratings between april and june 

apr_isa1 = as.factor(as.numeric(approved_apr$sub_grade == 'A1'))
jun_isa1 = as.factor(as.numeric(approved_jun$sub_grade == 'A1'))
mar_isa1 = as.factor(as.numeric(approved_mar$sub_grade == 'A1'))
feb_isa1 = as.factor(as.numeric(approved_feb$sub_grade == 'A1'))

#removing incomplete / leaky features 
strippedq2= approvedq2[,-c(38:51,1,2,7,9, 16,17, 19,56)]
strippedq1= approvedq1[,-c(38:51,1,2,7,9, 16,17, 19,56)]


to_remove2 = c('id','member_id','grade','sub_grade','int_rate','emp_title','issue_d','url','desc','title',
               'dti_joint','application_type','loan_status','earliest_cr_line','addr_state','policy_code',
               'revol_util','verification_status','pymnt_plan','purpose','verification_status_joint',
               'term','emp_length','zip_code','home_ownership','num_tl_120dpd_2m','mths_since_recent_revol_delinq')

approved_features = rbind(strippedq2,strippedq1)

approved_features[to_remove2] = NULL

#removing / fixing NA cols 

approved_features$mths_since_last_delinq[is.na(approved_features$mths_since_last_delinq)] = 0
approved_features$mths_since_last_record = NULL
approved_features$annual_inc_joint = NULL
approved_features$dti_joint = NULL
approved_features$mths_since_last_major_derog[is.na(approved_features$mths_since_last_major_derog)] = 0
approved_features$mths_since_rcnt_il[is.na(approved_features$mths_since_rcnt_il)] = 0
approved_features$all_util[is.na(approved_features$all_util)] = 0
approved_features$il_util[is.na(approved_features$il_util)] = 0
approved_features$bc_util[is.na(approved_features$bc_util)] = 0
approved_features$bc_open_to_buy[is.na(approved_features$bc_open_to_buy)] = 0
approved_features$mo_sin_old_il_acct[is.na(approved_features$mo_sin_old_il_acct)] = 0
#approved_features$emp_title[is.na(approved_features$emp_title)] = 'Truck Driver'
approved_features$mths_since_recent_bc[is.na(approved_features$mths_since_recent_bc)] = 0
approved_features$mths_since_recent_bc[is.na(approved_features$mths_since_recent_bc)] = 0
approved_features$mths_since_recent_bc_dlq[is.na(approved_features$mths_since_recent_bc_dlq)] = 1000
#approved_features$mths_since_recent_revol_delinq[is.na(approved_features$mths_since_recent_revol_delinq)] = 1000
approved_features$mths_since_recent_inq[is.na(approved_features$mths_since_recent_inq)] = 1000
#approved_features$num_tl_120dpd_2m[is.na(approved_features$num_tl_120dpd_2m)] = 0
approved_features$percent_bc_gt_75[is.na(approved_features$percent_bc_gt_75)] = 0
#just a handful of nas left, clearing in one swoop 
approved_features = na.exclude(approved_features) 

colSums(is.na(approved_features))

#splitting features by month 

jun_features = filter(approved_features, month == 'Jun')
apr_features = filter(approved_features, month == 'Apr')
mar_features = filter(approved_features, month == 'Mar')
feb_features = filter(approved_features, month == 'Feb')

#predicting number of A1 loans using ranger(random forest)

apr_features=cbind(apr_features,apr_isa1)
train_index = c(1:floor(.8*nrow(apr_features)))
apr_train = apr_features[train_index,]
apr_test = apr_features[-train_index,]
apr_test_true = apr_test$apr_isa1
apr_test$apr_isa1 = NULL




apr_model_test = ranger(formula = apr_isa1 ~ . , data=apr_train, importance = 'impurity',write.forest = TRUE, probability = TRUE, mtry = 40 )
apr_test_prediction = predict(apr_model_test,apr_test)
sum(apr_test_prediction$predictions[,2]) # predicts 771 A1 rated loans in test set 
sum(as.numeric(as.character(apr_test_true))) # This compares to 715 actual A1 loan in the test set. Pretty good. 

#now testing validity of model of entire april sample predicting march

apr_model_full =  ranger(formula = apr_isa1 ~ . , data=apr_features, importance = 'impurity',write.forest = TRUE,min.node.size = 1, probability = TRUE, mtry = 30 )
apr_full_prediction = predict(apr_model_full, mar_features)
sum(apr_full_prediction$predictions[,2]) 
sum(as.numeric(as.character(mar_isa1)))

#mar predicting april 
mar_model_features = cbind(mar_features,mar_isa1)
apr_features$apr_isa1 = NULL
mar_model = ranger(formula = mar_isa1 ~ . , data=mar_model_features, importance = 'impurity',write.forest = TRUE,min.node.size = 1, probability = TRUE, mtry = 40 )
mar_model_prediction = predict(mar_model,apr_features)
sum(mar_model_prediction$predictions[,2]) 
sum(as.numeric(as.character(apr_isa1)))

#mar predicting mar 
mar_train_index = c(1:floor(.8*nrow(mar_features)))
mar_train_features = cbind(mar_features,mar_isa1)
mar_train = mar_train_features[train_index,]
mar_test = mar_train_features[-train_index,]
mar_test_true = mar_test$mar_isa1
apr_test$apr_isa1 = NULL

mar_mar_model = ranger(formula = mar_isa1 ~ . , data=mar_train, importance = 'impurity',write.forest = TRUE,min.node.size = 1, probability = TRUE, mtry = 40 )
mar_mar_prediction = predict(mar_mar_model,mar_test)
sum(mar_mar_prediction$predictions[,2]) 
sum(as.numeric(as.character(mar_test_true)))

#feb predicting march
feb_model_features = cbind(feb_features,feb_isa1)
feb_model = ranger(formula = feb_isa1 ~ . , data=feb_model_features, importance = 'impurity',write.forest = TRUE,min.node.size = 1, probability = TRUE, mtry = 40 )
feb_model_prediction = predict(feb_model,mar_features)
sum(feb_model_prediction$predictions[,2]) 
sum(as.numeric(as.character(mar_isa1)))

#and at last april predicting jun

apr_jun_prediction = predict(apr_model_full,jun_features)
sum(apr_jun_prediction$predictions[,2])
sum(as.numeric(as.character(jun_isa1)))

#howsabout march predicting jun
mar_jun_prediction = predict(mar_model,jun_features)
sum(mar_jun_prediction$predictions[,2])
sum(as.numeric(as.character(jun_isa1)))



# last environment save made here. All below may need to be run again in event of crash 
#also examine differences in interest rates between A1, lower A rated loans 


#now to model interest rates for April, then apply to June features 
#returning interest rate feature to April and Jun dfs 

jun_features_ir = cbind(jun_features,approved_jun$int_rate)
names(jun_features_ir)[74] = 'int_rate'
jun_features_ir$month = NULL

apr_features_ir = cbind(apr_features,approved_apr$int_rate)
apr_features_ir$apr_isa1 = NULL
names(apr_features_ir)[74] = 'int_rate'
apr_features_ir$month=NULL

#apr_features_ir$approved_apr$int_rate = NULL

library(caret)
caret_reg = function(x, y, method, grid, ...) {
  set.seed(1)
  control = trainControl(method="repeatedcv", repeats=1,
                         number=3, verboseIter=TRUE)
  train(x=x, y=y, method=method, tuneGrid=grid,
        trControl=control, metric="RMSE",
        preProcess=c("center", "scale"), ...)
}

target = approved_apr$int_rate
features = select(apr_features,-apr_isa1)      
grid = data.frame(mtry=c(10,20,30,40))     

tune_int_rate = caret_reg(features,target, 'ranger',grid)     
tune_int_rate$results
#using mtry = 40 

apr_features_ir_train = apr_features_ir[train_index,]
apr_features_ir_test = apr_features_ir[-train_index,]

apr_features_ir_model_part = ranger(formula = int_rate ~ . , data=apr_features_ir_train, importance = 'impurity',write.forest = TRUE, mtry = 40 )
apr_it_prediction = predict(apr_features_ir_model_part,select(apr_features_ir_test,-int_rate))
mean(apr_it_prediction$predictions) #12.45922
mean(approved_apr$int_rate) #12.43423

#now applying full apr IR model to jun loans 
apr_ir_model_full = ranger(formula = int_rate ~ . , data=apr_features_ir, importance = 'impurity',write.forest = TRUE, mtry = 40 )

apr_jun_ir_prediction = predict(apr_ir_model_full, select(jun_features_ir,-int_rate))
mean(apr_jun_ir_prediction$predictions) # 12.19748
mean(approved_jun$int_rate) #12.51205
mean(approved_apr$int_rate) #12.43423

#coming up with a big, sexy number 

jun_loans_counterfactual = select(approved_jun, c(loan_amnt, int_rate, term))
jun_loans_counterfactual = cbind(jun_loans_counterfactual, apr_jun_ir_prediction$predictions)
names(jun_loans_counterfactual)[4] = 'alt_int_rate' 
jun_loans_counterfactual$term = as.numeric(substring(as.character(jun_loans_counterfactual$term),2,3))
jun_loans_counterfactual$int_rate = jun_loans_counterfactual$int_rate/100
jun_loans_counterfactual$alt_int_rate = jun_loans_counterfactual$alt_int_rate/100



repayment_true = vector(length = nrow(jun_loans_counterfactual))
for (i in 1:nrow(jun_loans_counterfactual)){
  repayment_true[i]=sum(amortize(jun_loans_counterfactual$loan_amnt[i],jun_loans_counterfactual$int_rate[i],jun_loans_counterfactual$term[i],'payment')[,2])
}

repayment_projected = vector(length = nrow(jun_loans_counterfactual))
for (i in 1:nrow(jun_loans_counterfactual)){
  repayment_projected[i]=sum(amortize(jun_loans_counterfactual$loan_amnt[i],jun_loans_counterfactual$alt_int_rate[i],jun_loans_counterfactual$term[i],'payment')[,2])
}

sum(repayment_true)-sum(repayment_projected)
#619647575 - 607039770 = 12607805 (!)

  
str(jun_loans_counterfactual)



# The MIT License (MIT)
#
# Copyright (c) 2012 Schaun Jacob Wheeler
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.


amortize <- function(p_input = 25000, i_input = .10, n_months = 36 , 
                     output = "table", index = NULL) { 
  
  n_months <- rep(n_months, length(p_input))
  
  if(is.null(index)) {
    index <- matrix(rep(1:length(n_months), each = n_months[1]), 
                    nrow = n_months[1])
  } else {
    index <- matrix(rep(index, each = n_months[1]), nrow = n_months[1])
  }
  
  p_input <- matrix(p_input, ncol = length(p_input))
  i_input <- matrix(i_input, ncol = length(i_input))
  i_monthly <- i_input / (12)
  payment <- p_input * i_monthly / (1 - (1 + i_monthly)^(-n_months[1]))
  
  Pt <- p_input # current principal or amount of loan
  currP <- NULL
  
  for(i in 1:n_months[1]) {
    H <- Pt * i_monthly # current monthly interest
    C <- payment - H # monthly payment minus monthly interest (principal paid for each month)
    Q <- Pt - C # new balance of principal of loan
    Pt <- Q # loops through until balance goes to zero
    currP <- rbind(currP, Pt)    
  }
  
  amortization <- rbind(p_input, currP[1:(n_months[1]-1),, drop = FALSE])
  monthly_principal <- amortization - currP
  monthly_interest <- rbind(
    (matrix(
      rep(payment, n_months[1]), 
      nrow = n_months[1], 
      byrow = TRUE) - monthly_principal)[1:(n_months[1]-1),, drop = FALSE],
    rep(0, length(n_months)))
  monthly_interest[1:nrow(monthly_interest) %% 12 == 0] <-
    monthly_principal[1:nrow(monthly_interest) %% 12 == 0] * i_monthly
  monthly_payment <- monthly_principal + monthly_interest
  installment <- matrix(rep(1 : n_months[1], length(n_months)), 
                        nrow = n_months[1])
  
  input <- list(
    "amortization" = amortization,
    "payment" = monthly_payment,
    "principal" = monthly_principal,
    "interest" = monthly_interest,
    "installment" = installment,
    "index" = index)
  
  out <- switch(output, 
                "list" = input,
                "table" = as.data.frame(
                  lapply(input, as.vector), 
                  stringsAsFactors = FALSE),
                "balance" = as.data.frame(
                  lapply(input[c("index", "amortization")], as.vector), 
                  stringsAsFactors = FALSE),
                "payment" = as.data.frame(
                  lapply(input[c("index", "payment")], as.vector), 
                  stringsAsFactors = FALSE),
                "principal" = as.data.frame(
                  lapply(input[c("index", "principal")], as.vector), 
                  stringsAsFactors = FALSE), 
                "interest" = as.data.frame(
                  lapply(input[c("index", "interest")], as.vector), 
                  stringsAsFactors = FALSE)
  )
  
  out
}
