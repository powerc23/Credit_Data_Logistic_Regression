require(MASS)
require(caret)

credit_data <- read.table("CreditData.txt",sep = " ",header=FALSE, na.strings = "NA")

#Setting seed with randomly chosen number between 1 and 1 million (commented out after 1 run so number won't change)
#Setting seed ensures training and test data remain the same
#rand_num = sample(1:1000000,1)
#rand_num
set.seed(762065)

#Renaming column headers
colnames(credit_data) <- c("ac_status","cred_dur","cred_hist","purpose","cred_amt","sav_ac","empl_since","inst_rate",
                           "status_sex","other_debtors","res_since","property","age","other_plans","housing","num_credits",
                           "job","num_liable","phone","foreign","outcome")

#Recoding outcome variable: Good customer = 1, Bad Customer = 0
credit_data$outcome[which(credit_data$outcome == 2)] <- 0

#Number of good customers
length(credit_data$outcome[which(credit_data$outcome == 1)])

#Ensuring no missing values and ensuring each column is of the correct type
for(i in 1:ncol(credit_data))
{
  print(paste("Attribute",i))
  print(paste("Factor:",is.factor(credit_data[,i])))
  print(paste("Numeric:",is.numeric(credit_data[,i])))
  print("--------------------------------")
  
}

#Plots for Continuous Variables
#plot.table() used to get proportions for each variable
#table() shows breakdown of data

#Credit Duration
#Median duration for bad customers is higher than for good customers -> seems significant factor visually
boxplot(credit_data$cred_dur~credit_data$outcome, data=credit_data,xlab="Outcome",ylab="Duration of Credit",
        col=c("red","dodgerblue"), main="Outcome and Credit Duration",names=c("Bad","Good"))

#Credit Amount
#Median and interquartile range larger for bad customers
boxplot(credit_data$cred_amt~credit_data$outcome, data=credit_data,xlab="Outcome",ylab="Credit Amount",
        col=c("red","dodgerblue"), main="Outcome and Credit Amount",names=c("Bad","Good"))

#Installment Rate
boxplot(credit_data$inst_rate~credit_data$outcome, data=credit_data,xlab="Outcome",ylab="Installment Rate",
        col=c("red","dodgerblue"), main="Outcome and Installment Rate",names=c("Bad","Good"))

#More suitable to do a bar plot of count given the values of the data
counts.inst_rate <- table(credit_data$outcome,credit_data$inst_rate)
counts.inst_rate
prop.table(counts.inst_rate,2)
barplot(counts.inst_rate, beside=TRUE, col=c("red","dodgerblue"),
        xlab="Installment Rate", ylab="Count",legend.text = c("Bad","Good"),
        main="Installment Rate and Outcome")

#Years of residency
boxplot(credit_data$res_since~credit_data$outcome, data=credit_data,xlab="Outcome",ylab="Years of Residency",
        col=c("red","dodgerblue"), main="Outcome and Years of Residency",names=c("Bad","Good"))

#More suitable to do a bar plot of count given the values of the data
counts.res_since <- table(credit_data$outcome,credit_data$res_since)
counts.res_since
prop.table(counts.res_since,2)
barplot(counts.res_since, beside=TRUE, col=c("red","dodgerblue"),
        xlab="Resident Since Years", ylab="Count",legend.text = c("Bad","Good"),
        main="Resident Since Years and Outcome")

#Age
#Median age higher for good outcomes
boxplot(credit_data$age~credit_data$outcome, data=credit_data,xlab="Outcome",ylab="Age",
        col=c("red","dodgerblue"), main="Outcome and Age",names=c("Bad","Good"))

#Number of existing credits at bank
boxplot(credit_data$num_credits~credit_data$outcome, data=credit_data,xlab="Outcome",ylab="No. of Existing Credits",
        col=c("red","dodgerblue"), main="Outcome and No. of Existing Credits",names=c("Bad","Good"))

counts.num_credits <- table(credit_data$outcome,credit_data$num_credits)
counts.num_credits
prop.table(counts.num_credits,2)
barplot(counts.num_credits, beside=TRUE, col=c("red","dodgerblue"),
        xlab="Number of Existing Credits", ylab="Count",legend.text = c("Bad","Good"),
        main="Number of Existing Credits")


#Number of people liable to provide maintenance for
#Don't recode as there is an adequate number of people in each group
counts.num_liable <- table(credit_data$outcome,credit_data$num_liable)
counts.num_liable
prop.table(counts.num_liable,2)
barplot(counts.num_liable, beside=TRUE, col=c("red","dodgerblue"),
        xlab="Number of people liable to provide maintenance for", ylab="Count",legend.text = c("Bad","Good"),
        main="Number of people liable to provide maintenance for")

#--------------------------------------------------------------------------------------------------------------------------
#Plotting categorical variables
#Current AC Status
#Don't recode as all variables have different proportions of good and bad
counts.ac_status <- table(credit_data$outcome,credit_data$ac_status)
counts.ac_status
prop.table(counts.ac_status,2)
barplot(counts.ac_status, beside=TRUE, col=c("red","dodgerblue"),
        xlab="Current Account Status", ylab="Count",legend.text = c("Bad","Good"),
        main="Current Account Status and Outcome")

#Credit history
counts.cred_hist <- table(credit_data$outcome,credit_data$cred_hist)
counts.cred_hist
prop.table(counts.cred_hist,2)
barplot(counts.cred_hist, beside=TRUE, col=c("red","dodgerblue"),
        xlab="Credit History", ylab="Count",legend.text = c("Bad","Good"),
        main="Credit History and Outcome")

#Recode to make to make three groups
#First group is first three variables combined, this is done due to small amount of datapoints
#for A30 and A31 as well as the fact that all three variables represent either no credits or 
#all credits paid back
credit_data$cred_hist[which(credit_data$cred_hist=="A31")] <- "A30"
credit_data$cred_hist[which(credit_data$cred_hist=="A32")] <- "A30"

#Purpose
counts.purpose <- table(credit_data$outcome,credit_data$purpose)
counts.purpose
prop.table(counts.purpose,2)
barplot(counts.purpose, beside=TRUE, col=c("red","dodgerblue"),
        xlab="Purpose", ylab="Count",legend.text = c("Bad","Good"),
        main="Purpose and Outcome")

#Recode due to small frequency of some variables
#A44, A45, A48 all merged with 410 becoming 'other category'
credit_data$purpose[which(credit_data$purpose=="A44")] <- "A410"
credit_data$purpose[which(credit_data$purpose=="A45")] <- "A410"
credit_data$purpose[which(credit_data$purpose=="A48")] <- "A410"

#Savings AC / Bonds
counts.sav_ac <- table(credit_data$outcome,credit_data$sav_ac)
counts.sav_ac
prop.table(counts.sav_ac,2)
barplot(counts.sav_ac, beside=TRUE, col=c("red","dodgerblue"),
        xlab="Savings Account/Bonds", ylab="Count",legend.text = c("Bad","Good"),
        main="Savings Account/Bonds")

#No variables recoded due to differing proportions of good to bad and
#grouping arbritrarily may cause loss of information

#Present Employment Since
counts.empl_since <- table(credit_data$outcome,credit_data$empl_since)
counts.empl_since
prop.table(counts.empl_since,2)
barplot(counts.empl_since, beside=TRUE, col=c("red","dodgerblue"),
        xlab="Present Employment Since", ylab="Count",legend.text = c("Bad","Good"),
        main="Present Employment Since")

#As the proportion of good to bad for the 4 to 7 and greater than 7 categories
#are similar, there does not appear to be much difference between the two categories
#and so they are merged into the A74 category representing employment greater than
#4 years
credit_data$empl_since[which(credit_data$empl_since=="A75")] <- "A74"

#Personal Status and Sex
counts.status_sex <- table(credit_data$outcome,credit_data$status_sex)
counts.status_sex
prop.table(counts.status_sex,2)
barplot(counts.status_sex, beside=TRUE, col=c("red","dodgerblue"),
        xlab="Personal Status and Sex", ylab="Count",legend.text = c("Bad","Good"),
        main="Personal Status and Sex")


credit_data$status_sex[which(credit_data$status_sex=="A93")] <- "A91"
credit_data$status_sex[which(credit_data$status_sex=="A94")] <- "A91"
#A91, A93 and A94 are merged into A91 to represent a category for all males
#of any status. A92 will represent a category for all females.
#If a single female occurs in the future, it can be put into A92.

#Other debtors/guarantors
counts.other_debtors<- table(credit_data$outcome,credit_data$other_debtors)
counts.other_debtors
prop.table(counts.other_debtors,2)
barplot(counts.other_debtors, beside=TRUE, col=c("red","dodgerblue"),
        xlab="Other Debtors/Guarantors", ylab="Count",legend.text = c("Bad","Good"),
        main="Other Debtors/Guarantors")

#Variables not recoded due to differences in proportion of good to bad

#Property
counts.property<- table(credit_data$outcome,credit_data$property)
counts.property
prop.table(counts.property,2)
barplot(counts.property, beside=TRUE, col=c("red","dodgerblue"),
        xlab="Property", ylab="Count",legend.text = c("Bad","Good"),
        main="Property")

#No values recoded

#Other Installment Plans
counts.other_plans <- table(credit_data$outcome,credit_data$other_plans)
counts.other_plans
prop.table(counts.other_plans,2)
barplot(counts.other_plans, beside=TRUE, col=c("red","dodgerblue"),
        xlab="Other Installment Plans", ylab="Count",legend.text = c("Bad","Good"),
        main="Other Installment Plans")

credit_data$other_plans[which(credit_data$other_plans=="A142")] <- "A141"
#Categories recoded with A141 and A142 merged into A141 due to similar proportions of good to bad and
#small number of observations for A142.
#A141 now represents having an installment plan. 
#A143 represents having no installment plan.

#Housing
counts.housing <- table(credit_data$outcome,credit_data$housing)
counts.housing
prop.table(counts.housing,2)
barplot(counts.housing, beside=TRUE, col=c("red","dodgerblue"),
        xlab="Housing", ylab="Count",legend.text = c("Bad","Good"),
        main="Housing")

credit_data$housing[which(credit_data$housing=="A153")] <- "A151"

#Housing can be recoded with A151 and A153 being merged into A151, representing not
#owning a property (proportions of good to bad also equal). A152 represents owning
#a house.

#Job
counts.job <- table(credit_data$outcome,credit_data$job)
counts.job
prop.table(counts.job,2)
barplot(counts.job, beside=TRUE, col=c("red","dodgerblue"),
        xlab="Job", ylab="Count",legend.text = c("Bad","Good"),
        main="Job")

credit_data$job[which(credit_data$job=="A172")] <- "A171"
credit_data$job[which(credit_data$job=="A174")] <- "A173"

#Job can be recoded with A171 and A172 being merged into A171, with A171
#now representing unskilled or unemployed workers. A173 and A174 can also me merged
#into A173 representing a higher class of job. This should be done due to the 
#similar proportions of good to bad between A173 and A174

#Telephone
counts.phone <- table(credit_data$outcome,credit_data$phone)
counts.phone
prop.table(counts.phone,2)
barplot(counts.phone, beside=TRUE, col=c("red","dodgerblue"),
        xlab="Telephone", ylab="Count",legend.text = c("Bad","Good"),
        main="Telephone")

#Telephone remains the same

#Foreign Worker
counts.foreign <- table(credit_data$outcome,credit_data$foreign)
counts.foreign
prop.table(counts.foreign,2)
barplot(counts.foreign, beside=TRUE, col=c("red","dodgerblue"),
        xlab="Foreign Worker", ylab="Count",legend.text = c("Bad","Good"),
        main="Foreign Worker")

#Foreign worker remains the same

#-------------------------------------------------------------------------------------------------------
#Model Fitting and Evaluation

#Creating training and test sets data using stratified sampling
train_index <- createDataPartition(credit_data$outcome,p=0.8,list=FALSE)
trainingData <- credit_data[train_index,]
testData <- credit_data[-train_index,]

#Function to return misclassification rate, sensitivity and specificity
modelAccuracy <- function(testData, trainFit)
{
  testPredictions = predict(trainFit,type='response',testData)
  
  
  #Create vector for results of test data
  testValues = c()
  
  for(i in 1:nrow(testData)){
    if(testPredictions[i] >= 0.7)
    {
      testValues[i] = 1
    }
    else testValues[i] = 0
  }
  
  rightCount = 0
  wrongCount = 0
  falsePos = 0
  falseNeg = 0
  truePos = 0
  trueNeg = 0
  
  for(i in 1:nrow(testData)){
    
    if(testValues[i] == 1 && testData$outcome[i] == 1)
    {
      rightCount = rightCount + 1
      truePos = truePos + 1
    }
    else if(testValues[i] == 0 && testData$outcome[i] == 0)
    {
      rightCount = rightCount + 1
      trueNeg = trueNeg + 1
    } 
    else if(testValues[i] == 0 && testData$outcome[i] == 1)
    {
      wrongCount = wrongCount + 1
      falseNeg = falseNeg + 1
    } 
    else if(testValues[i] == 1 && testData$outcome[i] == 0)
    {
      wrongCount = wrongCount + 1
      falsePos = falsePos + 1
    } 
  }
  
  falsePosRate <- falsePos/(falsePos+trueNeg)
  missclassificationRate = wrongCount/(rightCount+wrongCount)
  sensitivity <- truePos/(truePos+falseNeg)
  specificity <- trueNeg/(trueNeg+falsePos)
  print(paste("Sensitivity:",round(sensitivity,2)))
  print(paste("Specificity:",round(specificity,2)))
  print(paste("Misclassification:",round(missclassificationRate,2)))
  print(paste("False Positive Rate:",round(falsePosRate,2)))
}

#Fitting model with training data
trainModel1 <- trainingData$outcome ~ .
trainFit1 <- glm(trainModel1,data=trainingData, family=binomial(logit))
summary(trainFit1)
BIC(trainFit1) #989.5
AIC(trainFit1) #816.2
modelAccuracy(testData = testData, trainFit = trainFit1)
#"Sensitivity: 0.86"
#"Specificity: 0.57"
#"Misclassification: 0.22"

#Removing insignificant variables (p < 0.05) from trainModel1
trainModel2 <- trainingData$outcome ~.-empl_since-status_sex-other_debtors-res_since-property-age-housing-job-num_liable-phone
trainFit2 <- glm(trainModel2,data=trainingData,family=binomial(logit))
summary(trainFit2)
BIC(trainFit2)#913.8
AIC(trainFit2)#810.7
modelAccuracy(testData = testData, trainFit = trainFit2)
#"Sensitivity: 0.83" - 83% of actual good customers are correctly identified
#"Specificity: 0.59" - 59% of bad customers are correctly identified
#"Misclassification: 0.24"

#Removing further insignificant variable num_credits
trainModel3 <- trainingData$outcome ~.-empl_since-status_sex-other_debtors-res_since-property-age-housing-job-num_liable-phone-num_credits
trainFit3 <- glm(trainModel3,data=trainingData,family=binomial(logit))
summary(trainFit3)
BIC(trainFit3)#918.36
AIC(trainFit3)#815.30
modelAccuracy(testData = testData, trainFit = trainFit3)
#"Sensitivity: 0.84"
#"Specificity: 0.55"
#"Misclassification: 0.24"

#Model gets worse suggesting num_credits should be included
