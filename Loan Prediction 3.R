train1 <- read.csv("C:/Users/Siddhesh Pisal/Documents/Loan Prediction 3/train_u6lujuX_CVtuZ9i.csv", na.strings = "")
test1 <- read.csv("C:/Users/Siddhesh Pisal/Documents/Loan Prediction 3/test_Y3wMUE5_7gLdaTN.csv", na.strings = "")
test1$Loan_Status <- rep("N", nrow(test))
data1 <- rbind(train1,test1)
data <- data1[-1]
sum(is.na(data))
#attach(data)

library(rpart)
library(car)
library(caret)
library(randomForest)
library(Metrics)

str(data)
summary(data)

#To use the recode function we use the car package

names(data)

#All these columns have NA values. 
sum(is.na(data$Gender))
sum(is.na(data$Married))
sum(is.na(data$Dependents))
sum(is.na(data$Self_Employed))
sum(is.na(data$LoanAmount))
sum(is.na(data$Loan_Amount_Term))
sum(is.na(data$Credit_History))

#Imputing replacements for NA values in Gender
#We will use decision tree classifier
set.seed(123)
gentrain <- data[!is.na(data$Gender),]
gentest <- data[is.na(data$Gender),]

genmodel <- rpart(Gender ~., gentrain)
genpred_t <- predict(genmodel, gentrain, type = "class")
confusionMatrix(genpred_t, gentrain$Gender)

genpred <- predict(genmodel, gentest, type = "class")

data$Gender[is.na(data$Gender)] <- genpred

##Imputing replacements for NA values in Married column

prop.table(table(data$Married))

#if co-applicant income is 0 we can assume that they're not married
data$Married[is.na(data$Married) & data$CoapplicantIncome == 0] <- "No"
which(is.na(data$Married))
#Since co-applicant income is given, we can assume the person is married
data$Married[105] <- "Yes"



#Imputing replacements for NA values in Dependents column
sum(is.na(data$Dependents))

data$Dependents[is.na(data$Dependents) & data$Married == "No"] <- 0
deptrain <- data[!is.na(data$Dependents),]
deptest <- data[is.na(data$Dependents),]

depmodel <- rpart(Dependents ~., deptrain, control = rpart.control(minsplit = 20, cp = 0.005))
deppred_t <- predict(depmodel, deptrain, type = "class")
confusionMatrix(deppred_t, deptrain$Dependents)

deppred <- predict(depmodel, deptest, type = "class")

data$Dependents[is.na(data$Dependents)] <- deppred

data$Dependents <- ifelse(data$Dependents == "3+",3,data$Dependents)


#Imputing replacements for NA values in Self_Employed
#We will use the mode
plot(data$Self_Employed)
data$Self_Employed[is.na(data$Self_Employed)] <- "No"


#NA replacement in LoanAmountTerm

plot(data$Loan_Amount_Term)
hist(data$Loan_Amount_Term, 5)

data$Loan_Amount_Term <- ifelse(data$Loan_Amount_Term <= 60, 60, data$Loan_Amount_Term)
data$Loan_Amount_Term[is.na(data$Loan_Amount_Term)] <- 360


#NA replacement in Credit History
summary(data$Credit_History)
str(data$Credit_History)
data$Credit_History<-recode(data$Credit_History,"NA=2")

#NA replacement in LoanAmount
#We will have to use a model

summary(data$LoanAmount)
plot(data$LoanAmount, data$Loan_Amount_Term)
boxplot(data$LoanAmount)

loantrain <- data[!is.na(data$LoanAmount) & data$Loan_Amount_Term <= 500,]
loantest <- data[is.na(data$LoanAmount),]

loanmod <- lm(formula = LoanAmount ~ Married + Dependents + Education + 
                Self_Employed + ApplicantIncome + CoapplicantIncome + Loan_Amount_Term, 
              data = loantrain, na.action = na.exclude)

loanpred <- predict(loanmod, loantrain)
rmse(loanpred, loantrain$LoanAmount)

loantpred <- predict(loanmod, loantest)
data$LoanAmount[is.na(data$LoanAmount)] <- round(loantpred)
  
#All NA values have been resolved. Let us separate the train and test data again

train <- data[1:614,]
test <- data[615:981,]

tscopy <- test
#Decision Tree

fit <- rpart(Loan_Status~., data = train, control = rpart.control(minsplit = 20, cp = 0.009))
prp(fit)


DTpred <- predict(fit, type = "class", data = train)
confusionMatrix(DTpred, train$Loan_Status)

dtpred1 <- predict(fit, test, type="class")

test1$Loan_Status <- dtpred1

finalpred <- test1[,c("Loan_ID", "Loan_Status")]

write.csv(finalpred, "loanpred.csv", row.names = F)


#Recoding all variables to numbers

alldata <- data
str(alldata)

alldata$Gender <- ifelse(alldata$Gender == "Female", 1,0)
alldata$Married <- ifelse(alldata$Married == "Yes", 1,0)
alldata$Education <- ifelse(alldata$Education == "Graduate", 1,0)
alldata$Self_Employed <- ifelse(alldata$Self_Employed == "Yes", 1,0)
alldata$Loan_Status <- ifelse(alldata$Loan_Status == "Y", 1,0)
alldata$Property_Area <- as.character(alldata$Property_Area)
alldata$Property_Area[alldata$Property_Area == "Rural"] <- 0
alldata$Property_Area[alldata$Property_Area == "Semiurban"] <- 1
alldata$Property_Area[alldata$Property_Area == "Urban"] <- 2
alldata$Property_Area <- as.numeric(alldata$Property_Area)

alldata$TotalIncome <- alldata$ApplicantIncome + alldata$CoapplicantIncome
alldata$Lalam <- alldata$LoanAmount/alldata$Loan_Amount_Term
str(alldata)
#Minmax function

minmax <- function(x){
  (x-min(x))/(max(x)-min(x))
}

alldata$ApplicantIncome <- minmax(alldata$ApplicantIncome)
alldata$CoapplicantIncome <- minmax(alldata$CoapplicantIncome)
alldata$Loan_Amount_Term <- minmax(alldata$Loan_Amount_Term)
alldata$LoanAmount <- minmax(alldata$LoanAmount)

xgtrain <- alldata[1:614,]
xgtest <- alldata[615:981,]

library(rpart.plot)
library(xgboost)

xgb <- xgboost(data = data.matrix(xgtrain[,-12]), 
               label = xgtrain[,12], 
               eta = 0.1,
               max_depth = 15, 
               nround=25, 
               subsample = 0.5,
               colsample_bytree = 0.5,
               seed = 1,
               eval_metric = "merror",
               objective = "multi:softmax",
               num_class = 12,
               nthread = 3
)

xgpredtr <- predict(xgb, data.matrix(xgtrain))

confusionMatrix(xgpredtr, xgtrain$Loan_Status)












#RandomForest1

fit2 <- randomForest(Loan_Status ~., train, ntree = 500, mtry = 3, type = "classification")

plot(fit2$importance)
fit2$importance
plot(fit2, type = "l")

rfpred <- predict(fit2, train,  type = "response")
rfpred
confusionMatrix(rfpred, train$Loan_Status)

rftest <- predict(fit2, test, type = "response")
rftest


test1$Loan_Status <- rftest
finalpred <- test1[,c("Loan_ID", "Loan_Status")]

write.csv(finalpred, "loanpred.csv", row.names = F)

#RandomForest2

rftrain <- train
rftest <- test

rfdata <- rbind(rftrain, rftest)

rfdata$TotalIncome <- rfdata$ApplicantIncome + rfdata$CoapplicantIncome
rfdata$Lalam <- rfdata$LoanAmount/rfdata$Loan_Amount_Term


rfdata$ApplicantIncome <- minmax(rfdata$ApplicantIncome)
rfdata$CoapplicantIncome <- minmax(rfdata$CoapplicantIncome)
rfdata$LoanAmount <- minmax(rfdata$LoanAmount)
rfdata$TotalIncome <- minmax(rfdata$TotalIncome)
str(rfdata)


rftrain <- rfdata[1:614,]
rftest <- rfdata[615:981,]

fit3 <- randomForest(Loan_Status ~., rftrain, ntree = 600, mtry = 4, type = "classification")
print(fit3)
fit3$importance
plot(fit3, type = "l")

rfpred <- predict(fit3, rftrain,  type = "response")
rfpred
confusionMatrix(rfpred, rftrain$Loan_Status)

rftestpred <- predict(fit3, rftest, type = "response")
rftestpred

test1$Loan_Status <- rftestpred
finalpred <- test1[,c("Loan_ID", "Loan_Status")]

write.csv(finalpred, "loanpred.csv", row.names = F)


#Trial forest another

fit4 <- randomForest(Loan_Status ~., rftrain, ntree = 600, mtry = 3, type = "classification")
rfmtry <- tuneRF(rftrain[,-12], rftrain$Loan_Status, ntreeTry=500, 
               stepFactor=1.5, improve = 0.01, trace=TRUE, plot=TRUE, doBest = T)

best.m <- rfmtry[rfmtry[, 2] == min(rfmtry[, 2]), 1]



rfpred <- predict(fit4, rftrain,  type = "response")
rfpred
confusionMatrix(rfpred, rftrain$Loan_Status)

rftestpred <- predict(fit4, rftest, type = "response")
rftestpred

test1$Loan_Status <- rftestpred
finalpred <- test1[,c("Loan_ID", "Loan_Status")]

