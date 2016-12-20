spam <- read.csv("spam.csv")
summary(data)
attach(data)
str(data)
summary(data$V58)

#Creating training and validation set
class1=subset(spam,V58==1) #subset when random sample is not required
class1
class0=subset(spam,V58==0)
class0

ind0 <- sample(1:nrow(class0), floor(.80*nrow(class0)))
ind1 <- sample(1:nrow(spam), floor(.80*nrow(class1)))

train0 <- class0[ind0,]
test0 <- class0[-ind0,]

train1 <- class1[ind1,]
test1 <- class1[-ind1,]

train <- rbind(train0, train1)
test <- rbind(test0, test1)

#First model with all the features
mfit <- glm(V58~., family = binomial(link = "logit"), data = data)
summary(mfit) #AIC = 1931 

#Reducing variables using step function
#step(mfit)

plot(mfit)

mfit1 <- glm(formula = V58 ~ V1 + V2 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + 
               V12 + V15 + V16 + V17 + V19 + V20 + V21 + V22 + V23 + V24 + 
               V25 + V26 + V27 + V28 + V29 + V33 + V35 + V36 + V38 + V39 + 
               V41 + V42 + V43 + V44 + V45 + V46 + V47 + V48 + V49 + V52 + 
               V53 + V54 + V56 + V57, family = binomial(link = "logit"), 
             data = data)
summary(mfit1) #AIC = 1912
plot(mfit1)
summary(spam) #Checking for variables that need to be scaled

#Found 3 variables with huge values, used log function on then
mfit2 <- glm(formula = V58 ~ V1 + V2 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + 
               V12 + V15 + V16 + V17 + V19 + V20 + V21 + V22 + V23 + V24 + 
               V25 + V26 + V27 + V28 + V29 + V33 + V35 + V36 + V38 + V39 + 
               V41 + V42 + V43 + V44 + V45 + V46 + V47 + V48 + V49 + V52 + 
               V53 + V54 + log(V55) + log(V56) + log(V57), family = binomial(link = "logit"), 
             data = data)
summary(mfit2) #AIC = 1734
plot(mfit2)

#Prediction output using the second model
out <- predict(mfit2, test, type = "response")
out <- ifelse(out>0.5,1,0)

#Finding RMSE 
sqerror <- (out-test$V58)^2
rmse <- sqrt(mean(sqerror))
rmse #=0.257

#This loop will tell us the count of correct predictions and give us the accuracy
correct <- 0
total <- 0
for(i in 1:nrow(test)){
  if(out[i]==test[i,58]){
    correct = correct + 1
  }
  total =  correct/nrow(test)
}
correct
total #93%

#Another way to find the accuracy
accuracy = correct/nrow(test)
accuracy

#Checking with the confusion matrix 
library(caret)
library(e1071)
confusionMatrix(out, test$V58)
