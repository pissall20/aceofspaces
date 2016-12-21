setwd("~")

bcdat <- read.csv("wisc_bc_data.csv")
attach(bcdat)
summary(bcdat)
head(bcdat)

str(bcdat)
#Removing the ID column which is not needed
bcdata <- bcdat[,-1]

head(bcdata)

#CHanging our class vector to factors
#diagnosis <- factor(bcdata$diagnosis, levels = c("B","M"), labels = c("Benign","Malignant"))

#Finding the propoertion of benign and malignant tumors
table(bcdata$diagnosis)
round(prop.table(table(bcdata$diagnosis))*100, digits = 1)
namevec <- bcdata[1,]

function(x){
  return((x - min(x))/(max(x)-min(x)))
}

summary(bcdata[c("radius_mean", "area_mean", "smoothness_mean")])

#Checking if the minmax function we made is working

minmax(c(1,2,3,4,5))
minmax(c(10,20,30,40,50))

#Apply the minmax function to all the columns in our data

datanor <- as.data.frame(lapply(bcdata[2:31],minmax))

#Split the normalized dataframe into training and test 

datanor_train <- datanor[1:469,]
datanor_test <- datanor[470:569,]

#Create a vector containing class labels for the train and test data
train_labels <- bcdata[1:469,1]
test_labels <- bcdata[470:569,1]

install.packages(class)
library(class)

#Using the K-nearest neigbor algorithm to diagnose cancer
test_pred <- knn(datanor_train, datanor_test, train_labels, k = 21)

#The output should be in factors
print(bc_test_pred)

library(gmodels)
#CrossTable creates a confusion matrix for the prediction
CrossTable(test_labels, test_pred, prop.chisq = F)
