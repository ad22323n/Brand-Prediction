library(caret)
library(lattice)
library(ggplot2)
library(devtools)
library(mlbench)
library(rpart.plot)
library(rpart)

complete<-read.csv("file:///C:/Users/User/Desktop/Ubiqum/Task3/Survey_Key_and_Complete_Responses_excel.csv")
incomplete<-read.csv("file:///C:/Users/User/Desktop/Ubiqum/Task3/SurveyIncomplete.csv")

summary(complete)
str(complete)
summary(incomplete)
str(incomplete)

#let's change our data
complete$elevel<-as.factor(complete$elevel)
complete$car<-as.factor(complete$car)
complete$zipcode<-as.factor(complete$zipcode)
complete$brand<-as.factor(complete$brand)

incomplete$elevel<-as.factor(incomplete$elevel)
incomplete$car<-as.factor(incomplete$car)
incomplete$zipcode<-as.factor(incomplete$zipcode)
incomplete$brand<-as.factor(incomplete$brand)

#setting our seed and training/testing set
intrain<- createDataPartition(complete$brand, p=0.80, list = FALSE)

training<-complete[intrain,]
testing<-complete[-intrain,]

nrow(training)
nrow(testing)

#10 fold cross validation
set.seed(2017)
ctrl<- trainControl(method="repeatedcv", number=10, repeats = 2)

knnFit<- train(brand~., data = training[,c(1,2,7)], method="knn",trControl=ctrl, preProcess=c("center","scale"))
print(knnFit)

predicknn<-predict(knnFit, testing)
postResample(predicknn,testing$brand)

dfFit<- train(brand~., data=training[,c(1,2,7)], method="rf", trControl=ctrl, preProcess=c("center","scale"))
print(dfFit)

predictFit<-predict(dfFit, testing)
postResample(predictFit, testing$brand)

##########################Checking the weght of attributes===========
myname<-rpart(brand~., data = training)
rpart.plot(myname)


#Inserting new prediction into the incomplete data
predicknn<-predict(knnFit, incomplete[c(1,2,7)])
incomplete$NewB<-predicknn
View(incomplete)

ggplot(data = complete, aes(age,fill=brand))+geom_bar()
prop.table(table(complete$brand))

ggplot(data = incomplete, aes(salary,age, colour=NewB))+geom_jitter()


#EXporting prediction in excel
output<-incomplete
write.csv(output, file="c4.T3output.csv", row.names = TRUE)




