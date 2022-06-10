library(readxl)

airline=read_excel('Airline_Satisfaction.xlsx')
attach(airline)
View(airline) #View data structure
str(airline)

# Data Pre-processing
airline[1]<- NULL #Remove first id column as it is not correlated with other variables  
sum(is.na(airline)) #Detect if missing values exist

library(tidyr)
airline=airline %>% drop_na() #Delete miss
head(airline)

airline$RATE <- as.numeric(ifelse(airline$satisfaction_v2=='satisfied',1,0)) #Create a new variable based on satisfaction
library("ggcorrplot")
#correlation between numerical variables only
ggcorrplot(cor(airline[, unlist(lapply(airline, is.numeric))]),tl.cex = 7,type = "lower",lab = TRUE, lab_size=1.5) 

library(plyr)
count(airline, "satisfaction_v2")
count(airline, "Gender")

#Research Question 6
table(airline$`Seat comfort`,airline$`Gender`)
table(airline$`Departure/Arrival time convenient`,airline$`Gender`)
table(airline$`Food and drink`,airline$`Gender`)
table(airline$`Gate location`,airline$`Gender`)
table(airline$`Inflight wifi service`,airline$`Gender`)
table(airline$`Inflight entertainment`,airline$`Gender`)
table(airline$`Online support`,airline$`Gender`)
table(airline$`Ease of Online booking`,airline$`Gender`)
table(airline$`On-board service`,airline$`Gender`)
table(airline$`Leg room service`,airline$`Gender`)
table(airline$`Baggage handling`,airline$`Gender`)
table(airline$`Checkin service`,airline$`Gender`)
table(airline$Cleanliness,airline$`Gender`)
table(airline$`Online boarding`,airline$`Gender`)

corr(airline$`Inflight wifi service`,airline$`Inflight entertainment`)


str(airline)
library(caret)
names(airline) <- make.names(names(airline), unique=TRUE)

#q2
print(table(airline$Gender,airline$Class))
print(table(airline$Type.of.Travel,airline$Class))

#q3
boxplot(airline$Flight.Distance~airline$RATE)

#q4
boxplot(airline$Seat.comfort~airline$Type.of.Travel)
boxplot(airline$Checkin.service~airline$Type.of.Travel)
boxplot(airline$Baggage.handling~airline$Type.of.Travel)

#q5
airline$RATE <- as.numeric(ifelse(airline$satisfaction_v2=='satisfied',1,0)) #Create a new variable based on satisfaction
library("ggcorrplot")
ggcorrplot(cor(airline[, unlist(lapply(airline, is.numeric))]),tl.cex = 6,type = "lower",lab = TRUE, lab_size=1.5)

#q6
library('fastDummies')
airline <- dummy_cols(airline, select_columns = c('Gender','Customer.Type','Type.of.Travel','Class'))
str(airline)
names(airline) <- make.names(names(airline), unique=TRUE)

ggcorrplot(cor(airline[, unlist(lapply(airline, is.numeric))]),tl.cex = 5,type = "lower",lab = TRUE, lab_size=1.2) 
#ggcorrplot(cor(airline[, unlist(lapply(airline, is.numeric))]),tl.cex = 7,type = "lower") 


#logistic regression
airline$RATE<- factor(airline$RATE, levels=c(0,1))
set.seed(12345)
inTrain <- sample(nrow(airline),0.7*nrow(airline))
traindata <- airline[inTrain,]
testdata <- airline[-inTrain,]
str(airline)
#corr. coefficient>=0.2
model_0.2<- glm(RATE ~ Online.boarding+Cleanliness+Checkin.service+Baggage.handling+Leg.room.service+On.board.service+Ease.of.Online.booking+Online.support+Inflight.entertainment+Inflight.wifi.service+Seat.comfort+Class_Eco+Class_Business+Customer.Type_Loyal.Customer+Gender_Male, data=traindata, family="binomial")
summary(model_0.2)
Actual <- testdata$RATE
predicted.probability <- predict(model_0.2, type = "response", newdata=testdata[,-1]) 
## Note the predictions are probabilities
cutoff <- 0.5
Predicted <- ifelse(predicted.probability > cutoff, 1,0)
(confusion <- table(Actual,Predicted))
(acc_0.2=(confusion[1,1]+confusion[2,2])/sum(confusion)) # 0.827 accuracy rate for logistic model 
(SEN_glm = confusion[1,1]/(confusion[1,1]+confusion[1,2])) # 0.804 sensitivity rate 
(SPE_glm = confusion[2,2]/(confusion[2,1]+confusion[2,2])) #0.847 specificity rate 

#corr. coefficient>=0.25
model_0.25<- glm(RATE ~ Online.boarding+Cleanliness+Checkin.service+Baggage.handling+Leg.room.service+On.board.service+Ease.of.Online.booking+Online.support+Inflight.entertainment+Class_Eco+Class_Business+Customer.Type_Loyal.Customer, data=traindata, family="binomial")
summary(model_0.25)
Actual <- testdata$RATE
predicted.probability <- predict(model_0.25, type = "response", newdata=testdata[,-1]) 
## Note the predictions are probabilities
cutoff <- 0.5
Predicted <- ifelse(predicted.probability > cutoff, 1,0)
(confusion <- table(Actual,Predicted))
(acc_0.25=(confusion[1,1]+confusion[2,2])/sum(confusion))

#corr. coefficient>=0.3
model_0.3<- glm(RATE ~ Online.boarding+Leg.room.service+On.board.service+Ease.of.Online.booking+Online.support+Inflight.entertainment+Class_Business, data=traindata, family="binomial")
summary(model_0.3)
Actual <- testdata$RATE
predicted.probability <- predict(model_0.3, type = "response", newdata=testdata[,-1]) 
cutoff <- 0.5
Predicted <- ifelse(predicted.probability > cutoff, 1,0)
(confusion <- table(Actual,Predicted))
(acc_0.3=(confusion[1,1]+confusion[2,2])/sum(confusion))

#printing accuracies for all the 3 models
print(c(acc_0.2, acc_0.25, acc_0.3))

#using model_0.2
#Final feature selection
attach(airline)
airline2<-data.frame(RATE,Online.boarding,Cleanliness,Checkin.service,Baggage.handling,Leg.room.service,
                     On.board.service,Ease.of.Online.booking,Online.support,Inflight.entertainment,
                     Inflight.wifi.service,Seat.comfort,Class_Eco,Class_Business,Customer.Type_Loyal.Customer,
                     Gender_Male)
airline2$RATE = as.numeric(airline2$RATE)-1 

#Data partition
set.seed(12345)
inTrain <- sample(nrow(airline2),0.7*nrow(airline2))
traindata <- airline2[inTrain,]
testdata <- airline2[-inTrain,]

### KNN 
# Now we need to normalize each variable
fun <- function(x){ 
  a <- mean(x) 
  b <- sd(x) 
  (x - a)/(b) 
} 
airline2.copy = airline2
airline2.copy[,2:16] <- apply(airline2.copy[,2:16], 2, fun)

library("caret")

set.seed(12345)
inTrain <- sample(nrow(airline2.copy),0.7*nrow(airline2.copy))
traindata_knn <- airline2.copy[inTrain,]
testdata_knn <- airline2.copy[-inTrain,]


library(class)

train_input <- as.matrix(traindata_knn[,-1])
train_output <- as.vector(traindata_knn[,1])
test_input <- as.matrix(testdata_knn[,-1])

kmax <- 10
ER1 <- rep(0,kmax)
ER2 <- rep(0,kmax)
#
for (i in 1:kmax){
  prediction <- knn(train_input, train_input,train_output, k=i)
  prediction2 <- knn(train_input, test_input,train_output, k=i)
  
  # The confusion matrix for training data is:
  CM1 <- table(prediction, traindata_knn$RATE)
  # The training error rate is:
  ER1[i] <- (CM1[1,2]+CM1[2,1])/sum(CM1)
  # The confusion matrix for test data is: 
  CM2 <- table(prediction2, testdata_knn$RATE)
  ER2[i] <- (CM2[1,2]+CM2[2,1])/sum(CM2)}


plot(c(1,kmax),c(0,0.1),type="n", xlab="k",ylab="Error Rate")
lines(ER1,col="red")
lines(ER2,col="blue")
legend(7, 0.1, c("Training","Validation"),lty=c(1,1), col=c("red","blue"))
z <- which.min(ER2)
cat("Minimum Validation Error k:", z)

prediction2 <- knn(train_input, test_input,train_output, k=z)
confusionTest <- table(prediction2, testdata_knn$RATE)
(SensitivityTest <- confusionTest[2,2]/sum(confusionTest[2,])) # 0.93 sensitivity 
(SpecificityTest <- confusionTest[1,1]/sum(confusionTest[1,])) # 0.88 specificity 
(CONF = table(Actual = testdata$RATE, Prediction2 = prediction2))
(ACC = (CONF[1,1]+CONF[2,2])/sum(CONF)) # 0.906 accuracy for KNN

### Naive Bayes:
set.seed(1)
library(e1071)
model <- naiveBayes(RATE~., data=traindata)
model
prediction <- predict(model, newdata = testdata[,-1])
(conf = table(testdata$RATE,prediction,dnn=list('actual','predicted')))
model$apriori
(Accuracy <- sum(diag(conf))/sum(conf)) # 0.82 accuracy rate for Naive Bayes model 
(SEN_NB = conf[1,1]/(conf[1,1]+conf[1,2])) # 0.806 sensitivity
(SEN_NB = conf[2,2]/(conf[2,1]+conf[2,2])) # 0.828 specificity

predicted.probability <- predict(model, newdata = testdata[,-1], type="raw")

PL <- as.numeric(testdata$RATE)-1
prob <- predicted.probability[,2]
df1 <- data.frame(prediction, PL, prob)
df1S <- df1[order(-prob),]
df1S$Gains <- cumsum(df1S$PL)
plot(df1S$Gains,type="n",main="Lift Chart",xlab="Number of Cases",ylab="Cumulative Success") #Plot lift chart 
lines(df1S$Gains,col="blue")
abline(0,sum(df1S$PL)/nrow(df1S),lty = 2, col="red")



### Classification tree
library(tree)
set.seed(1)
traindata2 =  traindata
traindata2$RATE = as.factor(traindata2$RATE)
testdata2 = testdata
testdata2$RATE = as.factor(testdata2$RATE)
tree.air = tree(RATE~.,data = traindata2)
#Plot the unpruned tree
plot(tree.air)
text(tree.air,pretty = 0)
# Using rpart and rpart.plot packages to plot 
library(rpart)
library(rpart.plot)
sss <- rpart(RATE~., method = "class",data=traindata)
sss
install.packages('rattle')
library(rattle)
fancyRpartPlot(sss,sub = "Classification Tree",cex = 0.6) 
summary(tree.air) # Training error rate of the unpruned tree is 0.1112
#Predict with unpruned tree
tree.air=tree(RATE~., data=traindata2)
tree.pred=predict(tree.air,newdata = testdata2,type="class")
(CM_t=table(Actual=testdata2$RATE, Prediction=tree.pred))
(ACC = sum(diag(CM_t))/sum(CM_t)) #0.86 accuracy rate for unpruned tree
#Prune the tree
set.seed(1)
cv.air=cv.tree(tree.air,FUN=prune.misclass)
plot(cv.air$size,cv.air$dev,type="b") # best size  = 7 
# Now prune the tree to a best size of 7
prune.air=prune.misclass(tree.air,best=7)
plot(prune.air)
text(prune.air,pretty=0)
#Predict with pruned tree
prune.pred.air=predict(prune.air,testdata2,type="class")
(CM_t = table(Actual = testdata2$RATE, Prediction = prune.pred.air))
(Acc_t = (CM_t[1,1]+CM_t[2,2])/sum(CM_t)) #0.86 accuracy rate for the pruned tree
(SEN_tree = CM_t[1,1]/(CM_t[1,1]+CM_t[1,2])) #0.734 sensitivity 
(SPE_tree = CM_t[2,2]/(CM_t[2,1]+CM_t[2,2])) #0.964 specificity 



### Boosting 
library(gbm)
set.seed(1)
boost.air = gbm(RATE~., 
                data = traindata, 
                distribution="bernoulli",
                n.trees=1000, 
                interaction.depth=4)
summary(boost.air)
par(mfrow=c(1,2))
plot(boost.air,i="Inflight.entertainment")
plot(boost.air,i="Seat.comfort")
boost.predict = predict(boost.air, 
                        newdata = testdata, 
                        n.trees = 1000, 
                        type = 'response')
prediction = ifelse(boost.predict>0.5, 1, 0)
(CONF = table(Actual = testdata$RATE, Prediction = prediction))
(ACC = (CONF[1,1]+CONF[2,2])/sum(CONF)) #0.934 accuracy rate for boosting model 
(SEN_Boost = CONF[1,1]/(CONF[1,1]+CONF[2,1])) # 0.926 sensitivity 
(SPE_Boost = CONF[2,2]/(CONF[2,1]+CONF[2,2])) # 0.938 sensitivity 



### XGBoost
library(xgboost)
set.seed(1)
xgb.traindata = as.matrix(traindata[,2:16])
xgb.label = traindata$RATE
xgb.air = xgboost(data = xgb.traindata, 
                  label = xgb.label, 
                  max.depth = 2, 
                  eta = 1, 
                  nround = 5, 
                  objective = "binary:logistic") 

xgb.labelT = testdata$RATE
xgb.testdata = as.matrix(testdata[,2:16])
xgb.pred = predict(xgb.air, xgb.testdata)
prediction = ifelse(xgb.pred>0.5, 1, 0)
(CONF_xgb = table(xgb.labelT,prediction))
(acc_xgb = (CONF_xgb[1,1]+CONF_xgb[2,2])/sum(CONF_xgb)) # 0.885 accuracy rate for XGBoost
(SEN_xgb = CONF_xgb[1,1]/(CONF_xgb[1,1]+CONF_xgb[2,1])) # 0.86 sensitivity rate
(SPE_xgb = CONF_xgb[2,2]/(CONF_xgb[2,1]+CONF_xgb[2,2])) # 0.883 specificity 








