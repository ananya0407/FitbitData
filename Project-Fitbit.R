---
  title: "Project"
author: "Ananya Naresh and Loshini Lavanya Krishnakumar"
date: "4/15/2020"
output:
  html_document: default
pdf_document: default
---

library(corrplot)
library(dplyr)
library(Hmisc)
library(data.table)
library(FactoMineR)
library(class)
library(e1071)
library(psych)
library(viridis)
library(Metrics)
library(neuralnet)
library(lubridate)
library(GPArotation)
library(scales)
library(GGally)
library(car)
library(caret)
library(dummies)
library(ModelMetrics)
library(rpart)


FitBitData <- read.csv("~/Desktop/data mining/FitBitData.csv", na.strings="", stringsAsFactors=FALSE)
#Data Cleaning, Exploration and Visualisation

#Converting to numeric
FitBitData$FitBit.minutesToFallAsleep=factor(FitBitData$FitBit.minutesToFallAsleep)
FitBitData$FitBit.minutesToFallAsleep=as.numeric(gsub(",","",FitBitData$FitBit.minutesToFallAsleep))

FitBitData$FitBit.levels.summary.deep.thirtyDayAvgMinutes=factor(FitBitData$FitBit.levels.summary.deep.thirtyDayAvgMinutes)
FitBitData$FitBit.levels.summary.deep.thirtyDayAvgMinutes=as.numeric(gsub(",","",FitBitData$FitBit.levels.summary.deep.thirtyDayAvgMinutes))

FitBitData$FitBit.levels.summary.wake.thirtyDayAvgMinutes=factor(FitBitData$FitBit.levels.summary.wake.thirtyDayAvgMinutes)
FitBitData$FitBit.levels.summary.wake.thirtyDayAvgMinutes=as.numeric(gsub(",","",FitBitData$FitBit.levels.summary.wake.thirtyDayAvgMinutes))

FitBitData$FitBit.levels.summary.light.thirtyDayAvgMinutes=factor(FitBitData$FitBit.levels.summary.light.thirtyDayAvgMinutes)
FitBitData$FitBit.levels.summary.light.thirtyDayAvgMinutes=as.numeric(gsub(",","",FitBitData$FitBit.levels.summary.light.thirtyDayAvgMinutes))

FitBitData$FitBit.levels.summary.rem.thirtyDayAvgMinutes=factor(FitBitData$FitBit.levels.summary.rem.thirtyDayAvgMinutes)
FitBitData$FitBit.levels.summary.rem.thirtyDayAvgMinutes=as.numeric(gsub(",","",FitBitData$FitBit.levels.summary.rem.thirtyDayAvgMinutes))


FitBitData$Level1Seconds=factor(FitBitData$Level1Seconds)
FitBitData$Level1Seconds=as.numeric(gsub(",","",FitBitData$Level1Seconds))

FitBitData$Level2Seconds=factor(FitBitData$Level2Seconds)
FitBitData$Level2Seconds=as.numeric(gsub(",","",FitBitData$Level2Seconds))

FitBitData$Level3Seconds=factor(FitBitData$Level3Seconds)
FitBitData$Level3Seconds=as.numeric(gsub(",","",FitBitData$Level3Seconds))

FitBitData$Level4Seconds=factor(FitBitData$Level4Seconds)
FitBitData$Level4Seconds=as.numeric(gsub(",","",FitBitData$Level4Seconds))

FitBitData$Level5Seconds=factor(FitBitData$Level5Seconds)
FitBitData$Level5Seconds=as.numeric(gsub(",","",FitBitData$Level5Seconds))

FitBitData$Level6Seconds=factor(FitBitData$Level6Seconds)
FitBitData$Level6Seconds=as.numeric(gsub(",","",FitBitData$Level6Seconds))

FitBitData$FitBit.duration=factor(FitBitData$FitBit.duration)
FitBitData$FitBit.duration=as.numeric(gsub(",","",FitBitData$FitBit.duration))

FitBitData$FitBit.minutesAsleep=factor(FitBitData$FitBit.minutesAsleep)
FitBitData$FitBit.minutesAsleep=as.numeric(gsub(",","",FitBitData$FitBit.minutesAsleep))

FitBitData$FitBit.minutesAwake=factor(FitBitData$FitBit.minutesAwake)
FitBitData$FitBit.minutesAwake=as.numeric(gsub(",","",FitBitData$FitBit.minutesAwake))

FitBitData$FitBit.timeInBed=factor(FitBitData$FitBit.timeInBed)
FitBitData$FitBit.timeInBed=as.numeric(gsub(",","",FitBitData$FitBit.timeInBed))

FitBitData$FitBit.levels.summary.deep.minutes=factor(FitBitData$FitBit.levels.summary.deep.minutes)
FitBitData$FitBit.levels.summary.deep.minutes=as.numeric(gsub(",","",FitBitData$FitBit.levels.summary.deep.minutes))

FitBitData$FitBit.levels.summary.wake.minutes=factor(FitBitData$FitBit.levels.summary.wake.minutes)
FitBitData$FitBit.levels.summary.wake.minutes=as.numeric(gsub(",","",FitBitData$FitBit.levels.summary.wake.minutes))


FitBitData$FitBit.levels.summary.light.minutes=factor(FitBitData$FitBit.levels.summary.light.minutes)
FitBitData$FitBit.levels.summary.light.minutes=as.numeric(gsub(",","",FitBitData$FitBit.levels.summary.light.minutes))

FitBitData$FitBit.levels.summary.rem.minutes=factor(FitBitData$FitBit.levels.summary.rem.minutes)
FitBitData$FitBit.levels.summary.rem.minutes=as.numeric(gsub(",","",FitBitData$FitBit.levels.summary.rem.minutes))

FitBitData$FitBit.daily_mean_BPM=factor(FitBitData$FitBit.daily_mean_BPM)
FitBitData$FitBit.daily_mean_BPM=as.numeric(gsub(",","",FitBitData$FitBit.daily_mean_BPM))

FitBitData$FitBit.Steps=factor(FitBitData$FitBit.Steps)
FitBitData$FitBit.Steps=as.numeric(gsub(",","",FitBitData$FitBit.Steps))

FitBitData$Sleep.Score=factor(FitBitData$Sleep.Score)
FitBitData$Sleep.Score=as.numeric(gsub(",","",FitBitData$Sleep.Score))

#Plot 1
ReqViz<-as.data.frame(FitBitData$FitBit.daily_mean_BPM)
ReqViz$SleepScoree<-FitBitData$Sleep.Score
colnames(ReqViz)<-c("BPM","SS")
ReqViz$steps<-factor(FitBitData$FitBit.Steps)
ReqViz$steps=as.numeric(gsub(",","",FitBitData$FitBit.Steps))
ReqViz

plot(SS ~ BPM, data = ReqViz, col = "red")
with(ReqViz, lines(loess.smooth(BPM, SS), col = "blue"))

BS_fit<-lm(SS~BPM,data=ReqViz)
print(BS_fit)
summary(BS_fit)

fit=FitBitData
selection1=select(fit,X,FitBit.minutesToFallAsleep,FitBit.minutesAsleep,FitBit.minutesAwake,FitBit.timeInBed,FitBit.daily_mean_BPM,FitBit.Steps,Sleep.Score)

#Plot 2
names(selection1)[1]="Fall_Asleep_Time(Minutes)"
names(selection1)[2]="Asleep_Time(Minutes)"
names(selection1)[3]="Awake_Time(Minutes)"
names(selection1)[4]="Time_In_Bed(Minutes)"
names(selection1)[5]="Heart_Rate"
names(selection1)[6]="Steps_Walked"
names(selection1)[7]="Sleep_Score"

omittedvalue=na.omit(selection1)
x=omittedvalue$`Asleep_Time(Minutes)`
y=omittedvalue$Steps_Walked
z=omittedvalue$Sleep_Score
corr1=cor(x,y)
corr2=cor(y,z)
cor(omittedvalue)
matrix1=as.matrix(omittedvalue)
rcor=rcorr(matrix1)
corrplot(cor(matrix1),tl.col="black")

#Plot 3
dup=fit
names(dup)[2]="Date"
names(dup)[45]="Steps"
dup$Date=week(dup$Date)
select2=select(dup,Date,Steps)
select2$Steps=factor(select2$Steps)
select2$Steps=as.numeric(gsub(",","",select2$Steps))

ggplot(select2, aes(Date, Steps)) + geom_line(color="dark red")+ geom_smooth(method = "lm", se = FALSE,color="black") +xlab("Weeks of the Year")+ylab("Average Steps")

#Plot 4
cor(fit$FitBit.minutesToFallAsleep,fit$FitBit.minutesAsleep)
ggplot(fit, aes(FitBit.minutesToFallAsleep, FitBit.minutesAsleep))+
  geom_point(aes(color = FitBit.minutesAsleep)) + geom_smooth(aes(color = FitBit.minutesAsleep , fill = FitBit.minutesAsleep), method = "lm",color="dark green")+ scale_color_viridis( option = "D")


#Data Preprocessing
#Priciple Component Analysis
Required_PCA<-FitBitData[,c(7,8,9,10,11,12,13,14,15,16,17,18,19,20,24,28,32,36,40,44,45,46,47)]
Required_PCA
Req_PCA<-na.omit(Required_PCA)
Req_PCA
Req_PCA$FitBit.Steps=factor(Req_PCA$FitBit.Steps)
Req_PCA$FitBit.Steps=as.numeric(gsub(",","",Req_PCA$FitBit.Steps))
Req_PCA
colnames(Req_PCA)<-c("Duration","MinsToSleep","MinsAsleep","MinsAwake","TimeInBed","efficiency","DeepMins","Deep30Avg","WakeMins","Wake30Avg","LightMins","Light30Avg","RemMins","Rem30Avg","Level1","Level2","Level3","Level4","Level5","Level6","Steps","BPM","SleepScore")
fa.parallel(Req_PCA,fa="pc",n.iter=100, main="Parallel Analysis Screen Plots")
pca<-principal(Req_PCA,nfactors=1,rotate="varimax")
pca_score<-pca$scores
pca_loadings<-pca$loadings
factor.plot(pca,labels=rownames(pca$loadings))

#Factor Analysis
selection2=select(fit,FitBit.duration,FitBit.levels.summary.deep.minutes,FitBit.levels.summary.wake.minutes,FitBit.levels.summary.light.minutes,FitBit.levels.summary.rem.minutes,Level1Seconds,Level2Seconds,Level3Seconds,Level4Seconds,Level5Seconds,FitBit.minutesToFallAsleep,FitBit.minutesAsleep,FitBit.minutesAwake,FitBit.timeInBed,FitBit.daily_mean_BPM,FitBit.Steps,Sleep.Score)

names(selection2)[1]="Duration"
names(selection2)[2]="DeepStage"
names(selection2)[3]="AwakeStage"
names(selection2)[4]="LightStage"
names(selection2)[5]="RemStage"
names(selection2)[6]="Stage1"
names(selection2)[7]="Stage2"
names(selection2)[8]="Stage3"
names(selection2)[9]="Stage4"
names(selection2)[10]="Stage5"
names(selection2)[11]="TimeToFallAsleep"
names(selection2)[12]="AsleepMins"
names(selection2)[13]="AwakeMins"
names(selection2)[14]="TimeInBed"
names(selection2)[15]="HeartRate"
names(selection2)[16]="StepsWalked"
names(selection2)[17]="SleepScore"

omit=na.omit(selection2)
cor1=cor(omit)

pairs.panels(cor1)
fa.parallel(cor1, fa="fa",n.iter = 100, main = "Scree Plot with Factor Analysis")
#number of factors=4

fa_cor<-fa(cor1)
fit_rotate<-fa(cor1, nfactors = 1, rotate = "varimax", fm="pa")
scores=fa(cor1,scores=TRUE)
factor.plot(fit_rotate,labels=rownames(fit_rotate$loadings))
fa.diagram(scores,labels=rownames(scores$loadings))

#PCA variable factor map
result <- PCA(omit)
#provides a higher level of rotation.

#Data Mining Techniques
#CART 
TrainRatio<-0.50
ValidRatio<-0.30
TestRatio<-0.20

Selection1<-Req_PCA[,c(1,3,4,7,9,11,13,21,22,23)]
TrainSizee<-floor(TrainRatio*nrow(Selection1))
ValideSizee<-floor(ValidRatio*nrow(Selection1))
TestSizee<-floor(TestRatio*nrow(Selection1))

Train<-sort(sample(seq_len(nrow(Selection1)),size = TrainSizee))
NotTrain<-setdiff(seq_len(nrow(Selection1)),Train)
Valid<-sort(sample(NotTrain,size = ValideSizee))
Test<- setdiff(NotTrain,Valid)

SelectTrain<-Selection1[Train,]
SelectValid<-Selection1[Valid,]
SelectTest<-Selection1[Test,]
fit_CART<-rpart(SleepScore~.,method="anova",data=SelectTrain)
printcp(fit_CART)
summary(fit_CART)


#LINEAR REGRESSION:
#Selection1
Selection1
str(Selection1)
Lm_fit1<-lm(SleepScore ~. ,data=Selection1)
summary(Lm_fit1)
ggplot(data=Selection1,aes(Lm_fit1$residuals))+geom_histogram(binwidth = 1, color = "black", fill = "purple4") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Histogram for Model Residuals")
#accuracy(Lm_fit1)
#Lm_fit1 : Multiple R-squred=0.865,F-statistic=42.6, p-value=7.35e-16

#Selection2
Selection2<-Req_PCA[,c(1,2,3,4,5,6,15,16,17,18,19,20,21,22,23)]
Lm_fit2<-lm(SleepScore~.,data = Selection2)
summary(Lm_fit2)
ggplot(data=Selection2,aes(Lm_fit2$residuals))+geom_histogram(binwidth = 1, color = "black", fill = "purple4") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Histogram for Model Residuals")
#Lm_fit2: Multiple R-squared= 0.874,F-statistic=22.2,p-value=1.39e-12

#Selection3
Selection3<-Req_PCA[,c(1,2,6,7,8,13,14,21,22,23)]
Lm_fit3<-lm(SleepScore~.,data = Selection3)
summary(Lm_fit3)
ggplot(data=Selection2,aes(Lm_fit2$residuals))+geom_histogram(binwidth = 1, color = "black", fill = "purple4") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Histogram for Model Residuals")
#Lm_fit3: Multiple R-squared=0.867,F-statistic=26.8, p-value=1.31e-13

#Entire Req_PCA
Lm_fit4<-lm(SleepScore ~. ,data=Req_PCA)
summary(Lm_fit4)
#Lm_fit4: Multiple R-squared= 0.902,F-statistic= 15.7,p-value= 2.72e-10

##Lm_fit1 is the best fit model
set.seed(100)
trainingRowIndex <- sample(1:nrow(Selection1), 0.8*nrow(Selection1))  
trainingData <- Selection1[trainingRowIndex, ]
testData  <- Selection1[-trainingRowIndex, ] 
SSpred<-predict(Lm_fit1,testData) #Predicted Sleep Score
actuals_preds <- data.frame(cbind(actuals=testData$SleepScore, predicteds=SSpred)) 
actuals_preds
correlation_accuracy<-cor(actuals_preds)
head(actuals_preds)
correlation_accuracy #correlation accuracy
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max)) 
min_max_accuracy #min_max_accuracy


#KNN:
#Using Req_PCA
train.index<-sample(c(1:dim(Req_PCA)[1]),0.5*dim(Req_PCA)[1])
Fit.train<-Req_PCA[train.index,]
Fit.valid<-Req_PCA[-train.index,]
Fit.valid.new<-Fit.valid[2:24,]
train1=Fit.train[,c(23)]
valid1=Fit.valid.new[,c(23)]
train.SleepScore=Fit.train$SleepScore
valid.SleepScore=Fit.train$SleepScore
Fit.test <- Fit.valid[0,]
KNNFit<-train(SleepScore ~.,data=Fit.train,method="knn",preProc=c("center","scale"))
print(KNNFit)
set.seed(400)
ctrl <- trainControl(method="repeatedcv",repeats = 3)
kNNFit1 <- train(SleepScore ~ ., data = Fit.train,method = "knn",tuneLength = 10,trControl = ctrl,preProc = c("center", "scale"))
print(kNNFit1)
Pred_Knn_Complete<-knn(Fit.train,Fit.valid,cl=train1,k=5)
Pred_Knn_Complete

head(Req_PCA)

Fit.test[1,]<-c(21256356,62,455,77,600,88,55,300,234,68,62,55,88,56,312,256, 120,800,420,520,672,8292,66.95,83)
Fit2.knn<-knn(train = Fit.train[,-23], test = Fit.test[,-23],cl=Fit.train[,23], k = 5)
Fit2.knn

train2.index<-sample(c(1:dim(Selection1)[1]),0.6*dim(Selection1)[1])
Fit.train2<-Selection1[train2.index,]
Fit.test2<-Selection1[-train2.index,]

train2=Fit.train2[,c(10)]
test2=Fit.test2[,c(10)]

KNNFit2<-train(SleepScore ~.,data=Fit.train2,method="knn",preProc=c("center","scale"))
print(KNNFit2)
set.seed(400)
ctrl <- trainControl(method="repeatedcv",repeats = 3)
kNNFit3 <- train(SleepScore ~ ., data = Fit.train2,method = "knn",tuneLength = 10,trControl = ctrl,preProc = c("center", "scale"))
print(kNNFit3)

Predicted_SS<-knn(Fit.train2,Fit.test2,cl=train2,k=7)
Predicted_SS
Pred_Actual_table<-table(Predicted_SS,test2)
accuracy<-function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(Pred_Actual_table)

#SVM
selection3<-select(fit,FitBit.duration,FitBit.minutesAsleep,FitBit.minutesAwake,FitBit.levels.summary.deep.minutes,FitBit.levels.summary.wake.minutes,FitBit.levels.summary.light.minutes,FitBit.levels.summary.rem.minutes,FitBit.Steps,FitBit.daily_mean_BPM,Sleep.Score)

rm=na.omit(selection3)

data_scale=as.data.frame(scale(rm))
Training <- 0.6
Validation <- 0.4
Trainingsize<- floor(Training * nrow(data_scale))
Validationsize<-floor(Validation * nrow(data_scale))
TrainingIndex <- sort(sample(seq_len(nrow(data_scale)),size=Trainingsize))
OtherIndex <- setdiff(seq_len(nrow(data_scale)),TrainingIndex)
ValidationIndex <- sort(sample(OtherIndex, size=Validationsize))

TrainingData <- data_scale[TrainingIndex,]
ValidationData <- data_scale[ValidationIndex,]

SVR_model=svm(Sleep.Score~.,data=TrainingData)
SVR_predict=predict(SVR_model,ValidationData)

actualandpredicted=as.data.frame(cbind(actual=ValidationData$Sleep.Score,predicted=SVR_predict))
actualandpredicted
cor(actualandpredicted)

Metrics::mae(ValidationData$Sleep.Score,SVR_predict)
Metrics::mape(ValidationData$Sleep.Score,SVR_predict)
Metrics::ae(ValidationData$Sleep.Score,SVR_predict)
Metrics::rmse(ValidationData$Sleep.Score,SVR_predict)


#Neural Networks
Sleep=fit$Sleep.Score
x=as.data.frame(na.omit(Sleep))
names(x)[1]="Sleep.Score"
ggplot(x, aes(x=Sleep.Score))+geom_histogram(aes(y = ..density..),binwidth=5,color="light blue")+geom_density(color="brown")

rmse_f<- function(x,y) {
  return(sqrt(sum((x-y)^2)/nrow(x)))
}

model <- neuralnet(Sleep.Score ~., data=TrainingData, threshold = 1, algorithm = "rprop+",stepmax = 10^8) 
prediction <- predict(object = model, newdata =TrainingData )
rmse_training <- rmse_f(prediction,TrainingData$Sleep.Score)
rmse_training

model <- neuralnet(Sleep.Score ~., data=TrainingData, threshold = 1, algorithm = "rprop+",stepmax = 10^8) 
prediction1 <- predict(object = model, newdata =ValidationData )
rmse_validation <- rmse_f(prediction,ValidationData$Sleep.Score)
rmse_validation


#The RMSE of the test model is very close to that of the training sample, hence we can say that we have built a good model.


cor(prediction,TrainingData$Sleep.Score)
cor(prediction1,ValidationData$Sleep.Score)

