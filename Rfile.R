library(AppliedPredictiveModeling)
library(caret)
data(AlzheimerDisease)

head(predictors)
head(diagnosis)
adData = data.frame(diagnosis,predictors)

adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis, p = 0.50, list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]


data(concrete)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

head(training)
hist(training$Cement)
hist(log(training$Cement))
log(0)
training$Cement

set.seed(3433)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

names(training)
names(training[,58:69])
pca <- prcomp(cor(training[,58:69]))
summary(pca)
pP <- preProcess(training[,58:69], method="pca", thresh=0.8)
summary(pP)
pP

trainingIL = adData[inTrain,c(1,58:59)]
testingIL = adData[-inTrain,c(1,58:59)]
model1 <- train(diagnosis ~ . , data = trainingIL, method="glm")

pP <- preProcess(trainingIL[,-1], method="pca", thresh=0.8)
trainPC <- predict(pP, trainingIL[,-1])
model2 <- train(diagnosis ~ . , data = trainPC, method="glm")















adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]



library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
str(segmentationOriginal)

inTrain <- createDataPartition(y=segmentationOriginal$Case, p=0.75, list=FALSE)
training <- segmentationOriginal[segmentationOriginal$Case=="Train",]
testing <- segmentationOriginal[segmentationOriginal$Case=="Test",]
dim(testing)
set.seed(125)
modFit <- train(Class ~ .,method="rpart",data=training)
print(modFit$finalModel)
plot(modFit$finalModel, uniform=TRUE, main="Classification Tree")
text(modFit$finalModel, use.n=TRUE, all=TRUE, cex=.8)

library(pgmm)
data(olive)
olive = olive[,-1]
modFit <- train(Area ~ .,method="rpart",data=olive)
print(modFit$finalModel)
plot(modFit$finalModel, uniform=TRUE, main="Classification Tree")
text(modFit$finalModel, use.n=TRUE, all=TRUE, cex=.8)
newdata = as.data.frame(t(colMeans(olive)))
predict(modFit,newdata=newdata)

library(ElemStatLearn) 
data(SAheart)
head((SAheart))
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]
set.seed(13234)
modFit<- train(chd ~ alcohol + obesity + tobacco + typea + ldl, method = "glm", onset=age, family="binomial", data=trainSA)
finMod <- modFit$finalModel
print(finMod)
missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
pr <- predict(modFit, newdata=trainSA)
missClass(trainSA, pr)

data(vowel.train)
data(vowel.test) 
str(vowel.train)
vowel.train$y <- factor(vowel.train$y)
vowel.test$y <- factor(vowel.test$y)
set.seed(33833)
modFit <- train(y ~ ., data=vowel.train, method="rf",prox=TRUE)
varImp(modFit)
