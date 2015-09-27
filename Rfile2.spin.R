setwd("Z:\\Dokumente\\Coursera\\Practical Machine Learning")
getwd()
pml <- read.csv("pml-training.csv")
library(dplyr)
library(ggvis)
library(caret)
library(RANN)
pml.dt <- tbl_df(pml)

head(pml.dt)
nums <- pml.dt %>%  sapply(is.numeric)
pml.dt.n <- pml.dt[nums]
pml.dt.f <- pml.dt[!nums]
glimpse(pml.dt.f)

filled <- pml.dt.f %>% sapply(function(x) sum(x!="")) %>% sapply(function(x) x>10000)
pml.dt.f <- pml.dt.f[filled]
dummies <- dummyVars(classe ~ user_name + new_window, pml.dt.f)
pml.f.pr <- data.frame(predict(dummies, newdata = pml.dt.f))

preP <- preProcess(pml.dt.n, method=c("center", "scale", "knnImpute", "pca"))
pml.n.pr <- predict(preP, pml.dt.n)
glimpse(pml.n.pr)

# M <- abs(cor(pml.n.pr))
# diag(M) <- 0
# which(M > 0.8, arr.ind = T)

pml.clean <- bind_cols(pml.n.pr, pml.f.pr, pml.dt.f[,c(2,4)])
pml.clean[52] #classe

nearZeroVar(pml.clean, saveMetrics = T)

sample <- createDataPartition(y=pml.clean$classe, p=0.1, list=F)
pml.sample <- pml.clean[sample,]
sample2 <- createDataPartition(y=pml.clean$classe, p=0.1, list=F)
pml.sample2 <- pml.clean[sample2,]

#featurePlot(x=pml.sample, y = pml.sample$classe, plot="pairs")

treeFit <- train(classe~., method="rpart", data=pml.sample)
print(treeFit$finalModel)      
plot(treeFit$finalModel)
library(rattle)
fancyRpartPlot(treeFit$finalModel)
treePrediction <- predict(treeFit, pml.sample2)
confusionMatrix(treePrediction, pml.sample$classe) #52%

forestFit <- train(classe~., method="rf", data=pml.sample, prox=T)
forestFit$finalModel
plot(forestFit$finalModel)
forestPrediction <- predict(forestFit, pml.sample2)
confusionMatrix(forestPrediction, pml.sample$classe) #92.36%

boostFit <- train(classe~., method="gbm", data=pml.sample, verbose=F)
print(boostFit)
plot(boostFit$finalModel)
boostPrediction <- predict(boostFit, pml.sample2)
confusionMatrix(boostPrediction, pml.sample$classe) #90.43%

ldaFit <- train(classe~., method="lda", data=pml.sample)
summary(ldaFit$finalModel)
ldaPrediction <- predict(ldaFit, pml.sample2)
confusionMatrix(ldaPrediction, pml.sample$classe) #85.79%

bayesFit <- train(classe~., method="nb", data=pml.sample) # warnings
summary(bayesFit$finalModel)
bayesPrediction <- predict(bayesFit, pml.sample2)
confusionMatrix(bayesPrediction, pml.sample$classe) #63.19%


accuracies <- c(tree=confusionMatrix(treePrediction, pml.sample$classe)$overall[1])
accuracies <- c(accuracies, forest=confusionMatrix(forestPrediction, pml.sample$classe)$overall[1])
accuracies <- c(accuracies, boost = confusionMatrix(boostPrediction, pml.sample$classe)$overall[1])
accuracies <- c(accuracies, lda = confusionMatrix(ldaPrediction, pml.sample$classe)$overall[1])
accuracies <- c(accuracies, bayes = confusionMatrix(bayesPrediction, pml.sample$classe)$overall[1])
stack(accuracies) %>% ggvis(~ind, ~values) %>% layer_bars() %>% hide_legend("fill") %>% add_axis(type="x", title="Method") %>% add_axis(type="y", title="Accuracy")
