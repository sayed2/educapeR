####### Course
## Data
load("~/gitprojects/Reducaper/Reducaper/gendata.RData")
head(simstudy_coursegen)
simdata001$yFactor <- factor(simdata001[,"y"], levels = 1:5, ordered = T)
ind <- sample(2, nrow(simdata001), replace = TRUE, prob =c(0.8,0.2))
train <- simdata001[ind == 1,]
test <- simdata001[ind == 2,]
# Model building
library(nnet)
model1 <- multinom(yFactor~D+CT+CS+CD,train)
summary(model1)
pred <- predict(model1, train)
pred
pred1 <- predict(model1, test)

# Misclassification error 
contingencytab <- table(pred1, test$yFactor)
1-sum(diag(contingencytab))/sum(contingencytab)
0.6031746
## Random forest
library(randomForest)
library(magrittr)
dataRF <- simstudy_coursegen %>% 
  dplyr::select(D,CT,CS,CD,yFactor) 
ind <- sample(2, nrow(dataRF), replace = TRUE, prob =c(0.8,0.2))
train <- dataRF[ind == 1,]
test <- dataRF[ind == 2,]

model2 <- randomForest(yFactor~D+CT+CS+CD, data = train, 
                       importance = TRUE, ntree = 4000)
pred2 <- predict(model2, test)
# Misclassification error 
contingencytab <- table(pred2, test$yFactor)
1-sum(diag(contingencytab))/sum(contingencytab)
0.363636
## decision tree + bagging
library(caret)
names(getModelInfo())
prop <- prop.table(table(simdata001$y))
xtab <-  xtabs(~y+D, simdata001)
tab_prob <- cbind(xtab, prop)

set.seed(1234)
rating <- simdata001[sample(nrow(simdata001)),]
split <- floor(nrow(simdata001)/3)
ensembleData <- simdata001[0:split,]
blenderData <- simdata001[(split+1):(split*2),]
testingData <- simdata001[(split*2+1):nrow(rating),]

avoidy <- "y"
labelName <- 'yFactor'
predictors <- names(ensembleData)[names(ensembleData) != labelName & 
                                    names(ensembleData) != avoidy]

# We create a caret trainControl object to control the 
# number of cross-validations performed (the more the 
# better but for breivity we only require 3):

myControl2 <- trainControl(method='cv', number=10, returnResamp='none')


# Benchmark Model

# We run the data on a gbm model without any enembling to 
# use as a comparative benchmark:
  
test_model3 <- train(blenderData[,predictors], blenderData[,labelName], 
                    method='treebag', trControl=myControl2)

# We then use the model to predict 6-cylinder vehicles using the testingData 
# data set and pROC’s auc function to get the Area Under the Curve (AUC):

preds0 <- predict(object=test_model3, testingData[,predictors])
library(pROC)
auc <- multiclass.roc(testingData[,labelName], preds0)
print(auc$auc) 
contingencytab <- table(preds0, testingData$yFactor)
1-sum(diag(contingencytab))/sum(contingencytab)

# Ensemble
grid <- expand.grid(.fL=c(0), .usekernel=c(FALSE))
model_gbm <- train(ensembleData[,predictors], ensembleData[,labelName], method='gbm', trControl=myControl2)

model_rpart <- train(ensembleData[,predictors], ensembleData[,labelName], method='rpart', trControl=myControl2)

model_treebag <- train(ensembleData[,predictors], ensembleData[,labelName], method='treebag', trControl=myControl2)

model_multinom <- train(ensembleData[,predictors], ensembleData[,labelName], method='multinom', trControl=myControl2)
# Now we test this tree models on blender and testing data

blenderData$gbm_PROB <- predict(object=model_gbm, blenderData[,predictors])
blenderData$rf_PROB <- predict(object=model_rpart, blenderData[,predictors])
blenderData$treebag_PROB <- predict(object=model_treebag, blenderData[,predictors])
blenderData$multinom_PROB <- predict(object=model_multinom, blenderData[,predictors])

testingData$gbm_PROB <- predict(object=model_gbm, testingData[,predictors])
testingData$rf_PROB <- predict(object=model_rpart, testingData[,predictors])
testingData$treebag_PROB <- predict(object=model_treebag, testingData[,predictors])
testingData$multinom_PROB <- predict(object=model_multinom, testingData[,predictors])
# see how each individual model performed on its own
auc <- multiclass.roc(testingData[,labelName], testingData$gbm_PROB )
print(auc$auc) # Area under the curve: 0.9893

auc <- multiclass.roc(testingData[,labelName], testingData$rf_PROB )
print(auc$auc) # Area under the curve: 0.958

auc <- multiclass.roc(testingData[,labelName], testingData$treebag_PROB )
print(auc$auc) # Area under the curve: 0.9734
# run a final model to blend all the probabilities together

auc <- multiclass.roc(testingData[,labelName], testingData$multinom_PROB )
print(auc$auc) #


predictors <- names(blenderData)[names(blenderData) != labelName & names(blenderData) != avoidy]
final_blender_model2 <- train(blenderData[,predictors], blenderData[,labelName], method='gbm', trControl=myControl2)

# See final prediction and AUC of blended ensemble
preds1 <- predict(object=final_blender_model1, testingData[,predictors])
auc <- multiclass.roc(testingData[,labelName], preds1)
print(auc$auc)

# Feature engineering
for (i in 1:nrow(simdata001)) {
     simdata001$feature1[i] <- simdata001$D[i]*simdata001$CT[i]*simdata001$CD[i]                   
}
# NOW redo the model building

simdata001$feature1 <- NULL



