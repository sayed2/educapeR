## DATA GENERATION
## 1 - Data generation & Naive Bayes BN contruction for Course

## Method explanation : --------------------------------------------------------
# Suppose linearity between feedback and learning to use the effect size for 
# prediction

## -----------------------------------------------------------------------------
## y : Course rating estination
## D : Domain | Humanities (Factor 1), 
##              Engineering (Factor 2)                 ES_D = 0.3 
## CT : Course type | higher(3,4th grade)(Factor 1) , 
##                    lower(1,2nd grade)(Factor 2)     ES_CT = 0.4  
## CS : Class size | CS < 50 (Factor 1) , 
##                   CS > 50 (Factor 2)                ES_CS = 0.45
## CD : Course difficulty | CD < 4(Factor 1), 
##                          CD e [4,7[(Factor 2), 
##                          CD >7 (Factor 3)           ES_CD = 0.5

data_course <- matrix(data = NA, nrow = 100, ncol = 9)
colnames(data_course) <-c("D","CT", "CS", "CD", "Dfactor", "CTfactor", 
                          "CSfactor", "CDfactor", "y")
data_course <- as.data.frame(data_course)
# Instead of rounding the results we gonna consider every ES = 0.5
# and consider the factors are equal to 2 and (+ | -)

set.seed(100)
data_course[,1] <- sample(c("Humanities", "Engineering"), 100, replace = TRUE)
data_course[,2] <- sample(c("lower", "higher"), 100, replace = TRUE)
data_course[,3] <- sample(c(20:80), 100, replace = TRUE)
data_course[,4] <- sample(c(2:10), 100, replace = TRUE)
N = 100
# Transforming predictors to factors 
# Dfactor
data_course[,5] <- factor(data_course[,1])
# CTfactor
data_course[,6] <- factor(data_course[,2])
# CSfactor
data_course$CSfactor[data_course$CS <= 50] = 1
data_course$CSfactor[data_course$CS > 50] = 2
data_course[,7] <- factor(data_course[,7])
# CDfactor
data_course$CDfactor[data_course$CD <= 4] = 1
data_course$CDfactor[data_course$CD > 4 & data_course$CD <=7] = 2
data_course$CDfactor[data_course$CD > 7] = 3
data_course[,8] <- factor(data_course[,8], levels = 1:3, ordered = T)
# Application of ES on the data

shiftedData <- data.frame(yDfactor = integer(), yCTfactor = integer(),
                          yCSfactor = integer(), yCDfactor = integer(),
                          yFinal = integer(), stringsAsFactors = FALSE)

ES = 0.5    #        can be the exact value of the effect size
#        but then you get to round the result if needed
#        When dealing with ordinal data
Xfactor = 2 #        can be other coefficient of a link function
ES_D = 0.25
ES_CT = 0.4
ES_CS = 0.55
ES_CD = 0.70

set.seed(200)
for(i in 1:N) {
  ybase = sample(c(1,2,3,4,5), 1, replace = TRUE)
  # D
  if (data_course[i,5] == 2) {
    shiftedData[i,1] <- ybase + Xfactor*ES_D
  }  else {
    shiftedData[i,1] <- ybase - Xfactor*2*ES_D
  }
  # CT
  if (data_course[i,6] == 2) {
    shiftedData[i,2] <- ybase - Xfactor*2*ES_CT
  } else {
    shiftedData[i,2] <- ybase + Xfactor*ES_CT
  }
  # CS
  if (data_course[i,7] == 2) {
    shiftedData[i,3] <- ybase - Xfactor*2*ES_CS
  } else {
    shiftedData[i,3] <- ybase + Xfactor*ES_CS
  }
  # CD
  if (data_course[i,8] == 1) {
    shiftedData[i,4] <- ybase + Xfactor*2*ES_CD
  } else if (data_course[i,8] == 3) {
    shiftedData[i,4] <- ybase - Xfactor*2*ES_CD
  } else {
    shiftedData[i,4] <- ybase
  }
  
}

# computing yfactor
for (i in 1:N) {
  for(j in 1:4) {
    shiftedData[i,5] <- (ES_D*shiftedData[i,1]+ES_CT*shiftedData[i,2]+ES_CS*shiftedData[i,3]+
                           ES_CD*shiftedData[i,4])/(ES_D+ES_CT+ES_CS+ES_CD)
  }
}

data_course[,"y"] <- shiftedData[,"yFinal"]
# Descritization 
data_course[,"y"] <- round(data_course[,"y"], digits = 0)
for (i in 1:nrow(data_course)) {
  if (data_course[i,"y"] < 0) {
    data_course[i,"y"] = 1
  } 
  if (data_course[i,"y"] > 5 ) {
    data_course[i,"y"] = 5
  }
}





library(dplyr)
library(magrittr)

Engineering <- data_course %>%
  filter(data_course[,1] == "Engineering")


## Prediction model
############################################################
## Data partitioning 
data_course$yfactor <- factor(data_course[,"y"],levels = 1:5, ordered = T)
data_course$Dfactor <- factor(data_course[,"Dfactor"], levels = 1:2, ordered = T)
data_course$CTfactor <- factor(data_course[,"CTfactor"], levels = 1:2, ordered = T)
data_course$CSfactor <- factor(data_course[,"CSfactor"], levels = 1:2, ordered = T)
data_course$CDfactor <- factor(data_course[,"CDfactor"], levels = 1:3, ordered = T)

ind <- sample(2, nrow(surveyCS), replace = TRUE, prob =c(0.8,0.2))
train <- surveyCS[ind == 1,]
test <- surveyCS[ind == 2,]
## Model - method 1 : Probit you have to descritize here
library(MASS) 

model <- polr(yfactor~Dfactor+CTfactor+CSfactor+CDfactor,train, method = "probit" , Hess = TRUE)
model
# P-value calculation:
ctable <- coef(summary(model))
p <- pnorm(abs(ctable[,"t value"]), lower.tail = FALSE)*2
ctable <- cbind(ctable, "p value" = p)

# prediction

pred <- predict(model, test)
pred
print(pred)
###########################################################


########
## Model2 - method 2 : Multinomial logistic regression
data_course$yProbitFactor <- factor(surveyCS[,"yProbit"], levels = 1:2, ordered = T)
surveyCS$yProbitFactor <- relevel(surveyCS$yProbitFactor, ref = "1")
# Data partitioning:
surveyCS <- data_course[1:100,] %>%
  dplyr::select(Dfactor,CTfactor,CSfactor,CDfactor,yProbitFactor)
ind <- sample(2, nrow(surveyCS), replace = TRUE, prob =c(0.8,0.2))
train <- surveyCS[ind == 1,]
test <- surveyCS[ind == 2,]
# Model building
library(nnet)
model1 <- multinom(yProbitFactor~Dfactor+CTfactor+CSfactor+CDfactor,train)
summary(model1)
pred <- predict(model1, train)
pred
# Misclassification error 
contingencytab <- table(predict(model1), train$yProbitFactor)
1-sum(diag(contingencytab))/sum(contingencytab)


pred1 <-predict(model1, test)
contingencytab1 <- table(pred1, test$yProbitFactor)
1-sum(diag(contingencytab1))/sum(contingencytab1)

library(pROC)
auc <- roc(test[,"yProbitFactor"], pred1)

###### Stacking
#### Interation 1
datastacking <- train %>%
  dplyr::select(D,CT,CSfactor,CDfactor)
datastacking$out <- pred
# Split
ind <- sample(2, nrow(data_course), replace = TRUE, prob =c(0.5,0.5))
datastacktrain <- datastacking[ind == 1,]
datastacktest <- datastacking[ind == 2,]
# Model Building
model2 <- multinom(out~D+CT+CSfactor+CDfactor,datastacktrain)
summary(model2)
pred3 <- predict(model2, datastacktrain)
pred4 <- predict(model2, datastacktest)
pred5 <- predict(model2, test)
# Misclassification error 
contMtab <- table(pred5, test$out)
1-sum(diag(contMtab))/sum(contMtab)

#### Interation 2
datastacking2 <- datastacktrain %>%
  dplyr::select(D,CT,CSfactor,CDfactor)
datastacking2$out <- pred3
# Split
ind <- sample(2, nrow(datastacking2), replace = TRUE, prob =c(0.5,0.5))
datastacktrain2 <- datastacking2[ind == 1,]
datastacktest2 <- datastacking2[ind == 2,]
# Model Building
model3 <- multinom(out~D+CT+CSfactor+CDfactor,datastacktrain2)
summary(model3)
pred6 <- predict(model3, datastacktrain2)
pred7 <- predict(model3, datastacktest2)
pred8 <- predict(model3, test)
# Misclassification error 
contMtab <- table(pred8, test$out)
1-sum(diag(contMtab))/sum(contMtab)


# Naive Bayes in e1071
library(e1071)
modelnaiveB <- naiveBayes(out~Dfactor+CTfactor+CSfactor+CDfactor ,data= train)
p <- predict(modelnaiveB,train$out)


plot(p)
table(p, train$out)
# Accuracy 
ContmatrixN <- table(p, train$out)
1-sum(diag(ContmatrixN))/sum(ContmatrixN)

## Naive Bayes in bnlearn
library(bnlearn)
library(Rgraphviz)
data <- data_course %>%
  dplyr::select(Dfactor,CTfactor,CSfactor,CDfactor,yfactor)
nbcl <- naive.bayes(data, training = "yfactor")
graphviz.plot(nbcl, layout = "fdp")

# Split the data
ind <- sample(2, nrow(data), replace = TRUE, prob =c(0.8,0.1))
trainNB <- data[ind == 1,]
testNB <- data[ind == 2,]

nbcl.trained = bn.fit(nbcl, trainNB)
coef(nbcl.trained$yfactor)

coef(nbcl.trained$Dfactor)
# Accuracy
cv.nb = bn.cv(nbcl, data = data, runs = 10, method = "k-fold", folds = 10)
cv.nb

## Tree-Augmented Naive Bayes Classifier

tancl = tree.bayes(data, training = "yfactor")
graphviz.plot(tancl)
# Training
tancl.trained = bn.fit(tancl, trainNB)

coef(tancl.trained$CDfactor)
# Accuracy
cv.tan = bn.cv("tree.bayes", data = data, runs = 10, method = "k-fold",
               folds = 10, algorithm.args = list(training = "yfactor"))

#### glmnet
library(glmnet)
library(caret)

model4 <- train(out~D+CT+CSfactor+CDfactor,train, method='glmnet',
               tuneGrid=expand.grid(
                 .alpha=0:1,
                 .lambda=0:30/10))
model5 <- train(out~D+CT+CSfactor+CDfactor,train, method='glmnet',
                tuneGrid=expand.grid(
                  .alpha=0.8,
                  .lambda=0.001))
model6 <- train(out~D+CT+CSfactor+CDfactor,train, method='glmnet',
                tuneGrid=expand.grid(
                  .alpha=0.5,
                  .lambda=0.001))
model7 <- train(out~D+CT+CSfactor+CDfactor,train, method='glmnet',
                tuneGrid=expand.grid(
                  .alpha=0.1,
                  .lambda=0.0001))
plot(model4)
plot(model5) # It cant be plotted
plot(model6)
plot(model7)
coef(model4$finalModel, s=model4$bestTune$.lambda)
# Prediction
pred9 <- predict(model4, train)
pred10 <- predict(model5, train)
pred11 <- predict(model6, train)
pred12 <- predict(model7, train)
# Accuracy 
ContmatrixN <- table(pred12, train$out)
1-sum(diag(ContmatrixN))/sum(ContmatrixN)

# cross-validation setup

set.seed(1)
cv_index <- createFolds(datastacking, returnTrain = TRUE)
cv_control <- trainControl(method = "cv", index = cv_index)

# run model

set.seed(1)
modelfinal1 <- train(solTrainXtrans, y = solTrainY,
                   method = "pcr",
                   tuneGrid = expand.grid(ncomp = 89),
                   trControl = cv_control
)



################################################################################
library(caret)
names(getModelInfo())
# Data descritization 0: no problem 1: there is problem
data_course
data_course$yProbit <- ifelse(data_course$y>3, 0,1)
data_course$yProbitFactor <- factor(data_course$yProbit)
prop.table(table(ensembleData$yProbit))


# Shuffle and split the data into three parts
set.seed(1234)
datacourse <- data_course[sample(nrow(data_course)),]
split <- floor(nrow(data_course)/3)
ensembleData <- data_course[0:split,]
blenderData <-data_course[(split-1):(split*2),]
testingData <- data_course[(split*2-1):nrow(data_course),]

# set labels names and predictors :
labelName <- ""

#########################################################################
#########################################################################
#########################################################################
# binp1
set.seed(100)
Simulated <- as.numeric()

  txtstring <- '
  data {
  for ( i in 1:N ) {
      Simulated[i] ~ dbin( theta[i] , N )
    }
    for ( sIdx in 1:N ) {
      theta[sIdx] ~ dbeta( omega*(kappa-2)+1 , (1-omega)*(kappa-2)+1 ) 
    }
    omega ~ dbeta( 2 , 3 ) # broad uniform beta(4,2) we get more 1
    # omega ~ dbeta( 5001 , 15001 ) # Skeptical prior for ESP
    kappa <- kappaMinusTwo + 2
    # kappaMinusTwo ~ dgamma( 0.01 , 0.01 )  # mean=1 , sd=10 (generic vague)
    kappaMinusTwo ~ dgamma( 1.105125 , 0.1051249 )  # mode=1 , sd=10 
    # kappaMinusTwo ~ dgamma( 36 , 0.12 )  # mode=300 , sd=50 : skeptical 
  }
  model {
  fake <- 0
  }
  #monitor# Simulated
  #data# N
  '
  library(runjags)
  
  N <- 100
  Simulated <- coda::as.mcmc(run.jags(txtstring, sample=1, n.chains=1, summarise=FALSE))
  Simulated
  str(Simulated)
 
  for(i in 1:N) {
    if (Simulated[i] >= 50) {
      binp1[i] <- 1
    } else {
      binp1[i] <- 0
    }
  }
  hist(binp1)
  
   # discritization
  
  binp1_5 <- as.numeric()
  for(i in 1:100) {
    binp1_5[i] <- Simulated[i]/20  
  }
  hist(binp1_5)
  
  binp1_5_dis <- as.numeric()
  binp1_5_dis <- round(binp1_5, digits = 0)  
  hist(binp1_5_dis)
 
  
  
  
# binp2
  set.seed(100)
  Simulated <- as.numeric()
  
  txtstring <- '
  data {
  for ( i in 1:N ) {
  Simulated[i] ~ dbin( theta[i] , N )
  }
  for ( sIdx in 1:N ) {
  theta[sIdx] ~ dbeta( omega*(kappa-2)+1 , (1-omega)*(kappa-2)+1 ) 
  }
  omega ~ dbeta( 1 , 1 ) # broad uniform beta(4,2) we get more 1
  # omega ~ dbeta( 5001 , 15001 ) # Skeptical prior for ESP
  kappa <- kappaMinusTwo + 2
  # kappaMinusTwo ~ dgamma( 0.01 , 0.01 )  # mean=1 , sd=10 (generic vague)
  kappaMinusTwo ~ dgamma( 1.105125 , 0.1051249 )  # mode=1 , sd=10 
  # kappaMinusTwo ~ dgamma( 36 , 0.12 )  # mode=300 , sd=50 : skeptical 
  }
  model {
  fake <- 0
  }
  #monitor# Simulated
  #data# N
  '
  library(runjags)
  
  N <- 100
  Simulated <- coda::as.mcmc(run.jags(txtstring, sample=1, n.chains=1, summarise=FALSE))
  Simulated
  str(Simulated)
  # discritization
  binp2 <- as.numeric()
  for(i in 1:N) {
    if (Simulated[i] >= 50) {
      binp2[i] <- 1
    } else {
      binp2[i] <- 0
    }
  }

  hist(binp2)  
  
  # discritization
  
  binp2_5 <- as.numeric()
  for(i in 1:100) {
    binp2_5[i] <- Simulated[i]/20  
  }
  hist(binp2_5)
  
  binp2_5_dis <- as.numeric()
  binp2_5_dis <- round(binp2_5, digits = 0)  
  hist(binp2_5_dis)

# binp3  
  set.seed(100)
  Simulated <- as.numeric()
  
  txtstring <- '
  data {
  for ( i in 1:N ) {
  Simulated[i] ~ dbin( theta[i] , N )
  }
  for ( sIdx in 1:N ) {
  theta[sIdx] ~ dbeta( omega*(kappa-2)+1 , (1-omega)*(kappa-2)+1 ) 
  }
  omega ~ dbeta( 3 , 2 ) # broad uniform beta(4,2) we get more 1
  # omega ~ dbeta( 5001 , 15001 ) # Skeptical prior for ESP
  kappa <- kappaMinusTwo + 2
  # kappaMinusTwo ~ dgamma( 0.01 , 0.01 )  # mean=1 , sd=10 (generic vague)
  kappaMinusTwo ~ dgamma( 1.105125 , 0.1051249 )  # mode=1 , sd=10 
  # kappaMinusTwo ~ dgamma( 36 , 0.12 )  # mode=300 , sd=50 : skeptical 
  }
  model {
  fake <- 0
  }
  #monitor# Simulated
  #data# N
  '
  library(runjags)
  
  N <- 100
  Simulated <- coda::as.mcmc(run.jags(txtstring, sample=1, n.chains=1, summarise=FALSE))
  Simulated
  str(Simulated)
  # discritization
  binp3 <- as.numeric()
  for(i in 1:N) {
    if (Simulated[i] >= 50) {
      binp3[i] <- 1
    } else {
      binp3[i] <- 0
    }
  }

  hist(binp3)  
  
  # discritization
  
  binp3_5 <- as.numeric()
  for(i in 1:100) {
    binp3_5[i] <- Simulated[i]/20  
  }
  hist(binp3_5)
  
  binp3_5_dis <- as.numeric()
  binp3_5_dis <- round(binp3_5, digits = 0)  
  hist(binp3_5_dis)
  
# Use simstudy :
  binp1_5 <- as.data.frame(binp1_5)
  binp2_5 <- as.data.frame(binp2_5)
  binp3_5 <- as.data.frame(binp3_5)

  binp1_5$high <- as.character("high") 
  binp2_5$average <- as.character("average")
  binp3_5$low <- as.character("low")

colnames(binp1_5) <- c("Performance", "dist")
colnames(binp2_5) <- c("Performance", "dist")
colnames(binp3_5) <- c("Performance", "dist")

save(binp1_5)

save(list = c("binp1_5", "binp2_5", "binp3_5"), file = "gendata.RData")

#######################################################################
# Data generation using simstudy 
library(simstudy)
# D formula="0.65;0.35" they tend to overate
def <- defData(varname = "D", dist = "categorical", formula="0.60;0.40", id = "idnum")
def <- defData(def, varname="CT", dist="categorical", formula="0.5;0.5")
def <- defData(def, varname="CD", formula="1;10", dist="uniform")
def <- defData(def, varname = "CS", formula = 60, variance = 50, dist = "normal")
#def <- defData(def, varname = "y", formula = "1/(0.1*D+0.24*CT+0.2*CS+0.31*CD)", 
#              variance = 0.1, dist = "normal")

dt <- genData(300, def)
dt$CS <-  round(dt$CS, digits = 0)
dt$CD <-  round(dt$CD, digits = 0)
#dt$CD[dt$CD >= 7] =  2
#dt$CD[dt$CD < 7] = 1
#dt$CS[dt$CS <= 50 ] = 1
#dt$CS[dt$CS > 50] = 2

#dt <- as.data.frame(dt)
#dt$y <- abs(dt$y) * 5 # *10 apres / 20
#dt$y <-  round(dt$y, digits = 0)

#dt$y[dt$y == 0] =1
#dt$y[dt$y > 5] =5
simstudy_coursegen <- dt
save(simstudy_coursegen, file = "gendata.RData")



# CD = 2 , CS = 2 , D = 2 , CT = 2
y1 <- "(a0 - a1*D - a2*CT - a3*CS - a4*CD)/(D+CT+CS+CD)"
# CD = 2 , CS = 2 , D = 1, CT = 1 
y2 <- "(a0 + a1*D + a2*CT - a3*CS - a4*CD)/(D+CT+CS+CD)"
# CD = 2 , CS = 2 , D = 2, CT = 1 
y3 <- "(a0 - a1*D + a2*CT - a3*CS - a4*CD)/(D+CT+CS+CD)"
# CD = 2 , CS = 2 , D = 1, CT = 2 
y4 <- "(a0 + a1*D - a2*CT - a3*CS - a4*CD)/(D+CT+CS+CD)"

# CD = 2 , CS = 1 , D = 2 , CT = 2
y5 <- "(a0 - a1*D - a2*CT + a3*CS - a4*CD)/(D+CT+CS+CD)"
# CD = 2 , CS = 1 , D = 1, CT = 1 
y6 <- "(a0 + a1*D + a2*CT + a3*CS - a4*CD)/(D+CT+CS+CD)"
# CD = 2 , CS = 1 , D = 2, CT = 1 
y7 <- "(a0 - a1*D + a2*CT + a3*CS - a4*CD)/(D+CT+CS+CD)"
# CD = 2 , CS = 1 , D = 1, CT = 2 
y8 <- "(a0 + a1*D - a2*CT + a3*CS - a4*CD)/(D+CT+CS+CD)"
formula="0.65;0.35"
#

# CD = 1 , CS = 2 , D = 2 , CT = 2
y9 <- "(a0 - a1*D - a2*CT - a3*CS + a4*CD)/(D+CT+CS+CD)"
# CD = 1 , CS = 2 , D = 1, CT = 1 
y10 <- "(a0 + a1*D + a2*CT - a3*CS + a4*CD)/(D+CT+CS+CD)"
# CD = 1 , CS = 2 , D = 2, CT = 1 
y11 <- "(a0 - a1*D + a2*CT - a3*CS + a4*CD)/(D+CT+CS+CD)"
# CD = 1 , CS = 2 , D = 1, CT = 2 
y12 <- "(a0 + a1*D - a2*CT - a3*CS + a4*CD)/(D+CT+CS+CD)"

# CD = 1 , CS = 1 , D = 2 , CT = 2
y13 <- "(a0 - a1*D - a2*CT + a3*CS + a4*CD)/(D+CT+CS+CD)"
# CD = 1 , CS = 1 , D = 1, CT = 1 
y14 <- "(a0 + a1*D + a2*CT + a3*CS + a4*CD)/(D+CT+CS+CD)"
# CD = 1 , CS = 1 , D = 2, CT = 1 
y15 <- "(a0 - a1*D + a2*CT + a3*CS + a4*CD)/(D+CT+CS+CD)"
# CD = 1 , CS = 1 , D = 1, CT = 2 
y16 <- "(a0 + a1*D - a2*CT + a3*CS + a4*CD)/(D+CT+CS+CD)"

library(simstudy)
def2 <- defDataAdd(varname = "a0", dist = "categorical", formula="0.05;0.1;0.2;0.4;0.25")
def2 <- defDataAdd(def2, varname = "a1", dist = "nonrandom", formula=0.29)
def2 <- defDataAdd(def2, varname = "a2", dist = "nonrandom", formula=0.37)
def2 <- defDataAdd(def2, varname = "a3", dist = "nonrandom", formula=0.20)
def2 <- defDataAdd(def2, varname = "a4", dist = "nonrandom", formula=0.31)
dt <- addColumns(def2, dt)

      
                    
defC <- defCondition(condition = "CD > 7 & CS > 50 & D == 2 & CT == 2" , formula = y1 , variance = 0,
                    dist = "normal", link = "identity")
defC <- defCondition(defC , condition = "CD > 7 & CS > 50 & D == 1 & CT == 1" , formula = y2 , variance = 0,
                     dist = "normal", link = "identity")
defC <- defCondition(defC , condition = "CD > 7 & CS > 50 & D == 2 & CT == 1" , formula = y3 , variance = 0,
                     dist = "normal", link = "identity")
defC <- defCondition(defC , condition = "CD > 7 & CS > 50 & D == 1 & CT == 2" , formula = y4 , variance = 0,
                     dist = "normal", link = "identity")

defC <- defCondition(defC , condition = "CD > 7 & CS <= 50 & D == 2 & CT == 2" , formula = y5 , variance = 0,
                     dist = "normal", link = "identity")
defC <- defCondition(defC , condition = "CD > 7 & CS <= 50 & D == 1 & CT == 1" , formula = y6 , variance = 0,
                     dist = "normal", link = "identity")
defC <- defCondition(defC , condition = "CD > 7 & CS <= 50 & D == 2 & CT == 1" , formula = y7 , variance = 0,
                     dist = "normal", link = "identity")
defC <- defCondition(defC , condition = "CD > 7 & CS <= 50 & D == 1 & CT == 2" , formula = y8 , variance = 0,
                     dist = "normal", link = "identity")

defC <- defCondition(defC , condition = "CD <= 7 & CS > 50 & D == 2 & CT == 2" , formula = y9 , variance = 0,
                     dist = "normal", link = "identity")
defC <- defCondition(defC , condition = "CD <= 7 & CS > 50 & D == 1 & CT == 1" , formula = y10 , variance = 0,
                     dist = "normal", link = "identity")
defC <- defCondition(defC , condition = "CD <= 7 & CS > 50 & D == 2 & CT == 1" , formula = y11 , variance = 0,
                     dist = "normal", link = "identity")
defC <- defCondition(defC , condition = "CD <= 7 & CS > 50 & D == 1 & CT == 2" , formula = y12 , variance = 0,
                     dist = "normal", link = "identity")

defC <- defCondition(defC , condition = "CD <= 7 & CS <= 50 & D == 2 & CT == 2" , formula = y13 , variance = 0,
                     dist = "normal", link = "identity")
defC <- defCondition(defC , condition = "CD <= 7 & CS <= 50 & D == 1 & CT == 1" , formula = y14 , variance = 0,
                     dist = "normal", link = "identity")
defC <- defCondition(defC , condition = "CD <= 7 & CS <= 50 & D == 2 & CT == 1" , formula = y15 , variance = 0,
                     dist = "normal", link = "identity")
defC <- defCondition(defC , condition = "CD <= 7 & CS <= 50 & D == 1 & CT == 2" , formula = y16 , variance = 0,
                     dist = "normal", link = "identity")

dt <- addCondition(defC, dt, "y")
dt$y <- abs(dt$y)
dt$y <- 1/dt$y
dt$y <- dt$y/2
dt$y <-  round(dt$y, digits = 0)
dt$y[dt$y == 2] =  sample(c(1,2), prob = c(0.5,0.5))


library(dplyr)
library(magrittr)
dt <- as.data.frame(dt)
dt[,"y"] <- round(dt[,"y"], digits = 0)
simdata001 <- dt %>%
                 dplyr::select(y, D, CT, CD, CS)
simdata001$y[simdata001$y <= 0] =  1
simdata001$y[simdata001$y >= 5] =  5
simdata001$yFactor <- factor(simdata001$y)

