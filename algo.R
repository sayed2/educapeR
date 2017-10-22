install.packages("coda")
install.packages("rjags")
install.packages("runjags")

library(coda)
library(rjags)
library(runjags)
library(dplyr)
library(ggplot2)
# Generate data

data1 <- data.frame(studentID = seq(1:100), abil_QZ00 = numeric(100),
         course_diff = numeric(100), teacher_exp = numeric(100),
         Q00_objClarity = numeric(100), st_engagment = numeric(100),
         clarity = numeric(100), QZ01 = numeric(100), Q02 = numeric(100))

data1$abil_QZ00 <- rnorm(100, 55, 10)
data1$course_diff <- 10
data1$teacher_exp <- 7
                  ####### generating correlated data #######
for(i in 1:length(data1$studentID)) {
  
# Objectives clarity data generation 
  data1$Q00_objClarity[i] <- 0.07*data1$abil_QZ00[i]
  # transformed data
  
  if (data1$Q00_objClarity[i] > 4.5) {
    data1$Q00_objClarity[i] =5
  }
  if (data1$Q00_objClarity[i] >= 4 & data1$Q00_objClarity[i] <=4.5){
    data1$Q00_objClarity[i] = 4
  } 
  if (data1$Q00_objClarity[i] > 3.5 & data1$Q00_objClarity[i] <=4){
    data1$Q00_objClarity[i] = 4
  } 
  if (data1$Q00_objClarity[i] >= 3 & data1$Q00_objClarity[i] <=3.5){
    data1$Q00_objClarity[i] = 3
  } 
  if (data1$Q00_objClarity[i] >= 2.5 & data1$Q00_objClarity[i] <=3){
    data1$Q00_objClarity[i] = 3
  } 
  if (data1$Q00_objClarity[i] >= 2 & data1$Q00_objClarity[i] <=2.5){
    data1$Q00_objClarity[i] = 2
  } 
  if (data1$Q00_objClarity[i] >= 1.5 & data1$Q00_objClarity[i] <=2){
    data1$Q00_objClarity[i] = 2
  } 
  if (data1$Q00_objClarity[i] >= 1 & data1$Q00_objClarity[i] <=1.5){
    data1$Q00_objClarity[i] = 1
  } 
  if (data1$Q00_objClarity[i] >= 0 & data1$Q00_objClarity[i] <=1){
    data1$Q00_objClarity[i] = 1
  } 
  
# Students engagment data genaration
  data1$st_engagment[i] <- 0.1*data1$abil_QZ00[i]-2
  # transformed data
  
  if (data1$st_engagment[i] > 4.5) {
    data1$st_engagment[i] =5
  }
  if (data1$st_engagment[i] >= 4 & data1$st_engagment[i] <=4.5){
    data1$st_engagment[i] = 4
  } 
  if (data1$st_engagment[i] > 3.5 & data1$st_engagment[i] <=4){
    data1$st_engagment[i] = 4
  } 
  if (data1$st_engagment[i] >= 3 & data1$st_engagment[i] <=3.5){
    data1$st_engagment[i] = 3
  } 
  if (data1$st_engagment[i] >= 2.5 & data1$st_engagment[i] <=3){
    data1$st_engagment[i] = 3
  } 
  if (data1$st_engagment[i] >= 2 & data1$st_engagment[i] <=2.5){
    data1$st_engagment[i] = 2
  } 
  if (data1$st_engagment[i] >= 1.5 & data1$st_engagment[i] <=2){
    data1$st_engagment[i] = 2
  } 
  if (data1$st_engagment[i] >= 1 & data1$st_engagment[i] <=1.5){
    data1$st_engagment[i] = 1
  } 
  if (data1$st_engagment[i] >= 0 & data1$st_engagment[i] <=1){
    data1$st_engagment[i] = 1
 }
}

########### Caculate prior for clarity #######

# source("DBDA2E-utilities.R")
# betaABfromMeanSD( mean= 0.6 , sd= 0.2) # a=3, b=2
# x <- seq(0,1,length=100)
# y <- dbeta(x,3,2)
# for(i in 1:length(data1$studentID)) {
#   if (data1$teacher_exp > 5 & data1$course_diff > 8) {
#     data1$clarity[i] <- y[i]
#   }  
# }
clusters1 <- matrix(, nrow = 5, ncol = 2)
colnames(clusters1) <- c("x","y")
orig.data <-select(data1, c(abil_QZ00, Q00_objClarity))
colnames(orig.data)<-c("x","y")

dirichletClusters <- function(orig.data, disp.param = NULL, 
                              max.iter = 100, tolerance = .001)
{
  n <- nrow( orig.data )
  
  data <- orig.data
  pick.clusters <- rep(1, n)
  k <- 1
  
  mu <- matrix( apply(data,2,mean), nrow=1, ncol=ncol(data) )
  colnames(mu) <- c("x", "y")
  
  is.converged <- FALSE
  iteration <- 0
  
  ss.old <- Inf
  ss.curr <- Inf
  
  while ( !is.converged & iteration < max.iter ) { # Iterate until convergence
    iteration <- iteration + 1
    
    for( i in 1:n ) { # Iterate over each observation and measure the distance each observation' from its mean center's squared distance from its mean
      distances <- rep(NA, k)
      
      for( j in 1:k ){
        distances[j] <- sum( (data[i, ] - mu[j, ])^2 ) # Distance formula.
      }
      
      if( min(distances) > disp.param ) { # If the dispersion parameter is still less than the minimum distance then create a new cluster
        k <- k + 1
        pick.clusters[i] <- k
        mu <- rbind(mu, data[i, ])
      } else {
        pick.clusters[i] <- which(distances == min(distances))
      }
      
    }
    
    ##
    # Calculate new cluster means
    ##
    for( j in 1:k ) {
      if( length(pick.clusters == j) > 0 ) {
        mu[j, ] <- apply(subset(data,pick.clusters == j), 2, mean)
      }
    }
    
    ##
    # Test for convergence
    ##
    ss.curr <- 0
    for( i in 1:n ) {
      ss.curr <- ss.curr +
        sum( (data[i, ] - mu[pick.clusters[i], ])^2 )
    }
    ss.diff <- ss.old - ss.curr
    ss.old <- ss.curr
    if( !is.nan( ss.diff ) & ss.diff < tolerance ) {
      is.converged <- TRUE
    }
    
  }
  
  centers <- data.frame(mu)
  ret.val <- list("centers" = centers, "cluster" = factor(pick.clusters),
                  "k" = k, "iterations" = iteration)
  return(ret.val)
  }

ret.val
num.vars <- length(orig.data[1,])
dp.update <- dirichletClusters(orig.data[,1:2], disp.param=50)
clusters1<- as.data.frame(dp.update[['centers']])


######## predicting clarity by looking to clusters of dirichlet clustering  ######################

# sorting clusters1 values by increasing order
clusters1[order(clusters1[,1],clusters1[,2],decreasing=FALSE),]

for(i in 1:nrow(data1)) {
  if (data1[i,2]<= clusters1[1,1]) {
      data1[i,7] = 1
    }  
  if (data1[i,2]> clusters1[1,1] & data1[i,j]<= clusters1[2,1]) {
      data1[i,7] = 2
    }  
  if (data1[i,2]> clusters1[2,1] & data1[i,j]<= clusters1[3,1]) {
      data1[i,7] = 3
    }
  if (data1[i,2]> clusters1[3,1] & data1[i,j]<= clusters1[4,1]) {
      data1[i,7] = 4
    }  
  if (data1[i,2]> clusters1[4,1] & data1[i,j]<= clusters1[5,1]) {
      data1[i,7] = 5
  }
}  



ggplot(orig.data, aes(x = x, y = y)) + geom_point()

data1$clarity_S [data1$clarity <3] = 1 # there may be problem of clarity
data1$clarity_S [data1$clarity >= 3] = 0 # there may be no problem of clarity


################### Graphical model ###################

# First method by sampling with jags
 
myData <- select(data1, clarity_S, studentID)
colnames(myData) <- c("y","s") 

# Load the functions genMCMC, smryMCMC, and plotMCMC:
source("Jags-Ydich-Xnom1subj-MbernBeta.R")

# Generate the MCMC chain:
mcmcCoda = genMCMC( data=myData , numSavedSteps=10000 , saveName=fileNameRoot )

# Display diagnostics of chain, for specified parameters:
parameterNames = varnames(mcmcCoda) # get all parameter names
for ( parName in parameterNames ) {
  diagMCMC( mcmcCoda , parName=parName , 
            saveName=fileNameRoot , saveType=graphFileType )
}

# Get summary statistics of chain:
summaryInfo = smryMCMC( mcmcCoda , compVal=0.5 , rope=c(0.45,0.55) ,
                        saveName=fileNameRoot )
mode1 <- summaryInfo[1,3]
clarity_s <- c(mode1, 1-mode1)

# 0.2806335 probability that there is no problem of clarity 0.7193665


# Second method using graphical model ####
# Calculating CPT
  ########## Ability ##########
data1$abil_QZ00_S[data1$abil_QZ00 <50] = 1 # there may be problem
data1$abil_QZ00_S[data1$abil_QZ00 >= 50] = 0 # there may be no prob

myData <- select(data1, QZ00, studentID)
colnames(myData) <- c("y","s")

# Generate the MCMC chain:
mcmcCoda = genMCMC( data=myData , numSavedSteps=10000 , saveName=fileNameRoot )
# Display diagnostics of chain, for specified parameters:
parameterNames = varnames(mcmcCoda) # get all parameter names
for ( parName in parameterNames ) {
  diagMCMC( mcmcCoda , parName=parName , 
            saveName=fileNameRoot , saveType=graphFileType )
}
# Get summary statistics of chain:
summaryInfo = smryMCMC( mcmcCoda , compVal=0.5 , rope=c(0.45,0.55) ,
                        saveName=fileNameRoot )
mode1 <- summaryInfo[1,3]
abil_QZ00_S <- c(mode1, 1-mode1)

  ########## Objectives clarity ##########
data1$objClarity_S [data1$Q00_objClarity <3] = 1 # there may be problem of clarity
data1$objClarity_S [data1$Q00_objClarity >= 3] = 0 # there may be no problem of clarity

myData <- select(data1, objClarity_S, studentID)
colnames(myData) <- c("y","s")

# Generate the MCMC chain:
mcmcCoda = genMCMC( data=myData , numSavedSteps=10000 , saveName=fileNameRoot )

# Display diagnostics of chain, for specified parameters:
parameterNames = varnames(mcmcCoda) # get all parameter names
for ( parName in parameterNames ) {
  diagMCMC( mcmcCoda , parName=parName , 
            saveName=fileNameRoot , saveType=graphFileType )
}

# Get summary statistics of chain:
summaryInfo = smryMCMC( mcmcCoda , compVal=0.5 , rope=c(0.45,0.55) ,
                        saveName=fileNameRoot )
mode1 <- summaryInfo[1,3]
objClarity_S <- c(mode1, 1-mode1)

  ########## Student engagement ##########
data1$st_engagment_S [data1$st_engagment <3] = 1 # there may be problem of clarity
data1$st_engagment_S [data1$st_engagment >= 3] = 0 # there may be no problem of clarity

myData <- select(data1, st_engagment_S, studentID)
colnames(myData) <- c("y","s")

# Generate the MCMC chain:
mcmcCoda = genMCMC( data=myData , numSavedSteps=10000 , saveName=fileNameRoot )

# Display diagnostics of chain, for specified parameters:
parameterNames = varnames(mcmcCoda) # get all parameter names
for ( parName in parameterNames ) {
  diagMCMC( mcmcCoda , parName=parName , 
            saveName=fileNameRoot , saveType=graphFileType )
}

# Get summary statistics of chain:
summaryInfo = smryMCMC( mcmcCoda , compVal=0.5 , rope=c(0.45,0.55) ,
                        saveName=fileNameRoot )
mode1 <- summaryInfo[1,3]
st_engagment_S <- c(mode1, 1-mode1)




  ########## Clarity ############
library(gRain)
abil_QZ00_S
objClarity_S
st_engagment_S
clarity_s

val = c("no problem","problem")
ability = cptable(~ability, values = abil_QZ00_S, levels = val)
objClarity = cptable(~objClarity|ability, values = c(objClarity_S,abil_QZ00_S) , levels = val)
st_engagment = cptable(~st_engagment|ability, values =c(st_engagment_S,abil_QZ00_S), levels = val)
clarity = cptable(~clarity|objClarity:ability, values = c(0.6,0.4,0.8,0.2,0.27,0.73,0.01,0.99), levels = val)

#plist = compileCPT(list(ability,objClarity,st_engagment,clarity))
#plist

#print(plist$ability)
#print(plist$objClarity)
#print(plist$st_engagment)
#print(plist$clarity)

jtree = grain(plist)
jtree
querygrain(jtree, nodes=c("ability"), type="marginal") = 
querygrain(jtree, nodes=c("clarity"), type="marginal") # We see that the
# obtained here is equal to the first one found by first method


#---------------
source("http://bioconductor.org/biocLite.R")
biocLite()
library(gRbase)
library(gRain)
library(gRbase)
graph <- ug(~ abil_QZ00 + Q00_objClarity:abil_QZ00 + st_engagment:abil_QZ00 + 
              clarity:Q00_objClarity:abil_QZ00)
plot(graph)

dag <- dag("abil_QZ00 + Q00_objClarity:abil_QZ00 + st_engagment:abil_QZ00 + 
              clarity:Q00_objClarity:abil_QZ00")
plot(dag)



val=c("true","false")
F = cptable(~abiliy, values = data1$abil_QZ00 ,levels=val)



# MYSQL
# Connecting to MySQL:

mydb = dbConnect(MySQL(), user='user', password='password', 
                 dbname='database_name', host='host')

# Listing Tables and Fields:

# Listing Tables and Fields:
#Now that a connection has been made we list the tables and 
# fields in the database we connected to.

dbListTables(mydb)

# This will return a list of the tables in our connection. 

dbListFields(mydb, 'some_table')

# This will return a list of the fields in some_table.

# Running Queries:
# Queries can be run using the dbSendQuery function.

dbSendQuery(mydb, 'drop table if exists some_table, some_other_table')

# In my experience with this package any SQL query that will run 
# on MySQL will run using this method.

# Making tables:
#We can create tables in the database using R dataframes.

dbWriteTable(mydb, name='table_name', value=data.frame.name)

# Retrieving data from MySQL:
# To retrieve data from the database we need to save a results set object.

rs = dbSendQuery(mydb, "select * from some_table")

# I believe that the results of this query remain on the MySQL server, to access the results in R we need to use the fetch function.

data = fetch(rs, n=-1)

# This saves the results of the query as a data frame object. 
# The n in the function specifies the number of records to retrieve, 
# using n=-1 retrieves all pending records.













