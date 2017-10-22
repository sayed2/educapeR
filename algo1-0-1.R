
## DATA GENERATION MODEL 1.0.1 CLARITY ####
# 32 classroom each of 25 students having Physics 101 course
data101 <- matrix(data = NA, nrow = 800, ncol = 10)
colnames(data101) <- c("course_name","section_id","texp","date",
                       "student_id", "GPA", "QZ00","QZ01","Q01", "clarity")
# we gonnq add later "domain", "course_type", "class_Size", "course_difficulty", "home_Work", "labs", "exam", "lms_activity", "attendance"
data101 <- as.data.frame(data101)
data101$student_id <- 1:nrow(data101) 
data101$course_name <- "Physics101"
# 
data101$section_id[data101$student_id < 201] = 1
data101$section_id[data101$student_id > 200 & data101$student_id < 401] = 2
data101$section_id[data101$student_id > 400 & data101$student_id < 601] = 3
data101$section_id[data101$student_id > 600 & data101$student_id < 801] = 4
# Teacher experience
data101$texp[data101$student_id < 201] = 3
data101$texp[data101$student_id > 200 & data101$student_id < 401] = 7
data101$texp[data101$student_id > 400 & data101$student_id < 601] = 12
data101$texp[data101$student_id > 600 & data101$student_id < 801] = 20
# datee data generation
data101$date[data101$student_id < 201] = "09/05/2016"
data101$date[data101$student_id > 200 & data101$student_id < 401] = "09/06/2016"
data101$date[data101$student_id > 400 & data101$student_id < 601] = "09/07/2016"
data101$date[data101$student_id > 600 & data101$student_id < 801] = "09/08/2016"

# data101$date <- as.Date(data101[,4],format = "%m/%d/%Y") 
# data101$date <- as.character(data101[,4])
# Simulate GPA data from our predifined model
set.seed(100)
txtstring <- '
data{
        for(i in 1:N) {
        Simulated[i] ~ dt(mu[i] , 1/sigma^2 , nu)
        mu[i] ~ dnorm(2.5, 1/1^2) # JAGS uses precision sigma2 = 1
    }
    sigma ~ dunif(0,1)
    nu = 1
}

model{
    fake <- 0
}
#monitor# Simulated
#data# N
'
library(runjags)

N <- nrow(data101)
Simulated <- coda::as.mcmc(run.jags(txtstring, sample=1, n.chains=1, summarise=FALSE))
Simulated
str(Simulated)
# Data cleaning (GPA)
for (i in 1:nrow(data101)) {
  data101[i,6] <- Simulated[i]
}

for (i in 1:nrow(data101)) {
if (data101[i,6] > 4) {
  data101[i,6] = 4
} 
if (data101[i,6] < 0) {
  data101[i,6] = 0
}
}
for (i in 1:nrow(data101)) {
  data101[i,6] <- data101[i,6]*25
}

# Generate QZ00 (ability in the future applications) data from our predifined model

for (i in 1:nrow(data101)) {
  data101[i,7] <- 25 +0.6*data101[i,6]+0.5*data101[i,3]
}





###########################################################################
###########################################################################
###########################################################################
# Prior generation for clarity based on a predifined model


txtstring <- '
data {
  for ( j in 6:7 ) {
    xm[j-5] <- mean(data101[,j])
    xsd[j-5] <- sd(data101[,j])
    for ( i in 1:N ) {
      zx[i,j-5] <- ( data101[i,j] - xm[j-5] ) / xsd[j-5]
    }
  }
  for ( i in 1:N ) {
    # In JAGS, ilogit is logistic:
    Simulated[i] ˜ dbern( ilogit( zbeta0 + zbeta[1] * zx[i,1] + zbeta[2] * zx[i,2] ) )
  }
  # Priors vague on standardized scale:
  zbeta0 ˜ dnorm( 0 , 1/2ˆ2 )
  for ( j in 1:2 ) {
    zbeta[j] ˜ dnorm( 0 , 1/2ˆ2 )
  }
  # Transform to original scale:
  beta[1:2] <- zbeta[1:2] / xsd[1:2]
  beta0 <- zbeta0 - sum( zbeta[1:2] * xm[1:2] / xsd[1:2] )
 

model {
  fake <- 0
}
#monitor# Simulated
#data# N
'
library(runjags)

N <- nrow(data101)
Simulated <- coda::as.mcmc(run.jags(txtstring, sample=1, n.chains=1, summarise=FALSE))
Simulated
str(Simulated)
# Data cleaning (GPA)
for (i in 1:N) {
  data101[i,10] <- Simulated[i]  
}

data101$clarityprob [data101$clarity <2.5] = 1 # there may be problem of clarity
data101$clarityprob [data101$clarity >= 2.5] = 0 # there may be no problem of clarity