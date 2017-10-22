############################### Utilities ################################
# Read data coming from the app
# bbb <- seq(as.Date("2017/02/06"), by = "day", 
#          length.out = as.Date("2017/05/15") - as.Date("2017/02/06"))
######### LIBRARY OF QUESTIONS : libeducaper #######
# 1 clarity           libeducaper[[1]][[1:6]][[1]]  
# 2 Learning 
# 3 organisation 
# 4 Group interaction 
# 5 breadth 
# 6 Enthusiasm 
# 7 Good teaching
###### 1 clarity

# 1st element in list clarity
terms <- data.frame(question =character(), prior = integer(), stringsAsFactors=FALSE)
terms[1,1] <- "Fails to define new or unfamiliar terms"
# 2nd element in list clarity
goals_clarity <- data.frame(question =character(), prior = integer(), stringsAsFactors=FALSE)
goals_clarity[1:3,1] <- c("Demonstrated the importance and signifcance of the subject matter",
                          "States objectives of each lecture",
                          "Made it clear how each topic fit into the course")
# 3rd element in list clarity
examples <- data.frame(question =character(), prior = integer(), stringsAsFactors=FALSE)
examples[1:2,1] <- c("Gives several examples of each concept",
                     "Uses concrete everyday examples to explain concepts and principles")
# 4th element in list clarity
pacing <- data.frame(question =character(), prior = integer(), stringsAsFactors=FALSE)
pacing[1:2,1] <- c("Repeats difficult ideas several times",
                   "Stresses most important points by pausing, speaking slowly, raising 
                    voice, and so on")
# 5th element in list clarity
knowledgeable <- data.frame(question =character(), prior = integer(), stringsAsFactors=FALSE)
knowledgeable[1,1] <- "Answers students' questions thoroughly"
# 6th element in list clarity
language <- data.frame(question =character(), prior = integer(), stringsAsFactors=FALSE)
language[1,1] <- "Explains subject matter in familiar colloquial language."
# list clarity
clarity <- list(terms,goals_clarity,examples,pacing,knowledgeable,language)

##### 2 Learning

understanding<- data.frame(question =character(), prior = integer(), stringsAsFactors=FALSE)
understanding[1:2,1] <- c("I have learned and understood the subject materials of this course",
                        "The discussion lectures are useful for deepening my understanding of the material")
application<- data.frame(question =character(), prior = integer(), stringsAsFactors=FALSE)
application[1,1] <- "I increased my ability to apply engineering tools and methods."
real_life_connection<- data.frame(question =character(), prior = integer(), stringsAsFactors=FALSE)
real_life_connection[1,1] <- "I have a greater understanding of how course concepts apply to contemporary problems."
valuable <-data.frame(question =character(), prior = integer(), stringsAsFactors=FALSE)
valuable[1,1] <- "I have learned something which I consider valuable"
interest<- data.frame(question =character(), prior = integer(), stringsAsFactors=FALSE)
interest[1,1] <- "My interest in the subject has increased as a consequence of this course"
stimulation<- data.frame(question =character(), prior = integer(), stringsAsFactors=FALSE)
stimulation[1,1] <- "Introduced stimulating ideas about the subject"

learning <- list(understanding, application, real_life_connection, valuable, interest, stimulation)

##### 3 organisation

cours_objectives<-data.frame(question =character(), prior = integer(), stringsAsFactors=FALSE)
cours_objectives[1,1] <- "Proposed objectives agreed with those actually taught so I knew where the course was going"

revision<- data.frame(question =character(), prior = integer(), stringsAsFactors=FALSE)
revision[1,1] <- "Periodically summarizes points previously made"

topics_fit<- data.frame(question =character(), prior = integer(), stringsAsFactors=FALSE)
topics_fit[1,1] <- "Explains how each topic fits into the course as a whole"

organisation <- list(cours_objectives,revision,topics_fit)

##### 4 Group interaction

participation<-data.frame(question =character(), prior = integer(), stringsAsFactors=FALSE)
participation[1,1] <- "Students were encouraged to participate in class discussions"

ideas_sharing<- data.frame(question =character(), prior = integer(), stringsAsFactors=FALSE)
ideas_sharing[1,1] <- "Students were invited to share their ideas and knowledge"

peer_learning<- data.frame(question =character(), prior = integer(), stringsAsFactors=FALSE)
peer_learning[1,1] <- "Asked students to help each other understand ideas or concepts"

group_interaction <- list(participation,ideas_sharing,peer_learning)

##### 5 breadth

ideas_contrast<-data.frame(question =character(), prior = integer(), stringsAsFactors=FALSE)
ideas_contrast[1,1] <- "Instructor contrasted the implications of various theories"

ideas_background<- data.frame(question =character(), prior = integer(), stringsAsFactors=FALSE)
ideas_background[1,1] <- "Instructor presented the background or origin of ideas/concepts developped in class"

real_life_connection<- data.frame(question =character(), prior = integer(), stringsAsFactors=FALSE)
real_life_connection[1,1] <- "Instructor adequately discussed current developments in the field"

breadth <- list(ideas_contrast,ideas_background,real_life_connection)

##### 6 Enthusiasm

speaking<-data.frame(question =character(), prior = integer(), stringsAsFactors=FALSE)
speaking[1,1] <- "Speaks in a dramatic or expressive way"

mouvement <- data.frame(question =character(), prior = integer(), stringsAsFactors=FALSE)
mouvement[1,1] <- "Walks up aisles beside students"

enthusiasm<- data.frame(question =character(), prior = integer(), stringsAsFactors=FALSE)
enthusiasm[1,1] <- "Instructor was enthusiastic about teaching the course"

enthusiasm <- list(speaking,mouvement,enthusiasm)

##### 7 Good teaching

helping_student<-data.frame(question =character(), prior = integer(), stringsAsFactors=FALSE)
helping_student[1,1] <- "The staff made a real effort to understand difficulties I might be having"

comments <- data.frame(question =character(), prior = integer(), stringsAsFactors=FALSE)
comments[1,1] <- "The staff put a lot of time into commenting on my work"

feedback<- data.frame(question =character(), prior = integer(), stringsAsFactors=FALSE)
feedback[1,1] <- "The teaching staff normally gave me helpful feedback on how I was"

good_teaching <- list(helping_student,comments,feedback)

libeducaper <- list(clarity,learning,organisation,group_interaction,breadth,enthusiasm,good_teaching)
save(libeducaper, file="libeducaper.RData")



         # DONE
######### INDEX BUILDER : index.builder() functions #######
# INDEX BUILDER CHAPTER SCALE
# The data in this function takes : 
# start of the lesson (3 startTheLesson function) output which is in the table
# lessonstartdate in MYSQL data base
index.builder.CHS <- function(mydata){
  ddd <- seq(as.Date(mydata[1,"dateandtime"]), by = "day", length.out = 98)
  index <- data.frame(courseid = character(), day = integer(), week = character(), 
                      date = character(), lecture = logical(), recitation = logical(), 
                      lab = logical(), mean = integer(), sd = integer(),  lecture.est.mean = integer(), 
                      lecture.est.sd = integer(), week.est.mean = integer(), week.est.sd = integer() , 
                      midterm.est.mean = integer(), midterm.est.sd = integer(), stringsAsFactors=FALSE)
  index[["courseid"]] <- mydata[1, "courseid"]      
  for(i in 1:(length(ddd))) {
    index[i,"date"] <- as.character(ddd[i])
  }
  n <- 14
  index[,"day"] <- rep(c(1:7), n)
  a = 1
  b = 7
  for(i in 1:14) {
    index[a:b,"week"] <- i
    a = a+7
    b = b+7
  }
  # This part is hard coded but we can build a function that takes lesson schedule of one week and
  # replicate it for the whole semester
  index$lecture[index$day == 1 | index$day == 3] = 1
  index$lecture[index$day != 1 | index$day != 3] = 0
  index$recitation[index$day == 3] = 1
  index$recitation[index$day != 3] = 0
  index$lab[index$day == 3] = 1
  index$lab[index$day != 3] = 0
  
  require(RMySQL)
  # Connect to educDB on AWS S3
  con <- dbConnect(RMySQL::MySQL(), dbname = "educDB", username = "sayed2", 
                   password = "Educaper90", 
                   host = "educdb.cruj7xzmfriy.eu-central-1.rds.amazonaws.com")
  # Create a dynamic file name
  # if courseid is directly written in the column name ex: CS101
  # ref <- colnames(df, do.NULL = TRUE, prefix = "col")[1] 
  # if the column name is literally "courseid"
  ref <- toString(index[1,"courseid"]) 
  # value <- write.csv(df, paste0("course", ref,".csv"), row.names=F)
  name <-  paste0("chapterScale_", ref)
  dbWriteTable(con, name= name, value= index, row.names = FALSE)
  
}






# INDEX BUILDER MIDTERM SCALE

index.builder.CHS <- function(mydata){
  ddd <- seq(as.Date(mydata[1,"date"],format = "%m/%d/%Y"), by = "day", length.out = 98)
  index <- data.frame(courseid = character(), date = character(), day = integer(), 
                      week = character(), lecture = logical(), recitation = logical(), lab = logical(),
                      lecture.est = integer(), week.est = integer(), midterm.est = integer(),
                      stringsAsFactors=FALSE)
  for(i in 1:(length(ddd))) {
    index[i,"date"] <- as.character(ddd[i])
  }
  n <- 14
  index[,"day"] <- rep(c(1:7), n)
  a = 1
  b = 7
  for(i in 1:14) {
    index[a:b,"week"] <- i
    a = a+7
    b = b+7
  }
  index$lecture[index$day == 1 | index$day == 3] = 1
  index$lecture[index$day != 1 | index$day != 3] = 0
  index$recitation[index$day == 3] = 1
  index$recitation[index$day != 3] = 0
  index$lab[index$day == 3] = 1
  index$lab[index$day != 3] = 0
  
  
  
}

######### CPT ABILITY CLASSIFIER : ability.classifier() #####
# 
ability.classifier  <- function(mydata){ 3 # Addapt the argument to the coming
  # or to the transformed data
  #require(dplyr)
  tocpt <- data.frame(GPA = character(), quiz = character(),
                      homework = character(), labs = character(), problem = integer(),
                      stringsAsFactors=FALSE)
  # "problem" column means if there maybe be a problem
  # to be searched.
  for(i in 1:nrow(mydata)) {
    ## GPA
    if (!is.na(mydata[i,"GPA"])) {
      if (mydata[i,"GPA"] >= 70) {
        tocpt[i,"GPA"] = "high"
      }
      if (mydata[i,"GPA"] < 70 & mydata[i,"GPA"] >= 50) {
        tocpt[i,"GPA"] = "average"
      }
      if (mydata[i,"GPA"] < 50) {
        tocpt[i,"GPA"] = "low"
      }
    } else {
      tocpt[i,"GPA"] = NA
    }
  }
  ## quiz
  for(i in 1:nrow(mydata)) {
    if (!is.na(mydata[i,"quiz"])) {
      if (mydata[i,"quiz"] >= 70) {
        tocpt[i,"quiz"] = "high"
      }
      if (mydata[i,"quiz"] < 70 & mydata[i,"quiz"] >= 50) {
        tocpt[i,"quiz"] = "average"
      }
      if (mydata[i,"quiz"] < 50) {
        tocpt[i,"quiz"] = "low"
      }
    } else {
      tocpt[i,"quiz"] = NA
    }
  }
  ## home work
  for(i in 1:nrow(mydata)) {
    if (!is.na(mydata[i,"homework"])) {
      if (mydata[i,"homework"] >= 70) {
        tocpt[i,"homework"] = "high"
      } 
      if (mydata[i,"homework"] < 70 & mydata[i,"homework"] >= 50) {
        tocpt[i,"homework"] = "average"
      } 
      if (mydata[i,"homework"] < 50) {
        tocpt[i,"homework"] = "low"
      }
    } else {
      tocpt[i,"homework"] = NA
    }
  }
  ## labs
  for(i in 1:nrow(mydata)) {
    if (!is.na(mydata[i,"labs"])) {
      if (mydata[i,"labs"] >= 70) {
        tocpt[i,"labs"] = "high"
      }
      if (mydata[i,"labs"] < 70 & mydata[i,"labs"] >= 50) {
        tocpt[i,"labs"] = "average"
      }
      if (mydata[i,"labs"] < 50) {
        tocpt[i,"labs"] = "low"
      }
    } else {
      tocpt[i,"labs"] = NA
    }
  }
  for (i in 1:nrow(tocpt)) { # if this part of code wont work check missin values
    countt = 0
    for(j in 1:(ncol(tocpt)-1)) {
      if (!is.na(tocpt[i,j]) == TRUE & tocpt[i,j] == "high") {
        countt = countt + 1
      } else {
        countt = countt
      }
    }
    if (countt >=2) {
      tocpt[i,"problem"] = 0
    } else {
      tocpt[i,"problem"] = 1
    }
    #require(gRbase, Rgraphiz, plyr)
    #edges <- list(GPA = list(), quiz = list(), 
    #              homework = list(), labs = list(), problem = list(1:4))
    #g <- graphNEL(nodes=names(tocpt), edgeL=edges,edgemod="directed")
    #learn(g,tocpt)
  }
   make_cpt<-function(df,pa) {
    prob <- nrow(df)
    parents <- data.frame(df[1,pa])
    names(parents) <- pa
    
    data.frame(parents,prob)
  }
   learn <- function(g,data) {
    rg <- reverseEdgeDirections(g)
    result <- list()
    
    for(var in rg@nodes)
    {
      pa <- unlist(adj(rg,var))
      if(length(pa)>0)
      {
        X <- ddply(data, c(var,pa), make_cpt, pa)
        Y <- ddply(data, pa, make_cpt, pa)
        for(i in 1:nrow(Y))
        {
          c <- sapply(1:nrow(X), function(j) all(X[j,pa] == Y[i,pa]))
          c <- which(c)
          X$prob[c] <- X$prob[c]/Y$prob[i]
        }
      }
      else
      {
        X <- ddply(data,var, function(df) c(prob=nrow(df)))
        X$prob <- X$prob/sum(X$prob)
      }
      
      result[[length(result)+1]] <- X
    }
    
    return(result)
  } # learn CPT tables
   require(gRbase, Rgraphiz, plyr)
   edges <- list(GPA = list(), quiz = list(), 
                homework = list(), labs = list(), problem = list(1:4))
   g <- graphNEL(nodes=names(tocpt), edgeL=edges,edgemod="directed")
   p <- learn(g,tocpt)
   return(p[[5]])
}

######### CPT ENGAGEMENT CLASSIFIER ####

######### CPT INSTRUCTOR EXPERIENCE #####
# The data this function takes : Data about course GENERAL data
# Instructor experience intervals : [0,6] bad, ]6,20]good, ]20,..[ bad
cpt.texp <- function(mydata) {
  cpt_texp = data.frame(problem = logical(), prob = integer(), 
                        stringsAsFactors = FALSE)
  cpt_texp[1:2,] = c(0,1) 
  if(mydata[1,"Instructor_experience"] <= 6) {
    cpt_texp[1,2] = 0.4 # to plug the prior this gonna be the 
                        # mode of posterior of problem = 0
    cpt_texp[2,2] = 1- cpt_texp[1,2]
  }
  if(mydata[1,"Instructor_experience"] > 6 & mydata[1,"Instructor_experience"] <= 20) {
    cpt_texp[1,2] = 0.6 # to plug the prior this gonna be the 
    # mode of posterior of problem = 0
    cpt_texp[2,2] = 1- cpt_texp[1,2]
  }
  if(mydata[1,"Instructor_experience"] > 20) {
    cpt_texp[1,2] = 0.4 # to plug the prior this gonna be the 
    # mode of posterior of problem = 0
    cpt_texp[2,2] = 1- cpt_texp[1,2]
  }
  return(cpt_texp)
}

######### CPT COURSE #####
# The data this function takes : Data about course GENERAL data
cpt.course <- function(mydata) {
  # Domain
  if (mydata[,"domain"] == "humanities") {
    cpt_domain = data.frame(prob0 = integer(), prob1= integer(), stringsAsFactors = FALSE)
    cpt_domain[1,1] = 0.6      # to plug the prior this gonna be the 
                               # mode of posterior of "problem = 0"
    cpt_domain[1,2] = 1- cpt_domain[1,1]
    
    cpt_domain[2,1] = 1- cpt_domain[1,1]     
    cpt_domain[2,2] = cpt_domain[1,1]
  } else {
    cpt_domain = data.frame(prob0 = integer(), prob1= integer(), stringsAsFactors = FALSE)
    cpt_domain[1,1] = 0.4      # to plug the prior this gonna be the 
                               # mode of posterior of "problem = 0"
    cpt_domain[1,2] = 1- cpt_domain[1,1]
    
    cpt_domain[2,1] = 1- cpt_domain[1,1]     
    cpt_domain[2,2] = cpt_domain[1,1]
  }
  # Course Type 
  if (mydata[,"course_type"] == 1 | mydata[,"course_type"] == 2) {
    cpt_course_type = data.frame(prob0 = integer(), prob1= integer(), stringsAsFactors = FALSE)
    cpt_course_type[1,1] = 0.2      # to plug the prior this gonna be the 
                                    # mode of posterior of "problem = 0"
    cpt_course_type[1,2] = 1- cpt_course_type[1,1]
    
    cpt_course_type[2,1] = 1- cpt_course_type[1,1]     
    cpt_course_type[2,2] = cpt_course_type[1,1]
  } else {
    cpt_course_type = data.frame(prob0 = integer(), prob1= integer(), stringsAsFactors = FALSE)
    cpt_course_type[1,1] = 0.6 # to plug the prior this gonna be the 
                               # mode of posterior of "problem = 0"
    cpt_course_type[1,2] = 1- cpt_course_type[1,1]
    
    cpt_course_type[2,1] = 1- cpt_course_type[1,1]     
    cpt_course_type[2,2] = cpt_course_type[1,1]
  }  
  # Class size
  if (ncol(mydata) < 50) {
    cpt_class_size = data.frame(prob0 = integer(), prob1= integer(), stringsAsFactors = FALSE)
    cpt_class_size[1,1] = 0.6      # to plug the prior this gonna be the 
                               # mode of posterior of "problem = 0"
    cpt_class_size[1,2] = 1- cpt_class_size[1,1]
    
    cpt_class_size[2,1] = 1- cpt_class_size[1,1]     
    cpt_class_size[2,2] = cpt_class_size[1,1]
    
  } else {
    cpt_class_size = data.frame(prob0 = integer(), prob1= integer(), stringsAsFactors = FALSE)
    cpt_class_size[1,1] = 0.4 # to plug the prior this gonna be the 
                               # mode of posterior of "problem = 0"
    cpt_class_size[1,2] = 1- cpt_class_size[1,1]
    
    cpt_class_size[2,1] = 1- cpt_class_size[1,1]     
    cpt_class_size[2,2] = cpt_class_size[1,1]
  }  
  # Course difficulty
  if (mydata[,"course_type"] < 4) {
    cpt_course_diff = data.frame(prob0 = integer(), prob1= integer(), stringsAsFactors = FALSE)
    cpt_course_diff[1,1] = 0.7 # to plug the prior this gonna be the 
                               # mode of posterior of "problem = 0"
    cpt_course_diff[1,2] = 1- cpt_course_diff[1,1]
    
    cpt_course_diff[2,1] = 1- cpt_course_diff[1,1]     
    cpt_course_diff[2,2] = cpt_course_diff[1,1]
  }
  if (mydata[,"course_type"] >= 4 & mydata[,"course_type"] <= 7) {
    cpt_course_diff = data.frame(prob0 = integer(), prob1= integer(), stringsAsFactors = FALSE)
    cpt_course_diff[1,1] = 0.6 # to plug the prior this gonna be the 
                               # mode of posterior of "problem = 0"
    cpt_course_diff[1,2] = 1- cpt_course_diff[1,1]
    
    cpt_course_diff[2,1] = 1- cpt_course_diff[1,1]     
    cpt_course_diff[2,2] = cpt_course_diff[1,1]
  } else {
    cpt_course_diff = data.frame(prob0 = integer(), prob1= integer(), stringsAsFactors = FALSE)
    cpt_course_diff[1,1] = 0.4 # to plug the prior this gonna be the 
                               # mode of posterior of "problem = 0"
    cpt_course_diff[1,2] = 1- cpt_course_diff[1,1]
    
    cpt_course_diff[2,1] = 1- cpt_course_diff[1,1]     
    cpt_course_diff[2,2] = cpt_course_diff[1,1]
  }
  # 
  require(gRain)
  require(gRbase)
  val = c(0,1)

  domain = cptable(~domain , 
                   values = as.numeric(c(cpt_domain[1,1],cpt_domain[1,2])) , levels = val)
  course_type = cptable(~course_type, 
                        values = as.numeric(c(cpt_course_type[1,1],cpt_course_type[1,2])), levels = val)
  class_size = cptable(~class_size , 
                       values = as.numeric(c(cpt_class_size[1,1],cpt_class_size[1,2])), levels = val)
  course_diff =  cptable(~course_diff, 
                         values = as.numeric(c(cpt_course_diff[1,1],cpt_course_diff[1,2])), levels = val)
  
  prob = cptable(~prob|domain:course_type:class_size:course_diff, 
                 values = c(0.8,0.2,0.6,0.4,0.6,0.4,0.5,0.5,
                            0.6,0.4,0.5,0.5,0.5,0.5,0.3,0.7,
                            0.4,0.6,0.5,0.5,0.5,0.5,0.3,0.7,
                            0.5,0.5,0.3,0.7,0.3,0.7,0.2,0.8), levels = val)
  # as.numeric(c(cpt_domain[1,1:2],cpt_domain[2,1:2],
  #cpt_course_type[1,1:2],cpt_course_type[2,1:2],
  #cpt_class_size[1,1:2],cpt_class_size[2,1:2],
  #cpt_course_diff[1,1:2],cpt_course_diff[2,1:2]))
  plist = compileCPT(list(domain,course_type,class_size,course_diff,prob))
  jtree = grain(plist)
  cpt_prob = as.data.frame(querygrain(jtree12, nodes=c("class_size"), type="marginal")) # take this
  # probalitiy after plugin the prior of the PRIOR LOOP alongside with 
  # the index, accuracy cpt_prob[]
  return(cpt_prob)
  
  # Just to learn part 
  
  texp = cptable(~texp , values = as.numeric(c(0.6,0.4)) , levels = val)
  course = cptable(~course,values = as.numeric(c(0.4,0.6)), levels = val)
  ability = cptable(~ability ,values = as.numeric(c(0.4,0.6)), levels = val)
  prob = cptable(~prob|texp:course:ability, 
                 values = c(0.8,0.2,0.4,0.6,0.4,0.6,0.3,0.7,
                            0.6,0.4,0.3,0.7,0.3,0.7,0.2,0.8), levels = val) # 16 values
  question =  cptable(~question|prob,values = as.numeric(c(0.5,0.5,0.5,0.5)), levels = val)
  
  
  
  plist = compileCPT(list(texp,course,ability,prob,question))
  jtree = grain(plist)
  cpt_prob = as.data.frame(querygrain(jtree6, nodes=c("prob"), type="marginal"))
  
  jtree6 <- setEvidence(jtree5, evidence=list(domain=c("1"), class_size =c("0"),
                                              course_type=c("1"),course_diff=c("0") ))
  jtree5 <- setEvidence(jtree4, evidence=list(prob=c("0")))
  cpt_prob
}



######## HERACHICAL DIAGNOSTIC AND ESTIMATION OF THE PROBLEM #########
######### CHAPTER SCALE CHS ####
index <- index.builder(mydata = )

problem.ExplorerCHS <- function(mydata){
  # Get the date to search for the index necessary to
  # find the specific day in order to see probable problems
  date <- as.character(as.Date(mydata[1,"date"],format = "%m/%d/%Y"))
  # course.diff <- mydata[1, "course difficulty"]
  # grade <- mydata[1, "grade"]
  # index <- index.builder(mydata) # Should be used outside the function
  current.index <- which(index[] == date) 
  day <- index[current.index,"day"]  # with this index you can find
  # specific column in  potential 
  # problems table
  week <- index[current.index, "week"] 
  
  if (week == 1) {
    begin.course <- TRUE
    if(day == 1) {
      begin.chapter <- TRUE
      
    }
  }
  
}

######### MIDTERM SCALE MIS ####


######### COURSE SCALE COS #####















chapter.scale <- data.frame(lecture1 =) 

##### 
courses <- function(mydata) {
  require(jags, coda, runjags)
  domain <- mydata
  course.type <-   # as.factor()  
  courses2017 <- data.frame(domain = character(), course.type = character(), 
                            class.size =  integer(), course.diff =integer(),
                            stringsAsFactors=FALSE)
}



index <- index.builder()

problemExplore <- function(mydata){
  # Get the date to search for the index necessary to
  # find the specific day in order to see probable problems
  date <- as.character(as.Date(mydata[1,"date"],format = "%m/%d/%Y"))
  # course.diff <- mydata[1, "course difficulty"]
  # grade <- mydata[1, "grade"]
  # index <- index.builder(mydata) # Should be used outside the function
  current.index <- which(index[] == date) 
  day <- index[current.index,"day"]  # with this index you can find
                                     # specific column in  potential 
                                     # problems table
  week <- index[current.index, "week"] 
  
  if (week == 1) {
    begin.course <- TRUE
    if(day == 1) {
      begin.chapter <- TRUE
      
    }
  }
  
}

make_cpt<-function(df,pa) {
  prob <- nrow(df)
  parents <- data.frame(df[1,pa])
  names(parents) <- pa
  
  data.frame(parents,prob)
}
learn <- function(g,data) {
  rg <- reverseEdgeDirections(g)
  result <- list()
  
  for(var in rg@nodes)
  {
    pa <- unlist(adj(rg,var))
    if(length(pa)>0)
    {
      X <- ddply(data, c(var,pa), make_cpt, pa)
      Y <- ddply(data, pa, make_cpt, pa)
      for(i in 1:nrow(Y))
      {
        c <- sapply(1:nrow(X), function(j) all(X[j,pa] == Y[i,pa]))
        c <- which(c)
        X$prob[c] <- X$prob[c]/Y$prob[i]
      }
    }
    else
    {
      X <- ddply(data,var, function(df) c(prob=nrow(df)))
      X$prob <- X$prob/sum(X$prob)
    }
    
    result[[length(result)+1]] <- X
  }
  
  return(result)
} # learn CPT tables


# Test ability.classifier ####
#GPA <- sample(50:100, 100, replace = TRUE)
#quiz <- sample(30:100, 100, replace = TRUE)
#homework <- sample(40:100, 100, replace = TRUE)
#labs <- sample(50:100, 100, replace = TRUE)
#problem <- vector(length = 100L)
#datacpt <- data.frame(GPA, quiz, homework,labs, problem)
#tocpt <- ability.classifier(datacpt)
#require(gRbase, Rgraphiz, plyr)
#edges <- list(GPA = list(), quiz = list(), 
#             homework = list(), labs = list(), problem = list(1:4))
#g <- graphNEL(nodes=names(tocpt), edgeL=edges,edgemod="directed")
#p <- learn(g,tocpt)


