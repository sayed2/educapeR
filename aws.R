################ getNewCourse function - FIRST CALL OF API ###############




# DONE
# 1 getNewCourse function - FIRST CALL OF API
# courseid:  course.code,
# name:  course.name,
# department:course.dep,
# grade : course.grad,
# credit: course.credit
getNewCourse <- function(data = '') { # The URL must be string
  # Get the JSON data an coerce it to a data frame
  require(opencpu)
  require(jsonlite)
  json_data = fromJSON(data)
  df = as.data.frame(json_data)
  
  
  require(RMySQL)
  # Connect to educDB on AWS S3
  con <- dbConnect(RMySQL::MySQL(), dbname = "educDB", username = "sayed2", password = "Educaper90", host = "educdb.cruj7xzmfriy.eu-central-1.rds.amazonaws.com")
  dbWriteTable(con, name= "getNewCourse", value= df, append = TRUE, overwrite = FALSE, row.names = FALSE)
}



# DONE
# 2 getCourseParticipants function
# courseid:  course.code,
# studentid : student, // array
# gpa:gpa //random
# PRIMARY KEY (courseid, studentid)
getCourseParticipants <- function(data ='') {
  library(jsonlite)
  json_data = fromJSON(data)
  
  df <- as.data.frame(json_data)
  colnames(df) <- c("courseid", "studentid", "gpa")
  # Connect to educDB on AWS S3
  require(RMySQL)
  con <- dbConnect(RMySQL::MySQL(), dbname = "educDB", username = "sayed2",
                   password = "Educaper90",
                   host = "educdb.cruj7xzmfriy.eu-central-1.rds.amazonaws.com")
  ref0 <- toString(df[1,1])
  # Check if the table of enrolled students exists or not
  # If it exist add the new data point to the table
  name <- paste0("students_", ref0) # name of the table in educDB
  # dbWriteTable(con, name= name, value= df, row.names = FALSE)
  name <- "students_CS155"
  # If The app gonna send students in seperates points in time we use this part:
  
  # Listing Tables
  tables_list <- dbListTables(con)
  check <- name %in% tables_list
  if (check == TRUE) {
    dbWriteTable(con, name= name, value= df,   
                 append = TRUE, overwrite = FALSE, row.names = FALSE)
  } else {
    dbSendQuery(con, paste0("CREATE TABLE"," ",name, "
                            (courseid varchar(45),
                            studentid varchar(45),
                            gpa int(11),
                            PRIMARY KEY (courseid, studentid)
                            )"))
  }
  }







# DONE
# 3 startTheLesson function
# courseid:  course.code
# dateandtime: dateandtime

startTheLesson <- function(data) {
  require(opencpu)
  library(jsonlite)
  require(RMySQL)
  json_data = fromJSON(data)
  df <- as.data.frame(json_data)
  colnames(df) <- c("courseid", "dateandtime")
  # Connect to educDB on AWS S3
  con <- dbConnect(RMySQL::MySQL(), dbname = "educDB", username = "sayed2", password = "Educaper90",
                   host = "educdb.cruj7xzmfriy.eu-central-1.rds.amazonaws.com")
  dbWriteTable(con, name= "lessonstartdate", value= df, append = TRUE, overwrite = FALSE, row.names = FALSE)
}








################################################################################
# 4 diagnosis  end of lesson
# courseid:  course.code,
# date :  date,
# attendance: studentsattended,
# homework: number,
# lab: labnote,
# quiz: quiznote
# Primary key (courseid, date)
diagnosis <- function(data ='') { # This function has output
  require(opencpu)
  library(jsonlite)
  require(RMySQL)
  json_data = fromJSON(data)
  #  leng <- as.numeric()
  #  for(i in 1:length(json_data)) {
  #    leng[i] <-length(json_data[[i]])
  #  }
  #  min <- min(leng)
  #  max <- max(leng)
  #  for(j in 1:length(json_data)) {
  #    if(length(json_data[[j]]) < max) {
  #      for(i in (length(json_data[[j]])+1):max)
  #       json_data[[j]][[i]] <- 0
  #    }
  #  }
  df <- as.data.frame(json_data)
  df$item <- NULL
  colnames(df) <- c("courseid","date","attendance","homework","lab","quiz", "item" )
  # Connect to educDB on AWS S3
  con <- dbConnect(RMySQL::MySQL(), dbname = "educDB", username = "sayed2",
                   password = "Educaper90",
                   host = "educdb.cruj7xzmfriy.eu-central-1.rds.amazonaws.com")
  
  dbWriteTable(con, name= "diagnosis", value= df, nrows = nrow(df) , row.names = FALSE,
               overwrite = FALSE, append = 1)
  # Algorithm ##################################################################
  
  
  
  
  
  
  ##############################################################################
  # Data to be returned
  # output<- data.frame(matrix(0, ncol = 2, nrow = nrow(df)))
  # colnames(output) <- c("courseid", "studentid", "question")
  
  
  # Data should be sent to data base as well for subsquent estimations (used by function 5)
  # "courseid", "date","studentid", "question"
  output_meta <- output
  for(i in 1:nrow(df)) {
    for(j in 1:nrow(output_meta)) {
      if(df[i,"attendance"] == output_meta$studentid[j]) {
        output_meta$date[j] <- df$date[i]
      }
    }
  }
  for(i in 1:nrow(output_meta)) {
    dbSendQuery(con, paste0("UPDATE diagnosis SET item =","'",output_meta[i,3],"'"," ","WHERE date =","'",toString(output_meta[i,4]),"'",";"))
  }
  
  
  # for testing
  n <- nrow(df)
  output$courseid <- df[1,1]
  output$studentid <- df[1:n,"attendance"]
  output$question <- 'How was the lesson?'
  #
  json_output <- toJSON(output, dataframe = 'columns', raw = 'base64')
  # json_data
  json_output
  
  #{"courseid" : "CS101", "studentid" : ["11111A","22222A","33333A"] , "question" : "How was the lesson?" }
}

# 5 getNewStudentFeed function : The prediction function
# courseid:  course.code,
# studentid:studentid,
# question:  text,
# feedicon: number
# time

getNewStudentFeed <- function(data = "") {
  require(opencpu)
  library(jsonlite)
  json_data = fromJSON(data)
  df <- as.data.frame(json_data)
  date <- as.Date(df[1,"time"])
  # Connect to educDB on AWS S3
  require(RMySQL)
  con <- dbConnect(RMySQL::MySQL(), dbname = "educDB", username = "sayed2",
                   password = "Educaper90",
                   host = "educdb.cruj7xzmfriy.eu-central-1.rds.amazonaws.com")
  
  # Send data coming one by one to educDB:
  ref0 <- toString(df[1,1])
  # Check if the table of enrolled students exists or not
  # If it exist add the new data point to the table
  name <- paste0("students_", ref0) # name of the table in educDB
  # If The app gonna send students in seperates points in time we use this part:
  # Do
  # Listing Tables
  tables_list <- dbListTables(con)
  check <- name %in% tables_list
  count = as.numeric()
  if (check == TRUE) {
    dbWriteTable(con, name= name, value= df,
                 append = TRUE, overwrite = FALSE, row.names = FALSE)
    count <- 2
    return(count)
  } else {
    query <- paste0("CREATE TABLE"," ",name, "
                    (courseid varchar(45),
                    studentid varchar(45),
                    question int(11),
                    feedicon varchar(45),
                    time varchar(45),
                    PRIMARY KEY (courseid, studentid, time)
                    )")
    dbSendQuery(con, query) 
    dbWriteTable(con, name= name, value= df, nrows = nrow(df) , row.names = FALSE,
                 overwrite = FALSE, append = 1)
    count <- 1
    return(count)
  }
  
  #### While loop come here
  
  
  # Retrieve the table if it exist:
  time <- toString(as.Date(Sys.time()))
  courseid <- toString(df[1,"courseid"])
  
  table0 = paste0("select * FROM"," ",name,"WHERE time LIKE"," ",time)
  rs = dbSendQuery(educDB, table0)
  data0 = fetch(rs, n=-1)
  
  # Retrieve data sent as push notification from diagnosis table:
  table0 = paste0("select * FROM"," ","diagnosis","WHERE time LIKE"," ",time," ","AND courseid ="," ",courseid)
  rs = dbSendQuery(educDB, table0)
  data_push <- fetch(rs, n=-1)
  
  
  endtime = Sys.time() + 10800   # Cental europe area we add 10800 second to get turkey time
  lastfeed = min(data0[,"time"])
  margin = endtime - lastfeed
  if ( margin >= 3 ) {
    # Run the function that make the estimation 
    
    # return
    output
    # Send output to Firebase (it contains curl to Firebase)
    
    # Send output to data base
    dbWriteTable(con, name = "report", value = output, nrows = nrow(output) , row.names = FALSE,
                 overwrite = FALSE, append = 1)
    count = 3  # arbitrary value to end the while loop
  }
}
# Data to be returned
output <- data.frame(courseid = character(), time = character(), index = character(), accuracy = integer(),
                     percentage = integer() , stringsAsFactors = FALSE)
# for testing
output[1,1] <- 'CS101'
output[1,2] <- 'Clarity'
output[1,3] <- 60
output[1,4] <- 70
#
json_output <- toJSON(output, dataframe = 'columns', raw = 'base64')
json_output


  }











df <- data.frame(courseid = character(), date = character(), attendance = character(),
                 homework = integer(), lab = integer(), quiz = integer(), stringsAsFactors = FALSE)
df[1:3,1] <- "CS120"
df[1,2] <- "2017-01-15 15:15:00"
df[2,2] <- "2017-01-15 15:15:15"
df[3,2] <- "2017-01-15 15:15:20"
df[1,3] <- "ABC17000"
df[2,3] <- "ABC17500"
df[3,3] <- "ABC17600"

# courseid:  course.code,
# date :  date,
# attendance: studentsattended,
# homework: number,
# lab: labnote,
# quiz: quiznote

#########################################################################################

output<- data.frame(matrix(0, ncol = 3, nrow = nrow(df)-1))
colnames(output) <- c("courseid", "studentid", "question")
output[1:2,1] <- "CS120"

output[1,2] <- "ABC17000"
output[2,2] <- "ABC17500"

output[1,3] <- "How was the lesson"
output[2,3] <- "How was the lesson"

output



