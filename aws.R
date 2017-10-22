################ getNewCourse function - FIRST CALL OF API ###############




# DONE
# 1 getNewCourse function - FIRST CALL OF API
# courseid:  course.code,
# name:  course.name,
# department:course.dep,
# grade : course.grad,
# credit: course.credit
# dateandtime: dateandtime
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
                    print(json_data)
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
                    # If The app gonna send students in seperates points in time we use this part:
                    
                    # Listing Tables
                    tables_list <- dbListTables(con)
                    check <- name %in% tables_list
                    if (check == TRUE) {
                                        dbWriteTable(con, name= name, value= df,
                                                     append = TRUE, overwrite = FALSE, row.names = FALSE)
                    } else {
                                        dbSendQuery(con, paste0("CREATE TABLE"," ",name," ","(courseid varchar(45), studentid varchar(45), gpa int(11), PRIMARY KEY (courseid, studentid));"))
                                        dbWriteTable(con, name= name, value= df, append = TRUE, overwrite = FALSE, row.names = FALSE)
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
                    colnames(df) <- c("courseid","date","attendance","homework","lab","quiz")
                    df$item <- as.character(45)
                    df$item <- "item"
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
                    library(dplyr)
                    library(magrittr)
                    output <- df %>% dplyr::select(courseid, date, attendance)
                    colnames(output) <- c("courseid","date","studentid")
                    output$question <- "How are you mounir?"
                    #output <- output[1:3,]
                    
                    # Data should be sent to data base as well for subsquent estimations (used by function 5)
                    # "courseid", "date","studentid", "question"
                    output_meta <- output
                    for(i in 1:nrow(output_meta)) {
                                        query <- paste0("UPDATE diagnosis SET item ="," ","'",output_meta[i,"question"],"'"," ","WHERE date ="," ","'",output_meta[i,"date"],"'"," ","and attendance ="," ","'",output_meta[i,"studentid"],"'",";")
                                        dbSendQuery(con, query)
                    }
                    
                    
                    # for testing
                    #
                    output$date <- NULL
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
                    con <- dbConnect(RMySQL::MySQL(), dbname = "educDB", username = "sayed2", password = "Educaper90", host = "educdb.cruj7xzmfriy.eu-central-1.rds.amazonaws.com")
                    
                    # Send data coming one by one to educDB:
                    ref0 <- toString(df[1,1])
                    # Check if the table of enrolled students exists or not
                    # If it exist add the new data point to the table
                    name <- paste0("studentFeed_", ref0) # name of the table in educDB
                    # If The app gonna send students in seperates points in time we use this part:
                    # Do
                    # Listing Tables
                    tables_list <- dbListTables(con)
                    check <- name %in% tables_list
                    count = as.numeric()
                    if (check == TRUE) {
                                        dbWriteTable(con, name= name, value= df, append = TRUE, overwrite = FALSE, row.names = FALSE)
                                        print("added !")
                                        count <- 33333
                    } else {
                                        query <- paste0("CREATE TABLE"," ",name, "(courseid varchar(45), studentid varchar(45), question varchar(45), feedicon varchar(45), time varchar(45), PRIMARY KEY (courseid, studentid, time))")
                                        dbSendQuery(con, query)
                                        dbWriteTable(con, name= name, value= df, nrows = nrow(df) , row.names = FALSE, overwrite = FALSE, append = 1)
                                        count <- 1
                                        firstfeedtime <- Sys.time() + 10800
                                        assign("count", 1, envir = .GlobalEnv)
                                        
                    }
                    # Cental europe area we add 10800 second to get turkey time
                    while( count == 1) {
                                        # WE wont retrieve table:
                                        time <- toString(as.Date(Sys.time()+10800))
                                        # courseid <- toString(df[1,"courseid"]) # we are supposing that courseid is just one so we wont use it in sql query
                                        
                                        table0 = paste0("select * FROM"," ",name," ","WHERE time LIKE"," ","'",time,"'",";")
                                        rs = dbSendQuery(con, table0)
                                        data0 = fetch(rs, n=-1)
                                        
                                        # Retrieve data sent as push notification from diagnosis table:
                                        
                                        #table0 = paste0("select * FROM diagnosis WHERE date LIKE","'"," ",time,"'"," ","and courseid LIKE","'",df[1,"courseid"],"'",";")
                                        #rs = dbSendQuery(con, table0)
                                        #data_push <- fetch(rs, n=-1)
                                        
                                        
                                        endtime = Sys.time() + 10800
                                        margin = endtime - firstfeedtime
                                        if ( margin >= 30 ) {
                                                            # Run the function that make the estimation
                                                            
                                                            output <- data.frame(courseid = character(), time = character(), index = character(), accuracy =integer(), percentage = integer(), stringsAsFactors = FALSE)
                                                            output[1,"courseid"] <- df[1,"courseid"]
                                                            output[1,"time"] <- time   # may create error
                                                            output[1,"index"] <- "Clarity"
                                                            output[1,"accuracy"] <- 80
                                                            output[1,"percentage"] <- 65
                                                            
                                                            # Send output to Firebase (it contains curl to Firebase)
                                                            
                                                            # Send output to data base
                                                            dbWriteTable(con, name = "report", value = output, nrows = nrow(output) , row.names = FALSE,  overwrite = FALSE, append = 1)
                                                            count <- 2
                                                            assign("count",2, envir = .GlobalEnv)
                                                            print("****####rahi mchat el while loop assidi####****")
                                        }
                                        else {
                                                            count <- 1
                                        }
                                        if(count==2) {
                                                            break
                                        }
                    }
}
