# 
source("educaper_utilities.R")  # load necessary functions

diagnosis <- function(url) { # The URL must be string
  # Get data and diagnose
   # Get the JSON data an coerce it to a data frame
  library(curl)
  library(jsonlite)
  url <- "https://educaper-app.firebaseio.com/Reports/.json"
  tmp <- tempfile()
  b <- curl_download(url, tmp)
  json_data = fromJSON(b)
  json_data = sapply(json_data,rbind)
  df <- as.data.frame(json_data)
  
   # Send data to the app
  json <- toJSON(df)
  json
  