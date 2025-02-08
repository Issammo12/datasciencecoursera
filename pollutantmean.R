pollutantmean <- function(dir , pollutant , id=1:332){
  list<-c()
  for(i in id){
    if(i<10){
      index<-paste("00" , i , sep="")
      path<-glue("{dir}/{index}.csv")
      l<-read.csv(path)
    }else if(i>=10 && i<=99){
      index<-paste("0" , i , sep="")
      path<-glue("{dir}/{index}.csv")
      l<-read.csv(path)
    }else{
      index<-i
      path<-glue("{dir}/{index}.csv")
      l<-read.csv(path)
    }
    list<-rbind(list , l)
  }
  col<-list[pollutant]
  col[is.na(col)]<-0
  meanCol<-colMeans(col)
  return(meanCol)
}

# read <- function(i){
#   if(i<10){
#     index<-paste("00" , i , sep="")
#     path<-glue("specdata/{index}.csv")
#     read.csv(path)
#   }else if(i>10 && i<99){
#     index<-paste("0" , i , sep="")
#     path<-glue("specdata/{index}.csv")
#     read.csv(path)
#   }else{
#     index<-i
#     path<-glue("specdata/{index}.csv")
#     read.csv(path)
#   }
# }



# pollutantmean <- function(directory, pollutant, id = 1:332) {
#   # Ensure the directory has a slash at the end
#   if (!endsWith(directory, "/")) {
#     directory <- paste0(directory, "/")
#   }
# 
#   # Initialize an empty vector to store pollutant data
#   pollutant_data <- c()
# 
#   # Loop through the specified monitor IDs
#   for (monitor_id in id) {
#     # Format the file name to have three digits (e.g., "001.csv", "010.csv", "100.csv")
#     file_name <- sprintf("%03d.csv", monitor_id)
#     file_path <- paste0(directory, file_name)
# 
#     # Read the CSV file
#     data <- read.csv(file_path)
# 
#     # Extract the pollutant data (ignore NA values)
#     pollutant_data <- c(pollutant_data, data[[pollutant]])
#   }
# 
#   # Calculate and return the mean, ignoring NA values
#   # mean_value <- mean(pollutant_data, na.rm = TRUE)
#   return(pollutant_data)
# }
