complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  ## prepare file list
  files <- list.files(directory, full.names=TRUE)
  
  ## create tmp vector where all data is stored
  tmp<-vector(mode="list", length=length(files))
  
  ## populate vector with files data
  for (i in seq_along(files)) {
    tmp[[i]] <- read.csv(files[[i]])
  }
  
  ## combine into single frame
  output <- do.call(rbind, tmp)
  
  ## filter rows with NA values
  ##  1. filter out nitares mesurements with NA values
  tmp1<-output[which(!is.na(output[,"nitrate"])),]
  ##  2. filter out sulfates mesurements with NA values
  tmp2<-tmp1[which(!is.na(tmp1[,"sulfate"])),]
  
  ## tmp2 frame contains compleate data from all stations
  
  ## create temporary list for requested stations
  sids<- vector(mode = "list", length = length(id))
  
  for(i in seq_along(sids)){
     sids[[i]]<-c(id[i], nrow(tmp2[which(tmp2[,"ID"]==id[i]),]))
  }
  
  output2 <- do.call(rbind, sids)
  colnames(output2)<-c("id","nobs")
  
  output2
  
}