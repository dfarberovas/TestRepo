pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files  
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  
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
  
  ## mean calculation by parameters
  mean(output[output[,"ID"] %in% id,][,pollutant],na.rm=TRUE)
}