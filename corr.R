corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
        
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
    sids<-list()
    
    ## list of id's and nobs per file
    j<-1
    for(i in seq_along(files)){
        n<-tmp2[which(tmp2[,"ID"]==i),]
        if(nrow(n)>threshold){
            sids[[j]]<-n
            j<-j+1
        }
    }
    corvals<-c()
    for(i in seq_along(sids)){             
             corvals<-c(corvals, cor(sids[[i]]$nitrate, sids[[i]]$sulfate))
        }
    corvals
}