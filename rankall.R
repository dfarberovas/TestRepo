rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  
  ## read data
  o <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## 1. check if state is correct
  ##s<-unique(o[,7])
  ##if (!(state %in% s)){
  ##  stop("invalid state")
  ##}
  
  ## 2. check if outcom is correct
  ## cn - column number for outcome
  cn<-numeric()
  cn<-NULL
  c("heart attack", "heart failure", "pneumonia")
  if (outcome == "heart attack")
    cn = 11
  else if (outcome == "heart failure")
    cn = 17
  else if (outcome == "pneumonia")
    cn = 23
  else
    stop("invalid outcome")
  
  ## 3. check if num is correct
  if (!((!is.numeric(num) && ((num == "best") || (num == "worst"))) || is.numeric(num)))
    stop("invalid num")
  
  ##convert column cn to numeric values
  o[,cn]<-as.numeric(o[,cn])
  
  ## filter NA values for cn columns
  tmp1<-o[which(!is.na(o[,cn])),]
      
  ##Vector if states
  s1<-unique(o[,7])
  s<-sort(s1)
  
  ##Result list
  r<- vector(mode = "list", length = length(s))  
  
  tmp2<-NULL
  tmp3<-NULL
  
  ##Iterate through states
  for(i in seq_along(s))
  {
    ## select state
    tmp2<-tmp1[tmp1$State==s[i],]
    
    ##sort by cn values
    tmp3<-tmp2[order(tmp2[,cn], tmp2[,2]),]
    
    n<-numeric()
    if (num == "best")
      n<-1
    else if (num == "worst")
      n<-nrow(tmp3)
    else
      n<-as.numeric(num)
    
    ##Hospital Name (or NA), state
    r[[i]]<-c(tmp3[n, 2], s[i])
    ##r[i]<-c(tmp3[n, 2], s[i])
  }
  
  ##colnames(r)<-c("hospital","state")  
  ##r
  or <- do.call(rbind, r)
  colnames(or)<-c("hospital","state")
  as.data.frame(or)
  
}