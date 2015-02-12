add2 <- function(x,y){
  x+y
}

above <- function(x, n = 10){
    use <- x > n
    x[use]
}

columnmeans <- function(x, removeNA = TRUE)
{
  nc <- ncol(x)
  means <- numeric(nc) ## empty numeric vector
  for(i in 1:nc){
    means[i] <- mean(x[,i], na.rm = removeNA)
  }
  means
}
