
yearFix <- function(Act.Year){
X <- Act.Year

# 1.0 Fix 08/07 Years
 a <- which(X==2008)
 b <- length(a)-length(which(X==2007))
 if(b>3){
   c <- round(b/2,0)
   d <- sample(a,c)
   X[d] <- 2007
 }

# 2.0 Fix 05/06 Years
a <- which(X==2006)
b <- length(a)-length(which(X==2005))
if(b>3){
  c <- round(b/2,0)
  d <- sample(a,c)
  X[d] <- 2005
}

# 3.0 Fix 02/03/04 Years
a <- which(X==2002)
b <- length(a)-length(which(X==2003|X==2004))
if(b>3){
  c <- round(b/1.5,0)
  d <- sample(a,c)
  e <- length(d)
  f <- round(e/2,1)
  X[d[1:f]] <- 2003
  X[d[(f+1):e]] <- 2004
}

# 4.0 Return Values
Act.Year <- X
return(Act.Year)
}


