print("SimpleSim(..., fun, pairwise=F)")
SimpleSim <- function(..., fun, pairwise=F) {
  # SimpleSim allows for the calling of a function varying multiple parameters
  # entered as vectors. In pairwise form it acts much like apply. In non-
  # paiwise form it makes a combination of each possible parameter mix
  # in a manner identical to block of nested loops.
  # Basec on code released:
  # http://www.econometricsbysimulation.com/2013/07/simulation-of-blackjack-odds-are-not.html
  
  returner <- NULL
  L        <- list(...)
  # Construct a vector that holds the lengths of each object
  vlength <- unlist(lapply(L, length)) 
  npar    <- length(vlength)
  CL      <- lapply(L, "[", 1) # Current list is equal to the first element
  
 # Pairwise looping
 if (pairwise) {
  # If pairwise is selected than all elements greater than 1 must be equal.
  # Checks if all of the elements of a vector are equal
  if (!(function(x) all(x[1]==x))(vlength[vlength>1])) {
   print(unlist(lapply(L, length)))
   stop("Pairwise: all input vectors must be of equal length", call. =F)
  }
  for (i in 1:max(vlength)) { # Loop through calling the function
   CL[vlength>1]  <- lapply(L, "[", i)[vlength>1] # Current list
   returner <- rbind(returner,c(do.call(fun, CL),pars="", CL))
  }
 } # End Pairwise

 # Non-pairwise looping
 if (!pairwise) {
  ncomb <- prod(vlength) # Calculate the number of combinations
  print(paste(ncomb, "combinations to loop through"))
  comb <- matrix(NA, nrow=prod(vlength), ncol=npar+1)
  comb[,1] <- 1:prod(vlength) # Create an index value
  comb <- as.data.frame(comb) # Converto to data.frame
  colnames(comb) <- c("ID", names(CL))
  for (i in (npar:1)) { # Construct a matrix of parameter combinations
   comb[,i+1] <- L[[i]] # Replace one column with values
   comb<-comb[order(comb[,(i+1)]),] # Reorder rows
  }
  comb<-comb[order(comb[,1]),]
  for (i in 1:ncomb) {
   for (ii in 1:npar) CL[ii] <- comb[i,ii+1]
   returner <- rbind(returner,c(do.call(fun, CL),pars="", CL))
  }
 } # End Non-Pairwise
  
 return(returner)
  
} # END FUNCTION DEFINITION
