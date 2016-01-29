## Miscellaneous utils

##
## Assert function for testing 
##
assert <- function (expr, error, info) {
  if (! expr) stop(error, call. = FALSE)
  else print(info)
}

##
## Returns alternatives with ranks 
##
getSorted <- function(p) {
  means <- apply(p, 1, mean)
  x = 1:nrow(p)
  ranks <- 1:nrow(p)
  
  result <- cbind(x, means)
  
  cbind(ranks, result[order(-means), ])
}

##
## Returns distance to original rank
##
getDistance <- function(p, x) {
  result <- getSorted(p)
  
  abs(result[which(result[, 2] == x), 1] - 1)
}