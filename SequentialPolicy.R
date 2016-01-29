## Presents Sequential Policy for Sequential A/B testing

##
## Sequetial policy
##
sequentialPolicy <- function(p, n) {
  m <- nrow(p)
  step <- n %/% (m - 1) 
  
  not.simulated <- 3:m
  
  x.one <- 1
  x.two <- 2
  wins <- 0
  current.try <- 1
  
  for (i in 1:n) {
    if (current.try > step && i != n) {
      current.try <- 0
      wins <- 0
      if (wins >= step / 2) {
        x.two <- sample(not.simulated, 1)
        not.simulated <- not.simulated[-which(not.simulated == x.two)]
      }
      else {
        x.one <- sample(not.simulated, 1)
        not.simulated <- not.simulated[-which(not.simulated == x.one)]
      }
    }
    
    wins <- wins + simulate(c(x.one, x.two), p)
  }
  
  if (wins >= step / 2) x.one 
  else x.two 
}