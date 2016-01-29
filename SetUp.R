## Script set ups matrix P

##
## Creates a matrix "P" of the given size "m"
##
getPWithProperty <- function(m, n = m) {
  zeros <- rep(0.0, m * m)
  p <- matrix(zeros, nrow=m, ncol=m)
  p <- fillPWithProperty(p, n)
  permute.matrix(p)
}

##
## Fills a matrix "P" according to transitivity property
##
fillPWithProperty <- function(p, n) {
  zeros <- rep(0.0, nrow(p)*ncol(p))
  ones <- rep(1.0, nrow(p)*ncol(p))
  
  lowerBounds <- setLowerBounds(matrix(zeros, nrow=nrow(p), ncol=ncol(p)), n)
  upperBounds <- setUpperBounds(matrix(ones, nrow=nrow(p), ncol=ncol(p)), n)
  
  for (x in 1:nrow(p)) {
    for (y in 1:ncol(p)) {
      if (x == y)
        p[x, x] = 0.5
      else if (p[x, y] == 0) {
        prob <- runif(1, min = lowerBounds[x, y], max = upperBounds[x, y])
        p[x, y] <- prob
        p[y, x] <- 1 - prob
      }
    }
  }
  
  p
}

##
## Checks if alternative "x" is not less than "y"
##
notLess <- function(x, y) {
  sum(x >= y) == length(x)
}

##
## Sets lower bounds for all alternatives
##
setLowerBounds <- function(lowerBounds, n) {
  step <- 0.5/(n - 1)
  
  for (x in 1:nrow(lowerBounds)) {
    for (y in 1:ncol(lowerBounds)) {
      if (y > x) {
        lowerBounds[x, y] = 0.5 + (y - x - 1)*step 
        lowerBounds[y, x] = 1 - 0.5 + (y - x)*step
      }
    }  
  }
  
  lowerBounds
}

##
## Sets upper bounds for all alternatives
##
setUpperBounds <- function(upperBounds, n) {
  step <- 0.5/(n - 1)
  
  for (x in 1:nrow(upperBounds)) {
    for (y in 1:ncol(upperBounds)) {
      if (y > x) {
        upperBounds[x, y] = 0.5 + (y - x)*step 
        upperBounds[y, x] = 1 - 0.5 + (y - x - 1)*step
      }
    }  
  }
  
  upperBounds
}

##
## Permutes matrix p randomly
##
permute.matrix <- function(p) {
  permutation <- sample(1:nrow(p), nrow(p))
  
  for (x in 1:nrow(p)) {
    p <- swapRows(x, permutation[x], p)
    p <- swapColumns(x, permutation[x], p)
  }
  
  p
}

##
## Swaps two rows
##
swapRows <-function(x, y, p) {
  x.row <- p[x, ]
  y.row <- p[y, ]
  
  p[x, ] <- y.row
  p[y, ] <- x.row
  
  p
}

##
## Swaps two columns
##
swapColumns <-function(x, y, p) {
  x.col <- p[, x]
  y.col <- p[, y]
  
  p[, x] <- y.col
  p[, y] <- x.col
  
  p
}

##
## Sets priors for the problem randomly
##
getRandomPriors <- function(p, n.min, n.max) {
  zeros <- rep(0.0, nrow(p)*ncol(p))
  
  alpha <- matrix(zeros, nrow=nrow(p), ncol=ncol(p))
  beta <- matrix(zeros, nrow=nrow(p), ncol=ncol(p))
  
  for (x in 1:nrow(p)) {
    for (y in 1:ncol(p)) {
      if (x != y) {
        trials <- sample(n.min:n.max, 1)
        alpha[x, y] <- sample(1:trials, 1)
        beta[x, y] <- trials - alpha[x, y]
        beta[y, x] <- alpha[x, y]
        alpha[y, x] <- beta[x, y]
      }
    }
  }
  
  list("alpha" = alpha, "beta" = beta)
}

##
## Sets priors for the problem randomly
##
getCorrelatedPriors <- function(p, n.min, n.max) {
  zeros <- rep(0.0, nrow(p)*ncol(p))
  
  alpha <- matrix(zeros, nrow=nrow(p), ncol=ncol(p))
  beta <- matrix(zeros, nrow=nrow(p), ncol=ncol(p))
  
  for (x in 1:nrow(p)) {
    for (y in 1:ncol(p)) {
      if (x != y) {
        trials <- sample(n.min:n.max, 1)
        alpha[x, y] <- rbinom(n = 1, size = trials, prob = p[x, y])
        beta[x, y] <- trials - alpha[x, y]
        beta[y, x] <- alpha[x, y]
        alpha[y, x] <- beta[x, y]
      }
    }
  }
  
  list("alpha" = alpha, "beta" = beta)
}

##
## Sets priors for the problem equally, n must be an even number
##
getEqualPriors <- function(p, n) {
  zeros <- rep(0.0, nrow(p)*ncol(p))
  
  alpha <- matrix(zeros, nrow=nrow(p), ncol=ncol(p))
  beta <- matrix(zeros, nrow=nrow(p), ncol=ncol(p))
  
  for (x in 1:nrow(p)) {
    for (y in 1:ncol(p)) {
      if (x != y) {
        trials <- n
        alpha[x, y] <- n %/% 2
        beta[x, y] <- trials - alpha[x, y]
        beta[y, x] <- alpha[x, y]
        alpha[y, x] <- beta[x, y]
      }
    }
  }
  
  list("alpha" = alpha, "beta" = beta)
}

##
## Test 1. Sum of "P" elements
##
{
  m1 <- sample(5:10, 1)
  m2 <- sample(10:50, 1)
  m3 <- sample(10:100, 1)
  
  p1 <- getPWithProperty(m1)
  p2 <- getPWithProperty(m2)
  p3 <- getPWithProperty(m3)
  
  assert(sum(p1) == (m1 * m1 - m1)/2 + 0.5*m1, "Wrong sum of matrix", "Right sum of matrix")
  assert(sum(p2) == (m2 * m2 - m2)/2 + 0.5*m2, "Wrong sum of matrix", "Right sum of matrix")
  assert(sum(p3) == (m3 * m3 - m3)/2 + 0.5*m3, "Wrong sum of matrix", "Right sum of matrix")
}

##
## Test 2. Transitivity property
##
{
  n <- sample(10:100, 1)
  m <- ifelse(n %% 2 == 0, n, n + 1) 
  p1 <- getPWithProperty(m)
  p2 <- getPWithProperty(m, 2*m)
  policy = matrix(sample(1:m, m), ncol=2)
  
  for (k in 1:nrow(policy)) {
    x <- policy[k, 1]
    y <- policy[k, 2]
    if (p1[x, y] >= p1[y, x]) assert(notLess(p1[x, ], p1[y, ]), "Transitivity does not stand true", "Transitivity stands true") 
    else assert(notLess(p1[y, ], p1[x, ]), "Transitivity does not stand true", "Transitivity stands true")
  }
  
  for (k in 1:nrow(policy)) {
    x <- policy[k, 1]
    y <- policy[k, 2]
    if (p2[x, y] >= p2[y, x]) assert(notLess(p2[x, ], p2[y, ]), "Transitivity does not stand true", "Transitivity stands true") 
    else assert(notLess(p2[y, ], p2[x, ]), "Transitivity does not stand true", "Transitivity stands true")
  }
}

##
## Test 3. Sum of alpha elements equals sum of beta elements
##
{
  m <- sample(10:100, 1)
  p <- getPWithProperty(m)
  
  state1 <- getRandomPriors(p, 2, 10)
  state2 <- getCorrelatedPriors(p, 2, 10)
    
  assert(sum(state1$alpha) == sum(state1$beta), "Sum does not equal", "Sum equals")
  assert(sum(state2$alpha) == sum(state2$beta), "Sum does not equal", "Sum equals")
}
