## Presents KG Policy for Sequential A/B testing

##
## Extension of KG policy for Sequential A/B testing
##
KGPolicy <- function(p, state, n) {
  values <- NULL
  decisions <- NULL
  kg.factors <- NULL
  ws <- NULL
  
  for (i in 1:n) {
    values <- c(values, getValue(state))                        ## logging
    decision <- getDecision(state)
    decisions <- rbind(decisions, decision)                     ## logging
    kg.factors <- c(kg.factors, getKGFactor(state, decision))   ## logging
    w <- simulate(decision, p)
    ws <- c(ws, w)                                              ## logging
    state <- nextState(state, decision, w)
  }
  
  list(
    "x" = getImplementationDecision(state), 
    "p.bar" = getValue(state), 
    "values" = values,
    "decisions" = decisions,
    "kg.factors" = kg.factors,
    "ws" = ws)
}

##
## Simulates an A/B test
##
simulate <- function(decision, p) {
  x.one <- decision[1]
  x.two <- decision[2]
  
  rbinom(1, 1, p[x.one, x.two])
}

##
## Returns decision to be applied
##
getDecision <- function(state) {
  decisions <- NULL
  kg.factors <- NULL
  
  for (x in 1:nrow(state$alpha)) {
    for (y in 1:nrow(state$alpha)) {
      if (y > x) {
        decision <- c(x, y)
        decisions <- rbind(decisions, decision)     
        kg.factors <- c(kg.factors, getKGFactor(state, decision))
      }
    }
  }
  
  decisions[sample(which(kg.factors == max(kg.factors)), 1), ]
}

##
## Returns KG factor for a decision
##
getKGFactor <- function(state, decision) {
  x.one <- decision[1]
  x.two <- decision[2]
  
  next.state.a <- nextState(state, decision, 1)
  next.state.b <- nextState(state, decision, 0)
  
  next.value.a <- getValue(next.state.a)
  next.value.b <- getValue(next.state.b)
  
  prob.of.a <- getProb(state, x.one, x.two)
  
  (next.value.a*prob.of.a + next.value.b*(1 - prob.of.a)) - getValue(state)
}

##
## Returns an implementation decision
##
getImplementationDecision <- function(state) {
  p.bars <- getPBars(state)
  
  which(p.bars == max(p.bars))[1]
}

##
## Returns the value of a state
##
getValue <- function(state) {
  max(getPBars(state))
}

##
## Returns p bars of a state
##
getPBars <- function(state) {
  probs <- matrix(rep(0.5, nrow(state$alpha)*ncol(state$alpha)), nrow = nrow(state$alpha), ncol = ncol(state$alpha))
  
  for (x in 1:nrow(probs)) {
    for (y in 1:ncol(probs)) {
      if (x != y) probs[x, y] <- getProb(state, x, y)
      else probs[x, y] <- 0.5
    }  
  }
  
  apply(probs, 1, mean)
}

##
## Returns posterior probability of "x" bieng better than "y"
##
getProb <- function(state, x, y) {
  state$alpha[x, y]/(state$alpha[x, y] + state$beta[x, y])
}

##
## Updates state after applied decision
##
nextState <- function(state, decision, w) {
  alpha <- updateAlpha(state$alpha, decision, w)
  beta <- updateBeta(state$beta, decision, w)
  
  list("alpha" = alpha, "beta" = beta)
}

##
## Updates alpha after applied decision
##
updateAlpha <- function(alpha, decision, w) {
  x.one <- decision[1]
  x.two <- decision[2]
  
  for (x in 1:nrow(alpha)) {
    for (y in 1:ncol(alpha)) {
      if (x == x.one && y == x.two) {
        alpha[x, y] <- alpha[x, y] + w
      } 
      else if (x == x.two && y == x.one) {
        alpha[x, y] <- alpha[x, y] + (1 - w)
      }
    }  
  }
  
  alpha
}

##
## Updates beta after applied decision
##
updateBeta <- function(beta, decision, w) {
  x.one <- decision[1]
  x.two <- decision[2]
  
  for (x in 1:nrow(beta)) {
    for (y in 1:ncol(beta)) {
      if (x == x.one && y == x.two) {
        beta[x, y] <- beta[x, y] + (1 - w)
      } 
      else if (x == x.two && y == x.one) {
        beta[x, y] <- beta[x, y] + w
      }
    }  
  }
  
  beta
}

##
## Test 1. Update alpha and beta
##
{
  alpha <- matrix(c(0, 1, 2, 1, 0, 1, 1, 1, 0), nrow = 3, ncol = 3, byrow = TRUE)
  beta <- matrix(c(0, 1, 2, 1, 0, 1, 1, 1, 0), nrow = 3, ncol = 3, byrow = TRUE)
  
  next.state <- nextState(list("alpha" = alpha, "beta" = beta), c(1, 2), 1)
  next.alpha <- next.state$alpha
  next.beta <- next.state$beta
  
  target.alpha <- matrix(c(0, 2, 2, 1, 0, 1, 1, 1, 0), nrow = 3, ncol = 3, byrow = TRUE)
  target.beta <- matrix(c(0, 1, 2, 2, 0, 1, 1, 1, 0), nrow = 3, ncol = 3, byrow = TRUE)
  
  assert(sum(next.alpha == target.alpha) == 9, "Wrong update", "Right update")
  assert(sum(next.beta == target.beta) == 9, "Wrong update", "Right update")
}

##
## Test 2. Implementation decision, value of state and p bars 
##
{
  state <- list(
    "alpha" = matrix(c(0, 1, 4, 1, 0, 1, 1, 1, 0), nrow = 3, ncol = 3, byrow = TRUE), 
    "beta" = matrix(c(0, 1, 1, 1, 0, 1, 4, 1, 0), nrow = 3, ncol = 3, byrow = TRUE))
  
  tol = 1e-12
  assert(abs(sum(getPBars(state) - c((0.5 + 0.5 + 4/5)/3, 0.5, (1/5 + 0.5 + 0.5)/3))) < tol, "Wrong p bars", "Right p bars")
  assert(abs(getValue(state) - (0.5 + 0.5 + 4/5)/3) < tol, "Wrong value", "Right value")
  assert(getImplementationDecision(state) == 1, "Wrong decision", "Right decision")
}

##
## Test 3. Policy and KG factor
##
{
  state <- list(
    "alpha" = matrix(c(0, 1, 4, 1, 0, 1, 1, 1, 0), nrow = 3, ncol = 3, byrow = TRUE), 
    "beta" = matrix(c(0, 1, 1, 1, 0, 1, 4, 1, 0), nrow = 3, ncol = 3, byrow = TRUE))
  
  tol = 1e-12
  assert(getKGFactor(state, c(1, 2)) == getKGFactor(state, c(2, 1)), "Order is important", "Order is not important")
  assert(getKGFactor(state, c(1, 2)) > getKGFactor(state, c(2, 3)), "Wrong results", "Right results")
  assert(abs(getKGFactor(state, c(2, 3)) - (1/5 * (0.5 * 2 + 4/6)/3 + 4/5 * (0.5 * 2 + 5/6)/3 - 0.6)) < tol, "Wrong results", "Right results")
  assert(sum(getDecision(state) == c(1, 2)) == 2, "Wrong decision", "Right decision")
}
