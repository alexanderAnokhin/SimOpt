## Presents Random Policy for Sequential A/B testing

##
## Random policy
##
randomPolicy <- function(p, state, n) {
  values <- NULL
  decisions <- NULL
  ws <- NULL
  
  for (i in 1:n) {
    values <- c(values, getValue(state))                        ## logging
    decision <- sample(1:nrow(p), 2)
    decisions <- rbind(decisions, decision)                     ## logging
    w <- simulate(decision, p)
    ws <- c(ws, w)                                              ## logging
    state <- nextState(state, decision, w)
  }
  
  list(
    "x" = getImplementationDecision(state), 
    "p.bar" = getValue(state), 
    "values" = values,
    "decisions" = decisions,
    "ws" = ws)
}