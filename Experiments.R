## Experiments

## Seed in order to reproduce experiments
set.seed(20160129)

source("Utils.R")
source("SetUp.R")
source("KGPolicy.R")
source("RandomPolicy.R")

##
## Experiment 1. Average performance along the 
## measurement budget [5, 100] and number of alternatives {5, 20}
##

## Seed in order to reproduce experiments
set.seed(20160129)

m = 5
R = 100
results_05 = data.frame()
for (N in seq(from = 5, to = 25, by = 1)) {
  rows = NULL  
  for (r in 1:R) {
    p <- getPWithProperty(m)
    state0 <- getCorrelatedPriors(p, 1, 3)
    resultKG <- KGPolicy(p, state0, N)
    resultRnd <- randomPolicy(p, state0, N)
    rows <- rbind(rows, c(getDistance(p, resultKG$x), getDistance(p, resultRnd$x)))
  }
  print(N)
  results_05 <- rbind(results_05, c(N, mean(rows[, 1]), sd(rows[, 1]), mean(rows[, 2]), sd(rows[, 2])))
}
names(results_05) <- c("N", "KG.Dist", "KG.Sd", "Rand.Dist", "Rand.Sd")
save(results_05, file = "data/results_05.rda")

## Seed in order to reproduce experiments
set.seed(20160129)

m = 20
R = 100
results_20 = data.frame()
for (N in seq(from = 20, to = 100, by = 10)) {
  rows = NULL  
  for (r in 1:R) {
    p <- getPWithProperty(m)
    state0 <- getCorrelatedPriors(p, 1, 3)
    resultKG <- KGPolicy(p, state0, N)
    resultRnd <- randomPolicy(p, state0, N)
    rows <- rbind(rows, c(getDistance(p, resultKG$x), getDistance(p, resultRnd$x)))
  }
  print(N)
  results_20 <- rbind(results_20, c(N, mean(rows[, 1]), sd(rows[, 1]), mean(rows[, 2]), sd(rows[, 2])))
}
names(results_20) <- c("N", "KG.Dist", "KG.Sd", "Rand.Dist", "Rand.Sd")
save(results_20, file = "data/results_20.rda")

## Plot results 
par(mfcol = c(1, 2))

plot(seq(from = 5, to = 25, by = 1), results_05$KG.Dist, type = "l", xlab = "N", ylab = "dis(x*)", lwd = 2, col = 2, ylim = c(-0.3, 1.6), main = "m = 5", panel.first = grid(lwd = 1.5))
lines(seq(from = 5, to = 25, by = 1), results_05$KG.Dist + results_05$KG.Sd, lwd = 2, col = 2, lty = 3)
lines(seq(from = 5, to = 25, by = 1), results_05$KG.Dist - results_05$KG.Sd, lwd = 2, col = 2, lty = 3)
lines(seq(from = 5, to = 25, by = 1), results_05$Rand.Dist, lwd = 2, col = 3)
lines(seq(from = 5, to = 25, by = 1), results_05$Rand.Dist + results_05$Rand.Sd, lwd = 2, col = 3, lty = 3)
lines(seq(from = 5, to = 25, by = 1), results_05$Rand.Dist - results_05$Rand.Sd, lwd = 2, col = 3, lty = 3)
legend("topright", c("KG", "Random"), lwd = c(2, 2), col = c(2, 3))

plot(seq(from = 20, to = 100, by = 10), results_20$KG.Dist, type = "l", xlab = "N", ylab = "dis(x*)", lwd = 2, col = 2, ylim = c(-0.5, 4.0), main = "m = 20", panel.first = grid(lwd = 1.5))
lines(seq(from = 20, to = 100, by = 10), results_20$KG.Dist + results_20$KG.Sd, lwd = 2, col = 2, lty = 3)
lines(seq(from = 20, to = 100, by = 10), results_20$KG.Dist - results_20$KG.Sd, lwd = 2, col = 2, lty = 3)
lines(seq(from = 20, to = 100, by = 10), results_20$Rand.Dist, lwd = 2, col = 3)
lines(seq(from = 20, to = 100, by = 10), results_20$Rand.Dist + results_20$Rand.Sd, lwd = 2, col = 3, lty = 3)
lines(seq(from = 20, to = 100, by = 10), results_20$Rand.Dist - results_20$Rand.Sd, lwd = 2, col = 3, lty = 3)
legend("topright", c("KG", "Random"), lwd = c(2, 2), col = c(2, 3))

##
## Experiment 2. KG factors over time
##

## Seed in order to reproduce experiments
set.seed(20160129)

m = 5
N = 25
R = 1000
kg.factors = NULL  
for (r in 1:R) {
  p <- getPWithProperty(m)
  state0 <- getCorrelatedPriors(p, 1, 3)
  resultKG <- KGPolicy(p, state0, N)
  kg.factors <- rbind(kg.factors, resultKG$kg.factors)
  print(r)
}

kgFactorMeans <- apply(kg.factors, 2, mean)
  
notZeroRate <- apply(kg.factors, 2, function(x) {
  zeros <- sum(abs(x) < 1e-12)
  (length(x) - zeros)/length(x) })

par(mfcol = c(1, 2))
plot(kgFactorMeans, type = "l", lwd = 2, ylim = c(0.0002, 0.0018), xlab = "n", ylab = expression(v^n), panel.first = grid(lwd = 1.5))
plot(notZeroRate, type = "l", lwd = 2, xlab="n", ylab = expression(paste("P[", v^n, " > 0]")), panel.first = grid(lwd = 1.5))

##
## Experiment 3. Effect of p elements
##
set.seed(20160129)

m = 5
R = 1000
results_05_v2 = data.frame()
for (N in seq(from = 5, to = 25, by = 1)) {
  rows = NULL  
  for (r in 1:R) {
    p <- getPWithProperty(m, 21)
    state0 <- getCorrelatedPriors(p, 1, 3)
    resultKG <- KGPolicy(p, state0, N)
    resultRnd <- randomPolicy(p, state0, N)
    rows <- rbind(rows, c(getDistance(p, resultKG$x), getDistance(p, resultRnd$x)))
  }
  print(N)
  results_05_v2 <- rbind(results_05_v2, c(N, mean(rows[, 1]), sd(rows[, 1]), mean(rows[, 2]), sd(rows[, 2])))
}
names(results_05_v2) <- c("N", "KG.Dist", "KG.Sd", "Rand.Dist", "Rand.Sd")
save(results_05_v2, file = "data/results_05_v2.rda")

par(mfcol=c(1, 1))
plot(seq(from = 5, to = 25, by = 1), results_05_v2$KG.Dist, type = "l", xlab = "N", ylab = "dis(x*)", lwd = 2, col = 2, ylim = c(-0.3, 3.6), panel.first = grid(lwd = 1.5))
lines(seq(from = 5, to = 25, by = 1), results_05_v2$KG.Dist + results_05_v2$KG.Sd, lwd = 2, col = 2, lty = 3)
lines(seq(from = 5, to = 25, by = 1), results_05_v2$KG.Dist - results_05_v2$KG.Sd, lwd = 2, col = 2, lty = 3)
lines(seq(from = 5, to = 25, by = 1), results_05_v2$Rand.Dist, lwd = 2, col = 3)
lines(seq(from = 5, to = 25, by = 1), results_05_v2$Rand.Dist + results_05_v2$Rand.Sd, lwd = 2, col = 3, lty = 3)
lines(seq(from = 5, to = 25, by = 1), results_05_v2$Rand.Dist - results_05_v2$Rand.Sd, lwd = 2, col = 3, lty = 3)
legend("topright", c("KG", "Random"), lwd = c(2, 2), col = c(2, 3))

##
## Experiment 4. Effect of priors
##
set.seed(20160129)

m = 5
R = 1000
results_05_v3 = data.frame()
for (N in seq(from = 5, to = 25, by = 1)) {
  rows = NULL  
  for (r in 1:R) {
    p <- getPWithProperty(m)
    state0 <- getEqualPriors(p, 4)
    resultKG <- KGPolicy(p, state0, N)
    resultRnd <- randomPolicy(p, state0, N)
    rows <- rbind(rows, c(getDistance(p, resultKG$x), getDistance(p, resultRnd$x)))
  }
  print(N)
  results_05_v3 <- rbind(results_05_v3, c(N, mean(rows[, 1]), sd(rows[, 1]), mean(rows[, 2]), sd(rows[, 2])))
}
names(results_05_v3) <- c("N", "KG.Dist", "KG.Sd", "Rand.Dist", "Rand.Sd")
save(results_05_v3, file = "data/results_05_v3.rda")

par(mfcol=c(1, 1))
plot(seq(from = 5, to = 25, by = 1), results_05_v3$KG.Dist, type = "l", xlab = "N", ylab = "dis(x*)", lwd = 2, col = 2, ylim = c(-0.3, 2), panel.first = grid(lwd = 1.5))
lines(seq(from = 5, to = 25, by = 1), results_05_v3$KG.Dist + results_05_v3$KG.Sd, lwd = 2, col = 2, lty = 3)
lines(seq(from = 5, to = 25, by = 1), results_05_v3$KG.Dist - results_05_v3$KG.Sd, lwd = 2, col = 2, lty = 3)
lines(seq(from = 5, to = 25, by = 1), results_05_v3$Rand.Dist, lwd = 2, col = 3)
lines(seq(from = 5, to = 25, by = 1), results_05_v3$Rand.Dist + results_05_v3$Rand.Sd, lwd = 2, col = 3, lty = 3)
lines(seq(from = 5, to = 25, by = 1), results_05_v3$Rand.Dist - results_05_v3$Rand.Sd, lwd = 2, col = 3, lty = 3)
legend("topright", c("KG", "Random"), lwd = c(2, 2), col = c(2, 3))

## Tests
t.test(results_05_v3$KG.Dist, results_05_v3$Rand.Dist, "less", paired = TRUE)

##
## Experiment 5.
##
plot(5:20, 1/(5:20 * (2 + 1)), ylim = c(0, 0.07), 
     xlab = expression(m), ylab = expression(paste(Delta, bar(p)[x])), type = "l", lwd = 2, panel.first = grid(lwd = 1.5))
lines(5:20, 1/(5:20 * (3 + 1)), lty = 2, lwd = 2)
lines(5:20, 1/(5:20 * (5 + 1)), lty = 3, lwd = 2)
lines(5:20, 1/(5:20 * (10 + 1)), lty = 4, lwd = 2)
legend("topright", c(expression(paste(n[xy], " = 2")), 
                     expression(paste(n[xy], " = 3")), 
                     expression(paste(n[xy], " = 5")), 
                     expression(paste(n[xy], " = 10"))), lty = 1:4, lwd = rep(2, 4))