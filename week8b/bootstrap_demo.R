# set parameters
n <- 100  # number of samples
B <- 1000  # number of bootstraps
p <- .25  # true success rate p

# generate iid data from Bern(p)
data <- rbernoulli(n = n, p = p)

# bootstrap to estimate Var(Xbar); using known distribution
boots <- sapply(X = 1:B,
                FUN = function(X) {
                  samp <- rbernoulli(n = n, p = p)
                  return(mean(samp))
                })
hist(boots)
var(boots)  # bootstrapped variance estimate
p * (1 - p) / n

# bootstrap to estimate Var(Xbar); using plug-in estimate
boots <- sapply(X = 1:B,
                FUN = function(X) {
                  samp <- sample(data, size = n, replace = T)
                  return(mean(samp))
                })
hist(boots)
var(boots)  # bootstrapped variance estimate
p * (1 - p) / n
