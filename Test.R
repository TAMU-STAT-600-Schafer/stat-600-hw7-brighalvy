## Run Tests:

# Give example
set.seed(1234) # set seed
n <- 300 # set n
p <- 3 # set number of covariates
K <- 2 # number of classes
beta <- matrix(c(2, -.11, .1, 1.5, .1, .01), p, K) # set beta
X1 <- cbind(rep(1, n/2), runif(n/2, 2, 3), rnorm(n/2)) # set X1
X2 <- cbind(rep(1, n/2), runif(n/2, -.5, 1), rnorm(n/2, 1)) # set X2
X <- rbind(X1, X2)
exp_XB <- exp(X %*% beta)
pk <- exp_XB / rowSums(exp_XB)
y <- apply(pk, 1, which.max) - 1 # Get simulated y:


init <- initialize_bw(p, 10, K)

