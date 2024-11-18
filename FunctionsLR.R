# Write function to calculate p_k(x;b)
calc_pk <- function(X, b) {
  exp_XB <- exp(X %*% b)
  pk <- exp_XB / rowSums(exp_XB)
}
# Objective Function:
f <- function(X, y, beta, lambda, pk, n) {
  y_use <- cbind(y + 1, 1:n)
  - sum(apply(y_use, 1, \(x) log(pk[x[2], x[1]]))) + lambda / 2 * sum(beta ^ 2)
}
# Error Function:
error <- function(y, pk) {
  preds <- apply(pk, 1, which.max) - 1
  mean(y != preds) * 100
}

# Function that implements multi-class logistic regression.
#############################################################
# Description of supplied parameters:
# X - n x p training data, 1st column should be 1s to account for intercept
# y - a vector of size n of class labels, from 0 to K-1
# Xt - ntest x p testing data, 1st column should be 1s to account for intercept
# yt - a vector of size ntest of test class labels, from 0 to K-1
# numIter - number of FIXED iterations of the algorithm, default value is 50
# eta - learning rate, default value is 0.1
# lambda - ridge parameter, default value is 1
# beta_init - (optional) initial starting values of beta for the algorithm, should be p x K matrix 

## Return output
##########################################################################
# beta - p x K matrix of estimated beta values after numIter iterations
# error_train - (numIter + 1) length vector of training error % at each iteration (+ starting value)
# error_test - (numIter + 1) length vector of testing error % at each iteration (+ starting value)
# objective - (numIter + 1) length vector of objective values of the function that we are minimizing at each iteration (+ starting value)
LRMultiClass <- function(X, y, Xt, yt, numIter = 50, eta = 0.1, lambda = 1, beta_init = NULL){
  ## Check the supplied parameters as described. You can assume that X, Xt are matrices; y, yt are vectors; and numIter, eta, lambda are scalars. You can assume that beta_init is either NULL (default) or a matrix.
  ###################################
  # Check that the first column of X and Xt are 1s, if not - display appropriate message and stop execution.
  if (unique(X[, 1]) != 1) {
    stop(paste("First column of X is not 1s"))
  }
  if (unique(Xt[, 1]) != 1) {
    stop(paste("First column of Xt is not 1s"))
  }
  # Check for compatibility of dimensions between X and Y
  n <- length(y)
  if (nrow(X) != n) {
    stop(
      paste(
        "Number of observations in y aren't compatible with number of observations in X"
      )
    )
  }
  # Check for compatibility of dimensions between Xt and Yt
  nt <- length(yt)
  if (nrow(Xt) != nt) {
    stop(
      paste(
        "Number of observations in yt aren't compatible with number of observations in Xt"
      )
    )
  }
  # Check for compatibility of dimensions between X and Xt
  p <- ncol(X)
  if (p != ncol(Xt)) {
    stop(paste("Number of columns in X don't match Xt"))
  }
  # Check eta is positive
  if (eta <= 0) {
    stop(paste("Eta isn't positive"))
  }
  # Check lambda is non-negative
  if (lambda < 0) {
    stop(paste("Lambda is negative"))
  }
  
  # Check whether beta_init is NULL. If NULL, initialize beta with p x K matrix of zeroes. If not NULL, check for compatibility of dimensions with what has been already supplied.
  K <- length(unique(y))
  if (is.null(beta_init)) {
    beta_init <- matrix(0, nrow = p, ncol = K)
  } else{
    if (p != nrow(beta_init)) {
      stop(paste(
        "wrong number of rows in beta_init. Should be ",
        p,
        " has ",
        nrow(beta_init)
      ))
    }
    if (K != ncol(beta_init)) {
      stop(paste(
        "wrong number of columns in beta_init. Should be ",
        K,
        " has ",
        ncol(beta_init)
      ))
    }
  }
  
  
  ## Calculate corresponding pk, objective value f(beta_init), training error and testing error given the starting point beta_init
  ##########################################################################
  pk <- calc_pk(X, beta_init)
  objective <- c(f(X, y, beta_init, lambda, pk, n), rep(0, numIter))
  error_train <- c(error(y, pk), rep(0, numIter))
  pkt <- calc_pk(Xt, beta_init)
  error_test <- c(error(yt, pkt), rep(0, numIter))
  
  ## Newton's method cycle - implement the update EXACTLY numIter iterations
  ##########################################################################
  beta <-  beta_init
  for (i in 2:(numIter + 1)) {
    # Update Beta:
    for (k in 1:K) {
      gradient <- t(X) %*% (pk[, k] - as.numeric(y == (k - 1))) + lambda * beta[, k]
      w <- pk[, k] * (1 - pk[, k])
      hessian <- t(X) %*% (w * X) + diag(lambda, nrow = p)
      beta[, k] <- beta[, k] - eta * solve(hessian) %*% gradient
    }
    # Within one iteration: perform the update, calculate updated objective function and training/testing errors in %
    pk <- calc_pk(X, beta)
    objective[i] <- f(X, y, beta, lambda, pk, n)
    error_train[i] <- error(y, pk)
    pkt <- calc_pk(Xt, beta)
    error_test[i] <- error(yt, pkt)
  }
  ## Return output
  ##########################################################################
  # beta - p x K matrix of estimated beta values after numIter iterations
  # error_train - (numIter + 1) length vector of training error % at each iteration (+ starting value)
  # error_test - (numIter + 1) length vector of testing error % at each iteration (+ starting value)
  # objective - (numIter + 1) length vector of objective values of the function that we are minimizing at each iteration (+ starting value)
  return(list(beta = beta, error_train = error_train, error_test = error_test, objective =  objective))
}