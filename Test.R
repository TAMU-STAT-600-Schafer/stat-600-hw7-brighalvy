# ## Run Tests:
# 
# # Give example
# set.seed(1234) # set seed
# n <- 300 # set n
# p <- 3 # set number of covariates
# K <- 2 # number of classes
# beta <- matrix(c(2, -.11, .1, 1.5, .1, .01), p, K) # set beta
# X1 <- cbind(rep(1, n/2), runif(n/2, 2, 3), rnorm(n/2)) # set X1
# X2 <- cbind(rep(1, n/2), runif(n/2, -.5, 1), rnorm(n/2, 1)) # set X2
# X <- rbind(X1, X2)
# exp_XB <- exp(X %*% beta)
# pk <- exp_XB / rowSums(exp_XB)
# y <- apply(pk, 1, which.max) - 1 # Get simulated y:
# 
# 
# init <- initialize_bw(p, 10, K)
# 
# 
# Xval = X
# yval = y
# 
# hidden_p = 100
# 
# seed = 1234
# out = NN_train(X, y, Xval, yval, rate = 0.01, mbatch = 20, nEpoch = 400, hidden_p = hidden_p, scale = 1e-3, seed = seed)
# 
# plot(1:length(out$error), out$error, ylim = c(0, 30))
# lines(1:length(out$error), out$error_val)

# Load the data

# Training data
letter_train <- read.table("Data/letter-train.txt",
                           header = F,
                           colClasses = "numeric")
Y <- letter_train[, 1]
X <- as.matrix(letter_train[, -1])

# Update training to set last part as validation
id_val = 1801:2000
Yval = Y[id_val]
Xval = X[id_val, ]
Ytrain = Y[-id_val]
Xtrain = X[-id_val, ]

# Testing data
letter_test <- read.table("Data/letter-test.txt",
                          header = F,
                          colClasses = "numeric")
Yt <- letter_test[, 1]
Xt <- as.matrix(letter_test[, -1])

# Source the NN function
source("FunctionsNN.R")

# [ToDo] Source the functions from HW3 (replace FunctionsLR.R with your working code)
source("FunctionsLR.R")

# Recall the results of linear classifier from HW3
# Add intercept column
Xinter <- cbind(rep(1, nrow(Xtrain)), Xtrain)
Xtinter <- cbind(rep(1, nrow(Xt)), Xt)


# Apply neural network training with default given parameters
out2 = NN_train(Xtrain, Ytrain, Xval, Yval,lambda = 0.001,
                rate = 0.1, mbatch = 50, nEpoch = 500, hidden_p = 100,
                scale = 1e-3, seed = 12345)
plot(1:length(out2$error), out2$error, ylim = c(0, 70))
lines(1:length(out2$error_val), out2$error_val, col = "red")


test_error = evaluate_error(Xt, Yt, out2$params$W1, out2$params$b1,
                            out2$params$W2, out2$params$b2)
test_error 
