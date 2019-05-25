Vardata = function(alpha = rep(0.5, 2), beta = rep(0.25, 4), inter = 0, 
                       gama = rep(0.5, 4), inter34 = 0)
{
  library(MASS)
  Z1 = runif(1000, 0 , 1)
  Z2 = rbinom(1000, 1, 0.7* Z1 + 0.3 * (1 - Z1))
  Sigma = matrix(c(1, 0.5, 0.5, 1), 2, 2)
  #Sigma = matrix(c(1,0,0,1),2,2)
  #Z_norm = mvrnorm(n=1000, rep(0, 2), Sigma)
  #Z3 = Z_norm[, 1]
  #Z4 = Z_norm[, 2]
  X1 = rnorm(1000, 1, 1)
  X2 = rnorm(1000, -1 ,1)
  Z_mat = matrix(c(Z1, Z2, X1, X2), 1000, 4)
  Z = Z_mat %*% as.numeric(beta) + rnorm(1000, 0, 1)
  A = exp(Z) / (1 + exp(Z))
  W = rbinom(1000, 1, A)
  
  #Y = W * inter + rnorm(1000, 0, 0.01) 
  Y = (1 - W) * (1.5 * X1 + X2) + W * (3 * X1) + rnorm(1000, 0, 0.1) 
  #Y[which(W=1)] = W * (3 * X1 +x2) + rnorm(1000, 0, 0.01)
  Y = Y + X1 * as.numeric(alpha[1]) + X2 * as.numeric(alpha[2]) 
  Y = Y + X1^2 * as.numeric(gama[1]) + X2^2 * as.numeric(gama[2])
  Data = data.frame(Z1, Z2, X1, X2, Treat = W, Y)
  return(Data)
} 



VarTree_data = function(alpha = rep(0.5, 2), beta = rep(0.25, 4), inter = 0, 
                     gama = rep(0.5, 4), inter34 = 0)
{
  library(MASS)
  Z1 = runif(1000, -1 , 1)
  Z2 = rbinom(1000, 1, 0.5 * (1 + Z1))
  #Sigma = matrix(c(1,0,0,1),2,2)
  #Z_norm = mvrnorm(n=1000, rep(0, 2), Sigma)
  #Z3 = Z_norm[, 1]
  #Z4 = Z_norm[, 2]
  X1 = rnorm(1000, 1, 1)
  X2 = rnorm(1000, -1 ,1)
  W = Treat_generate(Z1, Z2, X1, X2)
  Y = (1 - W) * (1.5 * X1 + X2) + W * (3 * X1) + rnorm(1000, 0, 0.1) 
  #Y[which(W=1)] = W * (3 * X1 +x2) + rnorm(1000, 0, 0.01)
  Y = Y + X1 * as.numeric(alpha[1]) + X2 * as.numeric(alpha[2]) 
  Y = Y + X1^2 * as.numeric(gama[1]) + X2^2 * as.numeric(gama[2])
  Data = data.frame(Z1, Z2, X1, X2, Treat = W, Y)
  return(Data)
} 
