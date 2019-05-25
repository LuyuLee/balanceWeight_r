gettreat_sq = function(x, n, b = 0, alpha= rep(1/n, time = n), beita, int13, int24)
{
  z = rep(b, time = 1000)
  Treat = array()
  for (i in (1 : n))
  {
    z = z + x[, i] * as.numeric(alpha[i])
    z = z + x[, i]^2 * as.numeric(beita[i])
  }
  z = z + x[,1] * x[, 3] * int13 + x[,2] + x[,4] * int24
  z_prob = exp(z) / (1 + exp(z))
  Treat[which(z_prob > 0.5)] = 1
  Treat[which(z_prob <= 0.5)] = 0
  return(data.frame(x, Treat))
}


sq_SurvivalTime = function(X, n, alpha = rep(1/n, time = n), e_random = FALSE, b = 1000, beita, int13, int24)
{
  if (e_random)
  {
    set.seed(1)
    residual = b  + rnorm(1000, 1000, 200)
  }
  else
    residual = rep(b, time = 1000)
  z = residual
  Treat = array()
  for (i in (1 : n))
  {
    z = z + x[, i] * as.numeric(alpha[i])
    z = z + x[, i]^2 * as.numeric(beita[i])
  }
  z = z + x[,1] * x[, 3] * int13 + x[,2] * x[,4] * int24
  y = as.integer(z)
  return(data.frame(X, Y = y))
}

x = simpledata(2)
Alpha = list(0.12, 0.04, 0.04, 0.01)
Beita = list(0.03, 0.02, 0.02, 0.03)
data = gettreat_sq(x, 4, alpha = Alpha, b = 0.01, beita = Beita, int13 = 0.2, int24 = 0.1)
SAlpha = list(-250, 500, 700, -400)
SBeita = list(-50, 700, 600, -140)
inputdata = sq_SurvivalTime(data, 4, SAlpha, e_random = FALSE, b = 2000, beita = SBeita, int13 = 100, int24 = 100)
summary(inputdata) 


