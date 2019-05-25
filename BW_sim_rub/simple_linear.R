simpledata = function(m) 
{
  set.seed(1)
  {
    x1 = rnorm(mean = 0, sd = 1, n = 1000)
    x2 = runif(1000, -1, 1)
    # x1_norm = (x1 - min(x1))/(max(x1) - min(x1))
    x = data.frame(x_norm = x1, x_unif = x2)
  }
  for (i in 1 : (m - 1))
  {
    x1 = rnorm(mean = 0, sd = 1, n = 1000)
    x2 = runif(1000, -1, 1)
    name1 = paste('x', i - 1, sep = '')
    name2 = paste('x', i, sep = '')
    x = data.frame(x,x_norm = x1, x_unif = x2)
  }
  return(x)
}

gettreat = function(x, n, b = 0, alpha= rep(1/n, time = n))
{
  z = rep(b, time = 1000)
  Treat = array()
  for (i in (1 : n))
  {
    z = z + x[, i] * as.numeric(alpha[i])
  }
  z_prob = exp(z) / (1 + exp(z))
  Treat[which(z_prob > 0.5)] = 1
  Treat[which(z_prob <= 0.5)] = 0
  return(data.frame(x, Treat))
}


getSurvivalTime = function(X, n, alpha = rep(1/n, time = n), e_random = FALSE, b = 2500)
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
  }
  y = as.integer(z)
  return(data.frame(X, Y = y))
}

x = simpledata(2)
Alpha = list(0.22, 0.14, 0.12, 0.08)
data = gettreat(x, 4, alpha = Alpha, b = 0.01)
Belta = list(-450, 400, 300, -600)
inputdata = getSurvivalTime(data, 4, Belta, e_random = FALSE)
# summary(inputdata) 


