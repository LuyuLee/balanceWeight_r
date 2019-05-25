df = Data
num_sample = 400
effect =3
shuffledata = function(df)
{
  rorder = runif(1000,0,1)
  df_order = data.frame(df, rorder)
  df_order = df_order[order(df_order$rorder),]
  df = subset(df_order, select = -rorder )
  return (df)
}


get_check_Y = function(alpha = rep(0.5, 2), beta = rep(0.25, 4), 
                       inter = list(3, -3, 0), 
                       gama = rep(0.5, 4), inter34 = 0, Treegene = 0)
{
  Z1 = runif(1000, -1 , 1)
  Z2 = rbinom(1000, 1, 0.5 * (1 + Z1))
  X1 = rnorm(1000, 1, 1)
  X2 = rnorm(1000, -1 ,1)
  if (Treegene == 0)
  {
    Z_mat = matrix(c(Z1, Z2, X1, X2), 1000, 4)
    Z = Z_mat %*% as.numeric(beta) + rnorm(1000, 0, 1)
    A = exp(Z) / (1 + exp(Z))
    W = rbinom(1000, 1, A)
  }
  if (Treegene == 1)
    W = Treat_generate(Z1, Z2, X1, X2)
  
  Data = data.frame(Z1, Z2, X1, X2, Treat = W)
  YData = shuffledata(Data)
  D1 = YData[1:400, ]
  D2 = YData[401:800, ]
  D3 = YData[801:1000, ]
  Y1 = D1$Treat * inter[1] + rnorm(400, 0, 0.01) 
  Y1 = Y1 + D1$X1 * as.numeric(alpha[1]) + D1$X2 * as.numeric(alpha[2])
  Y1 = Y1 + D1$X1^2 * as.numeric(gama[1]) + D1$X2^2 * as.numeric(gama[2])+inter34*D1$X1*D1$X2
  
  Y2 = D2$Treat * inter[2] + rnorm(400, 0, 0.01) 
  Y2 = Y2 + D2$X1 * as.numeric(alpha[1]) + D2$X2 * as.numeric(alpha[2])
  Y2 = Y2 + D2$X1^2 * as.numeric(gama[1]) + D2$X2^2 * as.numeric(gama[2])+inter34*D2$X1*D2$X2
  
  Y3 = D3$Treat * inter[3] + rnorm(200, 0, 0.01) 
  Y3 = Y3 + D3$X1 * as.numeric(alpha[1]) + D3$X2 * as.numeric(alpha[2])
  Y3 = Y3 + D3$X1^2 * as.numeric(gama[1]) + D3$X2^2 * as.numeric(gama[2])+inter34*D3$X1*D3$X2
  Data = data.frame(YData, Y = c(Y1, Y2, Y3))
  return(Data)
}


