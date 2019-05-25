Var_treetest_ATE = function(effect = 2.5, item = 1000,
                        Method = "Combined" ,alpha = 0.05,
                        Alpha, Beta, Gama, inter_coef = 0.5)
{
  ATE = array()
  for (i in 1:item)
  {
    #set.seed(1)
    inputdata = var_data(alpha = Alpha, beta = Beta, gama = Gamma,
                         inter34 = inter_coef)
    summary(inputdata)
    formula_prob = Treat ~ Z1 + Z2 + X1 + X2
    situation1 = add_prob(inputdata, formula_prob)
    newdata = gbsgweight(situation1, method=Method, alpha = alpha)
    
    ATE[i] = sum(newdata$Treat * newdata$Y * newdata$weigh) /
      sum(newdata$Treat * newdata$weigh) - 
      sum((1 - newdata$Treat) * newdata$Y * newdata$weigh) /
      sum((1 - newdata$Treat) * newdata$weigh)
  }
  bias = sum(ATE) / item - effect
  std = sd(ATE)
  RMSE = (sum((ATE - effect)^2) / item)^0.5
  return(c(bias, std, RMSE))
}  


Var_treetest_ATE = function(effect = 2.5, item = 1000,
                            Method = "Combined" ,alpha = 0.05,
                            Alpha, Beta, Gama, inter_coef = 0.5)
{
  ATE = array()
  for (i in 1:item)
  {
    #set.seed(1)
    inputdata = var_data(alpha = Alpha, beta = Beta, gama = Gamma,
                         inter34 = inter_coef)
    summary(inputdata)
    formula_prob = Treat ~ Z1 + Z2 + X1 + X2
    situation1 = add_prob(inputdata, formula_prob)
    newdata = gbsgweight(situation1, method=Method, alpha = alpha)
    
    ATE[i] = sum(newdata$Treat * newdata$Y * newdata$weigh) /
      sum(newdata$Treat * newdata$weigh) - 
      sum((1 - newdata$Treat) * newdata$Y * newdata$weigh) /
      sum((1 - newdata$Treat) * newdata$weigh)
  }
  bias = sum(ATE) / item - effect
  std = sd(ATE)
  RMSE = (sum((ATE - effect)^2) / item)^0.5
  return(c(bias, std, RMSE))
}  
