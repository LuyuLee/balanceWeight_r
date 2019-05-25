Var_test_ATE = function(effect = 2.5, item = 1000,
                    Method = "Combined" ,alpha = 0.05,
                    Alpha, Beta, Gama, inter_coef = 0.5)
{
  ATE = array()
  effect = array()
  for (i in 1:item)
  {
    #set.seed(1)
    inputdata = Vardata(alpha = Alpha, beta = Beta, gama = Gamma,
                            inter34 = inter_coef)
    summary(inputdata)
    formula_prob = Treat ~ Z1 + Z2 + X1 + X2
    situation1 = add_prob(inputdata, formula_prob)
    newdata = gbsgweight(situation1, method=Method, alpha = alpha)
    effect[i] = mean(1.5*newdata$X1 - newdata$X2)
    
    ATE[i] = sum(newdata$Treat * newdata$Y * newdata$weigh) /
      sum(newdata$Treat * newdata$weigh) - 
      sum((1 - newdata$Treat) * newdata$Y * newdata$weigh) /
      sum((1 - newdata$Treat) * newdata$weigh)
  }
  bias = sum(ATE - effect) / item / mean(effect)
  std = sd(ATE)
  RMSE = (sum((ATE - effect)^2) / item)^0.5
  return(c(bias, std, RMSE))
}  


Var_treetest_ATE = function(effect = 2.5, item = 1000,
                        Method = "Combined" ,alpha = 0.05,
                        Alpha, Beta, Gama, inter_coef = 0.5)
{
  ATE = array()
  effect = array()
  for (i in 1:item)
  {
    #set.seed(1)
    inputdata = VarTree_data(alpha = Alpha, beta = Beta, gama = Gamma,
                         inter34 = inter_coef)
    summary(inputdata)
    formula_prob = Treat ~ Z1 + Z2 + X1 + X2
    situation1 = add_prob(inputdata, formula_prob)
    newdata = gbsgweight(situation1, method=Method, alpha = alpha)
    effect[i] = mean(1.5*newdata$X1 - newdata$X2)
    
    ATE[i] = sum(newdata$Treat * newdata$Y * newdata$weigh) /
      sum(newdata$Treat * newdata$weigh) - 
      sum((1 - newdata$Treat) * newdata$Y * newdata$weigh) /
      sum((1 - newdata$Treat) * newdata$weigh)
  }
  bias = sum(ATE - effect) / item / mean(effect) 
  std = sd(ATE)
  RMSE = (sum((ATE - effect)^2) / item)^0.5
  return(c(bias, std, RMSE))
}  

Var_test_algorithm = function(effect = 1, Method = 'Combined', Truncated.alpha = 0.05,
                          item = 1000, Alpha, Beta, Gama, inter_coef = 0.5, BuckN = 10,
                          BuckM = 100)
{
  bia_sum = array()
  effect = array()
  for (i in 1:item)
  {
    #set.seed(1)
    inputdata = Vardata(alpha = Alpha, beta = Beta, gama = Gamma,
                            inter34 = inter_coef, inter = effect)
    summary(inputdata)
    formula_prob = Treat ~ Z1 + Z2 + X1 + X2
    situation1 = add_prob(inputdata, formula_prob)
    
    newdata = gbsgweight(situation1, method= Method, alpha = Truncated.alpha)
    formula = Y ~  Z1 + Z2 + X1 + X2
    #set.seed(1)
    tree_new <- causalTree(formula, weight=newdata$weigh, data=newdata,
                           treatment=newdata$Treat,split.Rule = "CT", cv.option = "CT",
                           split.Honest = T,cv.Honest = T, split.Bucket = T, xval = 5, 
                           cp = 0, minsize = 40, bucketNum = BuckN, bucketMax = BuckM)
    #rpart.plot(tree_new)
    opcp <- tree_new$cptable[,1][which.min(tree_new$cptable[,4])]
    opfit <- prune(tree_new, opcp)
    #rpart.plot(opfit)
    #show(tree_new$cptable)
    df = opfit
    effect[i] = mean(1.5*newdata$X1 - newdata$X2)
    t1 = estimate_method(df)
    bia_sum[i] = t1
  }
  bias = sum(bia_sum - effect) / item / mean(effect)
  std =  sd(bia_sum)
  RMSE = (sum((bia_sum - effect)^2) / item)^0.5
  return(c(bias, std, RMSE))
}

Var_test_Treedata = function(effect = 1, Method = 'Combined', Truncated.alpha = 0.05,
                         item = 1000, Alpha, Beta, Gama, inter_coef = 0.5, BuckN = 10,
                         BuckM = 100)
{
  bia_sum = array()
  effect = array()
  for (i in 1:item)
  {
    #set.seed(1)
    inputdata = VarTree_data(alpha = Alpha, beta = Beta, gama = Gamma,
                             inter34 = inter_coef, inter = effect)
    summary(inputdata)
    formula_prob = Treat ~ Z1 + Z2 + X1 + X2
    situation1 = add_prob(inputdata, formula_prob)
    
    newdata = gbsgweight(situation1, method= Method, alpha = Truncated.alpha)
    formula = Y ~  Z1 + Z2 + X1 + X2
    #set.seed(1)
    tree_new <- causalTree(formula, weight=newdata$weigh, data=newdata,
                           treatment=newdata$Treat,split.Rule = "CT", cv.option = "CT",
                           split.Honest = T,cv.Honest = T, split.Bucket = T, xval = 5, 
                           cp = 0, minsize = 40, bucketNum = BuckN, bucketMax = BuckM)
    #rpart.plot(tree_new)
    opcp <- tree_new$cptable[,1][which.min(tree_new$cptable[,4])]
    opfit <- prune(tree_new, opcp)
    #rpart.plot(opfit)
    #show(tree_new$cptable)
    effect[i] = mean(1.5*newdata$X1 - newdata$X2)
    df = opfit
    t1 = estimate_method(df)
    bia_sum[i] = t1
  }
  bias = sum(bia_sum - effect) / item / mean(effect)
  std =  sd(bia_sum)
  RMSE = (sum((bia_sum - effect)^2) / item)^0.5
  return(c(bias, std, RMSE))
}
