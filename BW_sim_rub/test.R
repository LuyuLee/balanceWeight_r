test_algorithm = function(effect = 1, Method = 'Combined', Truncated.alpha = 0.05,
                          item = 1000, Alpha, Beta, Gama, inter_coef = 0.5, BuckN = 10,
                          BuckM = 100)
{
  bia_sum = array()
  for (i in 1:item)
  {
    #set.seed(1)
    inputdata = noprob_data(alpha = Alpha, beta = Beta, gama = Gamma,
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
    t1 = estimate_method(df)
    bia_sum[i] = t1
  }
  bias = sum(bia_sum) / item - effect
  std =  sd(bia_sum)
  RMSE = (sum((bia_sum - effect)^2) / item)^0.5
  return(c(bias, std, RMSE))
}


test_Treedata = function(effect = 1, Method = 'Combined', Truncated.alpha = 0.05,
                          item = 1000, Alpha, Beta, Gama, inter_coef = 0.5, BuckN = 10,
                          BuckM = 100)
{
  bia_sum = array()
  for (i in 1:item)
  {
    #set.seed(1)
    inputdata = Tree_data(alpha = Alpha, beta = Beta, gama = Gamma,
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
    t1 = estimate_method(df)
    bia_sum[i] = t1
  }
  bias = sum(bia_sum) / item - effect
  std =  sd(bia_sum)
  RMSE = (sum((bia_sum - effect)^2) / item)^0.5
  return(c(bias, std, RMSE))
}


test_ATE = function(effect = 1, item = 1000, 
                    Alpha, Beta, Gama, inter_coef = 0.5)
{
  ATE = array()
  for (i in 1:item)
  {
    #set.seed(1)
    inputdata = noprob_data(alpha = Alpha, beta = Beta, gama = Gamma,
                            inter34 = inter_coef, inter = effect)
    summary(inputdata)
    formula_prob = Treat ~ Z1 + Z2 + X1 + X2
    situation1 = add_prob(inputdata, formula_prob)
    ATE[i] = sum(situation1$Treat * situation1$Y / situation1$prob) /
      sum(situation1$Treat / situation1$prob) - 
      sum((1 - situation1$Treat) * situation1$Y / (1 - situation1$prob)) /
      sum((1 - situation1$Treat) / (1 - situation1$prob))
  }
  bias = sum(ATE) / item - effect
  std = sd(ATE)
  RMSE = (sum((ATE - effect)^2) / item)^0.5
  return(c(bias, std, RMSE))
}  


test_Treedata_ATE = function(effect = 1, item = 1000, 
                             Alpha, Beta, Gama, inter_coef = 0.5)
{
  ATE = array()
  for (i in 1:item)
  {
    #set.seed(1)
    inputdata = Tree_data(alpha = Alpha, beta = Beta, gama = Gamma,
                          inter34 = inter_coef, inter = effect)
    summary(inputdata)
    formula_prob = Treat ~ Z1 + Z2 + X1 + X2
    situation1 = add_prob(inputdata, formula_prob)
    ATE[i] = sum(situation1$Treat * situation1$Y / situation1$prob) /
      sum(situation1$Treat / situation1$prob) - 
      sum((1 - situation1$Treat) * situation1$Y / (1 - situation1$prob)) /
      sum((1 - situation1$Treat) / (1 - situation1$prob))
  }
  bias = sum(ATE) / item - effect
  std = sd(ATE)
  RMSE = (sum((ATE - effect)^2) / item)^0.5
  return(c(bias, std, RMSE))
}  
