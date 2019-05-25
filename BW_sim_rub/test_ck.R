test_algorithm_ck = function(effect = 1, Method = 'Combined', Truncated.alpha = 0.05,
                          item = 1000, Alpha, Beta, Gama, inter_coef = 0.5, BuckN = 10,
                          BuckM = 100, ck_effect = list(3, -3, 0), mul = 1)
{
  bia_sum = array()
  for (i in 1:item)
  {
    #set.seed(1)
    inputdata = get_check_Y(alpha = Alpha, beta = Beta, gama = Gamma,
                            inter34 = inter_coef, inter = ck_effect, Treegene = 0)
    summary(inputdata)
    formula_prob = Treat ~ Z1 + Z2 + X1 + X2
    situation1 = add_prob(inputdata, formula_prob)
    
    newdata = gbsgweight(situation1, method= Method, alpha = Truncated.alpha)
    formula = Y ~  Z1 + Z2 + X1 + X2
    #set.seed(1)
    tree_new <- causalTree(formula, weight=newdata$weigh * mul, data=newdata,
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
  return(c(bias, std))
}


test_Treedata_ck = function(effect = 1, Method = 'Combined', Truncated.alpha = 0.05,
                         item = 1000, Alpha, Beta, Gama, inter_coef = 0.5, BuckN = 10,
                         BuckM = 100, ck_effect = list(3, -3, 0), mul = 1)
{
  bia_sum = array()
  for (i in 1:item)
  {
    #set.seed(1)
    inputdata = get_check_Y(alpha = Alpha, beta = Beta, gama = Gamma,
                            inter34 = inter_coef, inter = ck_effect, Treegene = 1)
    summary(inputdata)
    formula_prob = Treat ~ Z1 + Z2 + X1 + X2
    situation1 = add_prob(inputdata, formula_prob)
    
    newdata = gbsgweight(situation1, method= Method, alpha = Truncated.alpha)
    formula = Y ~  Z1 + Z2 + X1 + X2
    #set.seed(1)
    tree_new <- causalTree(formula, weight=newdata$weigh * mul, data=newdata,
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
  return(c(bias, std))
}