buildTree = function(split.Rule.temp = "CT", cv.option.temp = "CT", 
                     split.Honest.temp = T, cv.Honest.temp = T, split.alpha.temp = .5,
                     cv.alpha.temp = .5, bucketNum.temp = 5, bucketMax.temp = 100,
                     minsize.temp = 25, split.Bucket.temp = T, Method = "Truncated",
                     dfTrain, dfEst, df, formula, n, probdata)
{
  ntr <- round(.333*n)
  nest <- round(.333*n)
  ntest <- n - ntr - nest
  weight = get_weight(probdata, method =Method, alpha = 0.05)
  dataTrain = data.frame(dfTrain, weight = weight[1:ntr])
  dataEst = data.frame(dfEst, weight = weight[(ntr+1):(ntr+nest)])
  datadish = data.frame(df, weight = weight[1:ntr + nest])
  #honest
  xvalvec = sample(5, nrow(dataTrain), replace=TRUE)
  if (split.Rule.temp == "TOT")
  {
    split.Honest.temp = F
    cv.Honest.temp = F
    split.alpha.temp = 1
  }
  if (split.Rule.temp == "tstats")
    split.Honest.temp = F
  tree <- honest.causalTree(as.formula(paste("y~",paste(f))),
                            data=dataTrain, treatment=dataTrain$w, weights = dataTrain$weight,
                            est_data=dataEst, est_treatment=dataEst$w,est_weights = dataEst$weight,
                            split.Rule=split.Rule.temp, split.Honest=T, split.Bucket=split.Bucket.temp, bucketNum = bucketNum.temp,
                            bucketMax = bucketMax.temp, cv.option=cv.option.temp, cv.Honest=cv.Honest.temp, minsize = minsize.temp,
                            split.alpha = split.alpha.temp, cv.alpha = cv.alpha.temp, xval=xvalvec, HonestSampleSize=nest, cp=0)
  opcpid <- which.min(tree$cp[,4])
  opcp <- tree$cp[opcpid,1]
  tree_prune <- prune(tree, cp = opcp)
  tree_honest = tree_prune
  
  # output 
  return(tree_honest)
  #return(list(tree_honest, tree_dishonest))
}

builddishonestTree = function(split.Rule.temp = "CT", cv.option.temp = "CT", 
                     split.Honest.temp = F, cv.Honest.temp = F, split.alpha.temp = 1,
                     cv.alpha.temp = .5, bucketNum.temp = 5, bucketMax.temp = 100,
                     minsize.temp = 25, split.Bucket.temp = T, Method = "Truncated",
                     dfTrain, dfEst, df, formula, n, probdata)
{
  ntr <- round(.333*n)
  nest <- round(.333*n)
  ntest <- n - ntr - nest
  weight = get_weight(probdata, method =Method, alpha = 0.05)
  datadish = data.frame(df, weight = weight[1:ntr + nest])
  if (split.Rule.temp == "fit")
    split.alpha.temp = .5
  xvalvec = sample(5, nrow(datadish), replace=TRUE)
  tree <- causalTree(as.formula(paste("y~",paste(formula))), 
                     data=datadish, treatment=datadish$w, weights = datadish$weight,
                     split.Rule=split.Rule.temp, split.Honest=F, split.Bucket=split.Bucket.temp, bucketNum = bucketNum.temp, 
                     bucketMax = bucketMax.temp, cv.option=cv.option.temp, cv.Honest=cv.Honest.temp, minsize = minsize.temp, 
                     split.alpha = split.alpha.temp, xval=xvalvec, HonestSampleSize=nest, cp=0)
  opcpid <- which.min(tree$cp[,4])
  opcp <- tree$cp[opcpid,1]
  tree_prune <- prune(tree, cp = opcp)
  tree_dishonest = tree_prune
  
  return(tree_dishonest)
}