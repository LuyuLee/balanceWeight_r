# This program tests out the features of the causalTree package

#install.packages("devtools")
library(devtools)
#install.packages("rpart", dependencies=TRUE, repos='http://cran.us.r-project.org')
#install.packages("rpart.plot", dependencies=TRUE, repos='http://cran.us.r-project.org')
#install.packages("reshape2", dependencies=TRUE, repos='http://cran.us.r-project.org')
#install.packages("plyr", dependencies=TRUE, repos='http://cran.us.r-project.org')
library(rpart)
library(rpart.plot)
# install_github("susanathey/causalTree",force=TRUE)
library(data.table)
library(causalTree)
library(reshape2)
library(plyr)
nitem = 10
CH_ans = vector(mode="list", length=nitem)
CdH_ans = vector(mode="list", length=nitem)
TH_ans = vector(mode="list", length=nitem)
TdH_ans = vector(mode="list", length=nitem)
OH_ans = vector(mode="list", length=nitem)
OdH_ans = vector(mode="list", length=nitem)
# Generate data 
# parameters for data generating
p <- 2 # number of total covariates
###different!!
pt <- 1 # number of covariates affecting treatment effects
py <- 2 # number of covariates affecting outcomes but not treatment effects
asym <- .5 # whether treatment effects are distributed asymmetrically across treated and control
###!!!
n <- 500 # total size of the dataset
###!!!
# propens <- .5 #treatment probability
sig = .01
treatsize <- .5 # treatment effect size
levsize <- 1


for (item in 1 : nitem)
{
  
  # draw X
  X <- matrix(rnorm(n * p), nrow = n, ncol = p)
  
  # draw W
  beta = rep(1/py, py)
  propens = Treat_generate(X, beta, py)
  w <- rbinom(n, 1, propens)
  w_ <- 1-w
  
  # generate treatment effects as function of X
  if ((p<pt) | (p<py)) print("error: p>=pt+py required")
  tau <- 0
  tau <- tau + treatsize * X[, 1]
  #for (iii in 1:pt) {
  #  tau <- tau + treatsize*pmax(X[,iii],array(0,n))*(2*X[,iii]) #pmax chosemax in vetors
  #}
  
  # generate average value of outcomes
  #mu <- treatsize*rowSums(X[,1:pt])+levsize*rowSums(X[,(pt+1):(pt+py)])
  mu <- treatsize*X[,1:pt]+levsize*rowSums(X[,1:py])
  
  # generate outcomes as function of treatment status, mu, tau, and noise
  y <- mu + asym*w*tau + (asym-1)*(1-w)*tau + rnorm(n,0,sig)
  y_ <- mu + asym*w_*tau + (asym-1)*(1-w_)*tau + rnorm(n,0,sig)
  
  # create formulas for estimation
  # if modifying code for another dataset, need to simply name outcome variable y, treatment variable w, and
  # create a formula such as f with the list of x variables separated by "+"
  f <- ""
  nextx <- ""
  name = ""
  if (p>1) {
    for (ii in 1:(p-1)) {
      nextx <- paste("X",ii, sep="")
      if (ii==1) {name <- nextx}
      if (ii>1) {name <- c(name, nextx)}
      f <- paste(f, nextx, "+", sep="")
    }
    f <- paste(f, "X", ii+1, sep="")
  } else if (p==1) {
    f <- "X1"
  }
  
  #get prob by logistic regression
  df = data.frame(X,w)
  probdata = getprob(df, f)
  
  for (ii in 1:p) {
    nextx <- paste("X",ii, sep="")
    if (ii==1) {name <- nextx}
    if (ii>1) {name <- c(name, nextx)}
  }
  
  name <- c( name,  "y", "w", "tau_true", "prob")
  
  tau_true <- (1-2*w)*(y_ - y)
  
  ntr <- round(.333*n)
  nest <- round(.333*n)
  ntest <- n - ntr - nest
  
  # set global parameters
  minsize.temp = 25
  split.Bucket.temp = T
  bucketNum.temp = 5
  bucketMax.temp = 100
  
  X<-data.frame(X)
  for (tmp1 in 1:ncol(X)){
    xtmp<-X[,tmp1]
    unxtmp<-unique(xtmp)
    if(length(unxtmp)<bucketMax.temp) #convert to factor
      X[,tmp1]<-factor(X[,tmp1])
  }
  weight = get_weight(probdata, method ="Truncated", alpha = 0.05)
  
  dfTrain <- data.frame(X[1:ntr,],y = y[1:ntr],w = w[1:ntr],tau_true = tau_true[1:ntr],
                        prob = probdata$prob[1:ntr])
  dfEst <- data.frame(X[(ntr+1):(ntr+nest),],y =  y[(ntr+1):(ntr+nest)],
                      w =  w[(ntr+1):(ntr+nest)],tau_true =  tau_true[(ntr+1):(ntr+nest)],
                      prob = probdata$prob[(ntr+1):(ntr+nest)])
  dfTest <- data.frame(X[(ntr+nest+1):n,],y =  y[(ntr+nest+1):n],
                       w = w[(ntr+nest+1):n], tau_true = tau_true[(ntr+nest+1):n],
                       prob = probdata$prob[(ntr+nest+1):n])
  dataTest = dfTest
  df = data.frame(X[1:(ntr+nest),],y =  y[1:(ntr+nest)],
                  w =  w[1:(ntr+nest)],tau_true =  tau_true[1:(ntr+nest)],
                  prob = probdata$prob[1:(ntr+nest)])
  
  names(dfTrain)=name
  names(dfEst)=name
  names(dfTest)=name
  names(df) = name
  
  dataTest_c = dfTest
  dataTest_t = data.frame(dfTest, weight = weight[(ntr+nest+1):n])
  weight = get_weight(probdata, method ="Overlap", alpha = 0.05)
  dataTest_o = data.frame(dfTest, weight = weight[(ntr+nest+1):n])
  
  
  
  Tree_honest_C <- vector(mode="list", length=4)
  Tree_dishonest_c <- vector(mode="list", length=4)
  Tree_honest_Truncation <- vector(mode="list", length=4)
  Tree_dishonest_Truncation <- vector(mode="list", length=4)
  Tree_honest_Overlap <- vector(mode="list", length=4)
  Tree_dishonest_Overlap <- vector(mode="list", length=4)
  
  ####Build TREE###
  #ct  
  Tree_list_c = buildTree(split.Rule.temp = "CT", cv.option.temp = "CT",
                             Method = "Control", n = n, formula = f,
                             df = df, dfTrain = dfTrain, dfEst = dfEst, probdata = probdata)
  Tree_honest_C[[1]] = Tree_list_c
  Tree_list_c = builddishonestTree(split.Rule.temp = "CT", cv.option.temp = "CT",
                          Method = "Control", n = n, formula = f,
                          df = df, dfTrain = dfTrain, dfEst = dfEst, probdata = probdata)
  Tree_dishonest_c[[1]] = Tree_list_c
  
  Tree_list_t = buildTree(split.Rule.temp = "CT", cv.option.temp = "CT",
                              Method = "Truncated", n = n, formula = f,
                              df = df, dfTrain = dfTrain, dfEst = dfEst, probdata = probdata)
  Tree_honest_Truncation[[1]] = Tree_list_t
  Tree_list_t = builddishonestTree(split.Rule.temp = "CT", cv.option.temp = "CT",
                          Method = "Truncated", n = n, formula = f,
                          df = df, dfTrain = dfTrain, dfEst = dfEst, probdata = probdata)
  Tree_dishonest_Truncation[[1]] = Tree_list_t
  
  Tree_list_ct = buildTree(split.Rule.temp = "CT", cv.option.temp = "CT",
                            Method = "Overlap", n = n, formula = f,
                            df = df, dfTrain = dfTrain, dfEst = dfEst, probdata = probdata)
  Tree_honest_Overlap[[1]]= Tree_list_ct
  Tree_list_ct = builddishonestTree(split.Rule.temp = "CT", cv.option.temp = "CT",
                           Method = "Overlap", n = n, formula = f,
                           df = df, dfTrain = dfTrain, dfEst = dfEst, probdata = probdata)
  Tree_dishonest_Overlap[[1]] = Tree_list_ct
  
  #tstats 
  Tree_list_c = buildTree(split.Rule.temp = "tstats", cv.option.temp = "CT",
                             Method = "Control", n = n, formula = f,
                             df = df, dfTrain = dfTrain, dfEst = dfEst, probdata = probdata)
  Tree_honest_C[[2]] = Tree_list_c
  Tree_list_c = builddishonestTree(split.Rule.temp = "tstats", cv.option.temp = "CT",
                          Method = "Control", n = n, formula = f,
                          df = df, dfTrain = dfTrain, dfEst = dfEst, probdata = probdata)
  Tree_dishonest_c[[2]] = Tree_list_c
  
  Tree_list_t = buildTree(split.Rule.temp = "tstats", cv.option.temp = "CT",
                             Method = "Truncated", n = n, formula = f,
                             df = df, dfTrain = dfTrain, dfEst = dfEst, probdata = probdata)
  Tree_honest_Truncation[[2]] = Tree_list_t
  Tree_list_t = builddishonestTree(split.Rule.temp = "tstats", cv.option.temp = "CT",
                          Method = "Truncated", n = n, formula = f,
                          df = df, dfTrain = dfTrain, dfEst = dfEst, probdata = probdata)
  Tree_dishonest_Truncation[[2]] = Tree_list_t
  
  Tree_list_o = buildTree(split.Rule.temp = "tstats", cv.option.temp = "CT",
                              Method = "Overlap", n = n, formula = f,
                              df = df, dfTrain = dfTrain, dfEst = dfEst, probdata = probdata)
  Tree_honest_Overlap[[2]] = Tree_list_o
  Tree_list_o = builddishonestTree(split.Rule.temp = "tstats", cv.option.temp = "CT",
                          Method = "Overlap", n = n, formula = f,
                          df = df, dfTrain = dfTrain, dfEst = dfEst, probdata = probdata)
  Tree_dishonest_Overlap[[2]] = Tree_list_o
  
  # fit
  Tree_list_c = buildTree(split.Rule.temp = "fit", cv.option.temp = "fit",
                             Method = "Control", n = n, formula = f,
                             df = df, dfTrain = dfTrain, dfEst = dfEst, probdata = probdata)
  Tree_honest_C[[3]] = Tree_list_c
  Tree_list_c = builddishonestTree(split.Rule.temp = "fit", cv.option.temp = "fit",
                          Method = "Control", n = n, formula = f,
                          df = df, dfTrain = dfTrain, dfEst = dfEst, probdata = probdata)
  Tree_dishonest_c[[3]] = Tree_list_c
  
  Tree_list_t = buildTree(split.Rule.temp = "fit", cv.option.temp = "fit",
                             Method = "Truncated", n = n, formula = f,
                             df = df, dfTrain = dfTrain, dfEst = dfEst, probdata = probdata)
  Tree_honest_Truncation[[3]] = Tree_list_t
  Tree_list_t = builddishonestTree(split.Rule.temp = "fit", cv.option.temp = "fit",
                          Method = "Truncated", n = n, formula = f,
                          df = df, dfTrain = dfTrain, dfEst = dfEst, probdata = probdata)
  Tree_dishonest_Truncation[[3]] = Tree_list_t
  
  Tree_list_o = buildTree(split.Rule.temp = "fit", cv.option.temp = "fit",
                              Method = "Overlap", n = n, formula = f,
                              df = df, dfTrain = dfTrain, dfEst = dfEst, probdata = probdata)
  Tree_honest_Overlap[[3]] = Tree_list_o
  Tree_list_o = builddishonestTree(split.Rule.temp = "fit", cv.option.temp = "fit",
                          Method = "Overlap", n = n, formula = f,
                          df = df, dfTrain = dfTrain, dfEst = dfEst, probdata = probdata)
  Tree_dishonest_Overlap[[3]] = Tree_list_o
  
  #ToT
  Tree_list_c = buildTree(split.Rule.temp = "TOT", cv.option.temp = "TOT",
                             Method = "Control", n = n, formula = f,
                             df = df, dfTrain = dfTrain, dfEst = dfEst, probdata = probdata)
  Tree_honest_C[[4]] = Tree_list_c
  Tree_list_c = builddishonestTree(split.Rule.temp = "TOT", cv.option.temp = "TOT",
                          Method = "Control", n = n, formula = f,
                          df = df, dfTrain = dfTrain, dfEst = dfEst, probdata = probdata)
  Tree_dishonest_c[[4]] = Tree_list_c
  
  Tree_list_t = buildTree(split.Rule.temp = "TOT", cv.option.temp = "TOT",
                             Method = "Truncated", n = n, formula = f,
                             df = df, dfTrain = dfTrain, dfEst = dfEst, probdata = probdata)
  Tree_honest_Truncation[[4]] = Tree_list_t
  Tree_list_t = builddishonestTree(split.Rule.temp = "TOT", cv.option.temp = "TOT",
                          Method = "Truncated", n = n, formula = f,
                          df = df, dfTrain = dfTrain, dfEst = dfEst, probdata = probdata)
  Tree_dishonest_Truncation[[4]] = Tree_list_t
  
  Tree_list_o = buildTree(split.Rule.temp = "TOT", cv.option.temp = "TOT",
                              Method = "Overlap", n = n, formula = f,
                              df = df, dfTrain = dfTrain, dfEst = dfEst, probdata = probdata)
  Tree_honest_Overlap[[4]] = Tree_list_o
  Tree_list_o = builddishonestTree(split.Rule.temp = "TOT", cv.option.temp = "TOT",
                          Method = "Overlap", n = n, formula = f,
                          df = df, dfTrain = dfTrain, dfEst = dfEst, probdata = probdata)
  Tree_dishonest_Overlap[[4]] = Tree_list_o
  
  #pre
  MSEtau_C_honest <- array(0,4)
  MSEtau_C_dishonest <- array(0,4)
  MSEtau_T_honest <- array(0,4)
  MSEtau_T_dishonest <- array(0,4)
  MSEtau_O_honest <- array(0,4)
  MSEtau_O_dishonest <- array(0,4)
  for (i in 1:4) 
  {
    # use the predictions from honest trees on the Test set with C
    predicthonest_C = predict(Tree_honest_C[[i]],newdata=dataTest_c,type="vector")
    # use the predictions from dishonest trees on Test set with C
    predictdishonest_C = predict(Tree_dishonest_c[[i]],newdata=dataTest_c,type="vector")
    # use the predictions from honest trees on the Test set with Truncation
    predicthonest_Truncation = predict(Tree_honest_Truncation[[i]],newdata=dataTest_t,type="vector")
    # use the predictions from dishonest trees on Test set with Truncation
    predictdishonest_Truncation = predict(Tree_dishonest_Truncation[[i]],newdata=dataTest_t,type="vector")
    # use the predictions from honest trees on the Test set with Overlap
    predicthonest_Overlap = predict(Tree_honest_Overlap[[i]],newdata=dataTest_o,type="vector")
    # use the predictions from dishonest trees on Test set with Overlap
    predictdishonest_Overlap = predict(Tree_dishonest_Overlap[[i]],newdata=dataTest_o,type="vector")
    
    # this is simulated data, so we know the true tau for each individual.  This is infeasible MSE
    MSEtau_C_honest[[i]] = mean((predicthonest_C - dataTest$tau_true)^2, na.rm=T)
    MSEtau_C_dishonest[[i]] = mean((predictdishonest_C - dataTest$tau_true)^2, na.rm=T)
    #truncation
    MSEtau_T_honest[[i]] = mean((predicthonest_Truncation - dataTest$tau_true)^2, na.rm=T)
    MSEtau_T_dishonest[[i]] = mean((predictdishonest_Truncation - dataTest$tau_true)^2, na.rm=T)
    #Overlap
    MSEtau_O_honest[[i]] = mean((predicthonest_Overlap - dataTest$tau_true)^2, na.rm=T)
    MSEtau_O_dishonest[[i]] = mean((predictdishonest_Overlap - dataTest$tau_true)^2, na.rm=T)
    
  }
  CH_ans[[item]] = MSEtau_C_honest
  TH_ans[[item]] = MSEtau_T_honest
  OH_ans[[item]] = MSEtau_O_honest
  CdH_ans[[item]] = MSEtau_C_dishonest
  TdH_ans[[item]] = MSEtau_T_dishonest
  OdH_ans[[item]] = MSEtau_O_dishonest
}

print("Control MSE(tau)'s for honest CT, tstats, fit, TOT")
ans_ch = get_ave(CH_ans, nitem)
std_ch = ans_ch[2]
MSE_ch = ans_ch[1]

print("Truncation MSE(tau)'s for honest CT, tstats, fit, TOT")
ans_th = get_ave(TH_ans, nitem)
std_th = ans_th[2]
MSE_th = ans_th[1]

print("Overlap MSE(tau)'s for honest CT, tstats, fit, TOT")
ans_oh = get_ave(OH_ans, nitem)
std_oh = ans_oh[2]
MSE_oh = ans_oh[1]

print("____________________________________________")

print("Control MSE(tau)'s for dishonest CT, tstats, fit, TOT")
ans_cdh = get_ave(CdH_ans, nitem)
std_cdh = ans_cdh[2]
MSE_cdh = ans_cdh[1]

print("Truncation MSE(tau)'s for dishonest CT, tstats, fit, TOT")
ans_tdh = get_ave(TdH_ans, nitem)
std_tdh = ans_tdh[2]
MSE_tdh = ans_tdh[1]

print("Overlap MSE(tau)'s for dishonest CT, tstats, fit, TOT")
ans_odh = get_ave(OdH_ans, nitem)
std_odh = ans_odh[2]
MSE_odh = ans_odh[1]
rowname = c("CT", "tstats", "fit", "TOT")
MSE = data.frame(MSE_ch, MSE_th, MSE_oh, MSE_cdh, MSE_tdh, MSE_odh)
names(MSE) =c("MSE_ch", "MSE_th", "MSE_oh", "MSE_cdh", "MSE_tdh", "MSE_odh") 
row.names(MSE) = rowname
std = data.frame(std_ch, std_th, std_oh, std_cdh, std_tdh, std_odh )
names(std) =c("std_ch", "std_th", "std_oh", "std_cdh", "std_tdh", "std_odh")
row.names(std) = rowname
ans =data.frame(MSE, std)
# write.csv(ans,"ans_s2_1st.csv")
