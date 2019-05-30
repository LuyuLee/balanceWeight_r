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


# Generate data 
# parameters for data generating
p <- 2 # number of total covariates
###different!!
pt <- 1 # number of covariates affecting treatment effects
py <- 2 # number of covariates affecting outcomes but not treatment effects
asym <- .5 # whether treatment effects are distributed asymmetrically across treated and control
###!!!
n <- 1000 # total size of the dataset
###!!!
propens <- .5 #treatment probability
sig = .01
treatsize <- .5 # treatment effect size
levsize <- 1

# draw W
w <- rbinom(n, 1, propens)
w_ <- 1-w

# draw X
X <- matrix(rnorm(n * p), nrow = n, ncol = p)

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
  nextx <- paste("x",ii, sep="")
  if (ii==1) {name <- nextx}
  if (ii>1) {name <- c(name, nextx)}
}

name <- c( name,  "y", "w", "tau_true")

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
#weight = get_weight(probdata, method =Method, alpha = 0.05)

dfTrain <- data.frame(X[1:ntr,],y = y[1:ntr],w = w[1:ntr],tau = tau_true[1:ntr],
                        prob = probdata$prob[1:ntr])
dfEst <- data.frame(X[(ntr+1):(ntr+nest),],y =  y[(ntr+1):(ntr+nest)],
                      w =  w[(ntr+1):(ntr+nest)],tau =  tau_true[(ntr+1):(ntr+nest)],
                      prob = probdata$prob[(ntr+1):(ntr+nest)])
dfTest <- data.frame(X[(ntr+nest+1):n,],y =  y[(ntr+nest+1):n],
                       w[(ntr+nest+1):n], tau_true[(ntr+nest+1):n],
                       prob = probdata$prob[(ntr+nest+1):n])

names(dataTrain)=name
names(dataEst)=name
names(dataTest)=name

Tree_honest_C <- vector(mode="list", length=4)
tree_dishonest_c <- vector(mode="list", length=4)
Tree_honest_Truncation <- vector(mode="list", length=4)
tree_dishonest_Truncation <- vector(mode="list", length=4)
Tree_honest_Overlap <- vector(mode="list", length=4)
tree_dishonest_Overlap <- vector(mode="list", length=4)

# preselect cross-validation groups to remove randomness in comparing methods
xvalvec = sample(5, nrow(dataTrain), replace=TRUE)
#xvalvec=5

# Do causal tree estimation
split.Rule.temp = "CT" #CT
cv.option.temp = "CT" #CT
split.Honest.temp = T
cv.Honest.temp = T
split.alpha.temp = .5
cv.alpha.temp = .5
Method_T = "Truncated"
Method_O = "Overlap"

#Truncation weight
weight = get_weight(probdata, method =Method_T, alpha = 0.05)
dataTrain_t = data.frame(dfTrain, weight = weight[1:ntr])
dataEst_t = data.frame(dfEst, weight = weight[(ntr+1):(ntr+nest)])

#This function is a wrapper for honest causal tree
tree <- honest.causalTree(as.formula(paste("y~",paste(f))),
                          data=dataTrain_t, treatment=dataTrain_t$w, weights = dataTrain_t$weight,
                          est_data=dataEst_t, est_treatment=dataEst_t$w,est_weights = dataEst_t$weight,
                          split.Rule=split.Rule.temp, split.Honest=T, split.Bucket=split.Bucket.temp, bucketNum = bucketNum.temp,
                          bucketMax = bucketMax.temp, cv.option=cv.option.temp, cv.Honest=cv.Honest.temp, minsize = minsize.temp,
                          split.alpha = split.alpha.temp, cv.alpha = cv.alpha.temp, xval=xvalvec, HonestSampleSize=nest, cp=0)
#You can still prune as usual; the cptable is the one from training the tree
opcpid <- which.min(tree$cp[,4])
opcp <- tree$cp[opcpid,1]
tree_prune <- prune(tree, cp = opcp)
Tree_honest_Truncation[[1]] = tree_prune

#Control weight
dataTrain = dfTrain
dataEst = dfEst
#This function is a wrapper for honest causal tree
tree <- honest.causalTree(as.formula(paste("y~",paste(f))),
                          data=dataTrain, treatment=dataTrain$w,
                          est_data=dataEst, est_treatment=dataEst$w,
                          split.Rule=split.Rule.temp, split.Honest=T, split.Bucket=split.Bucket.temp, bucketNum = bucketNum.temp,
                          bucketMax = bucketMax.temp, cv.option=cv.option.temp, cv.Honest=cv.Honest.temp, minsize = minsize.temp,
                          split.alpha = split.alpha.temp, cv.alpha = cv.alpha.temp, xval=xvalvec, HonestSampleSize=nest, cp=0)
#You can still prune as usual; the cptable is the one from training the tree
opcpid <- which.min(tree$cp[,4])
opcp <- tree$cp[opcpid,1]
tree_prune <- prune(tree, cp = opcp)
Tree_honest_C[[1]] = tree_prune

#Overlap weight
weight = get_weight(probdata, method =Method_O, alpha = 0.05)
dataTrain_o = data.frame(dfTrain, weight = weight[1:ntr])
dataEst_o = data.frame(dfEst, weight = weight[(ntr+1):(ntr+nest)])

#This function is a wrapper for honest causal tree
tree <- honest.causalTree(as.formula(paste("y~",paste(f))),
                          data=dataTrain_o, treatment=dataTrain_o$w, weights = dataTrain_o$weight,
                          est_data=dataEst_o, est_treatment=dataEst_o$w, est_weights = dataEst_o$weight,
                          split.Rule=split.Rule.temp, split.Honest=T, split.Bucket=split.Bucket.temp, bucketNum = bucketNum.temp,
                          bucketMax = bucketMax.temp, cv.option=cv.option.temp, cv.Honest=cv.Honest.temp, minsize = minsize.temp,
                          split.alpha = split.alpha.temp, cv.alpha = cv.alpha.temp, xval=xvalvec, HonestSampleSize=nest, cp=0)
#You can still prune as usual; the cptable is the one from training the tree
opcpid <- which.min(tree$cp[,4])
opcp <- tree$cp[opcpid,1]
tree_prune <- prune(tree, cp = opcp)
Tree_honest_Overlap[[1]] = tree_prune

#dishonest
split.Rule.temp = "CT" #CT
cv.option.temp = "CT" #CT
split.Honest.temp = F
cv.Honest.temp = F
split.alpha.temp = .5
cv.alpha.temp = .5
df = data.frame(X[1:(ntr+nest),],y =  y[1:(ntr+nest)],
                w =  w[1:(ntr+nest)],tau =  tau_true[1:(ntr+nest)],
                prob = probdata$prob[1:(ntr+nest)])
xvalvec = sample(5, nrow(df), replace=TRUE)

#Control
tree <- causalTree(as.formula(paste("y~",paste(f))), 
                   data=df, treatment=df$w, 
                   split.Rule=split.Rule.temp, split.Honest=F, split.Bucket=split.Bucket.temp, bucketNum = bucketNum.temp, 
                   bucketMax = bucketMax.temp, cv.option=cv.option.temp, cv.Honest=cv.Honest.temp, minsize = minsize.temp, 
                   split.alpha = split.alpha.temp, cv.alpha = cv.alpha.temp, xval=xvalvec, HonestSampleSize=nest, cp=0)
opcpid <- which.min(tree$cp[,4])
opcp <- tree$cp[opcpid,1]
tree_prune <- prune(tree, cp = opcp) 
tree_dishonest_c[[1]] <- tree_prune

#Truncation
weight = get_weight(probdata, method =Method_T, alpha = 0.05)
dataTrain_t = data.frame(df, weight = weight[1:ntr + nest])
tree <- causalTree(as.formula(paste("y~",paste(f))), 
                   data=dataTrain_t, treatment=dataTrain_t$w, weights = dataTrain_t$weight,
                   split.Rule=split.Rule.temp, split.Honest=F, split.Bucket=split.Bucket.temp, bucketNum = bucketNum.temp, 
                   bucketMax = bucketMax.temp, cv.option=cv.option.temp, cv.Honest=cv.Honest.temp, minsize = minsize.temp, 
                   split.alpha = split.alpha.temp, cv.alpha = cv.alpha.temp, xval=xvalvec, HonestSampleSize=nest, cp=0)
opcpid <- which.min(tree$cp[,4])
opcp <- tree$cp[opcpid,1]
tree_prune <- prune(tree, cp = opcp) 
tree_dishonest_Truncation[[1]] <- tree_prune

#Overlap
weight = get_weight(probdata, method =Method_O, alpha = 0.05)
dataTrain_t = data.frame(df, weight = weight[1:ntr + nest])
tree <- causalTree(as.formula(paste("y~",paste(f))), 
                   data=dataTrain_t, treatment=dataTrain_t$w, weights = dataTrain_t$weight,
                   split.Rule=split.Rule.temp, split.Honest=F, split.Bucket=split.Bucket.temp, bucketNum = bucketNum.temp, 
                   bucketMax = bucketMax.temp, cv.option=cv.option.temp, cv.Honest=cv.Honest.temp, minsize = minsize.temp, 
                   split.alpha = split.alpha.temp, cv.alpha = cv.alpha.temp, xval=xvalvec, HonestSampleSize=nest, cp=0)
opcpid <- which.min(tree$cp[,4])
opcp <- tree$cp[opcpid,1]
tree_prune <- prune(tree, cp = opcp) 
tree_dishonest_Overlap[[1]] <- tree_prune


Tree_honest_C <- vector(mode="list", length=4)
tree_dishonest_c <- vector(mode="list", length=4)
Tree_honest_Truncation <- vector(mode="list", length=4)
tree_dishonest_Truncation <- vector(mode="list", length=4)
Tree_honest_Overlap <- vector(mode="list", length=4)
tree_dishonest_Overlap <- vector(mode="list", length=4)



ystar = dataTest$y*(dataTest$w-propens)/(propens*(1-propens))
for (i in 1:4) {
  # use the predictions from honest trees on the Test set with C
  predicthonest_C = predict(Tree_honest_C[[i]],newdata=dataTest,type="vector")
  # use the predictions from dishonest trees on Test set with C
  predictdishonest_C = predict(tree_dishonest_c[[i]],newdata=dataTest,type="vector")
  # use the predictions from honest trees on the Test set with Truncation
  predicthonest_Truncation = predict(Tree_honest_Truncation[[i]],newdata=dataTest,type="vector")
  # use the predictions from dishonest trees on Test set with Truncation
  predictdishonest_Truncation = predict(tree_dishonest_Truncation[[i]],newdata=dataTest,type="vector")
  # use the predictions from honest trees on the Test set with Overlap
  predicthonest_Overlap = predict(Tree_honest_Overlap[[i]],newdata=dataTest,type="vector")
  # use the predictions from dishonest trees on Test set with Overlap
  predictdishonest_Overlap = predict(tree_dishonest_Overlap[[i]],newdata=dataTest,type="vector")

  # this is simulated data, so we know the true tau for each individual.  This is infeasible MSE
  MSEtau_infeas_honest[[i]] = mean((predicthonest - dataTest$tau_true)^2, na.rm=T)
  MSEtau_infeas_dishonest[[i]] = mean((predictdishonest - dataTest$tau_true)^2, na.rm=T)
  
  # look at how the different partitions do predicting outcomes within a leaf
  dataTest$leaff <- as.factor(round(predicthonest,4))
  
  #honest estimation of predicted y
  dataEst$leaff <- as.factor(round(predict(tree_honest_prune_list[[i]], newdata=dataEst,type="vector"),4))
  yPredHonestTable <- melt(tapply(dataEst$y, list(dataEst$leaff, dataEst$w), mean), varnames=c("leaff","w"))
  yPredHonestTable <- rename(yPredHonestTable,replace=c("value"="ypredhon"))
  dataTest <- merge(dataTest,yPredHonestTable, by.x=c("leaff", "w"), by.y=c("leaff","w"))
  MSEy_honest[[i]] = mean((dataTest$ypredhon-dataTest$y)^2, na.rm=T)
  
  #dishonest estimation of predicted y
  dataTest$leaffd <- as.factor(round(predictdishonest,4))
  dataTrain$leaffd <- as.factor(round(predict(tree_dishonest_prune_list[[i]], newdata=dataTrain,type="vector"),4))
  yPredDishonestTable <- melt(tapply(dataTrain$y, list(dataTrain$leaffd, dataTrain$w), mean), varnames=c("leaffd","w"))
  yPredDishonestTable <- rename(yPredDishonestTable,replace=c("value"="ypreddishon"))
  dataTest <- merge(dataTest,yPredDishonestTable, by.x=c("leaffd", "w"), by.y=c("leaffd","w"))
  MSEy_dishonest[[i]] = mean((dataTest$ypreddishon-dataTest$y)^2, na.rm=T)
  
  dataTest <- dataTest[, !(names(dataTest) %in% c("ypredhon","ypreddishon"))]
}