library(rpart)
library(rpart.plot)
library(data.table)
library(causalTree)
#source(getprob.r)
#source(balanceweight.R)

inputdata = load('simulation.1.rda')
situation = getprob(situation.1)
newdata = balanceweight(situation, method='Combined')
tree <- causalTree(y~ x1 + x2 + x3 + x4, data = newdata, weight = newdata$weigh, 
                   treatment = newdata$treatment,split.Rule = "CT", cv.option = "CT",
                   split.Honest = T,cv.Honest = T, split.Bucket = T, xval = 5, 
                   cp = 0, minsize = 20
                   )
opcp <- tree$cptable[,1][which.min(tree$cptable[,4])]
opfit <- prune(tree, opcp)
rpart.plot(opfit)

# now I have got 3 different results
# (Truncated/Combined  Control  Matching,etc.)
# some questions are emerged in this process of programing
# 1. prob question  (almost all prob are in (0.4,0.6))
# 2. judgement criterion
# 3. paper
