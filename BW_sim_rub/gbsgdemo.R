library(rpart)
library(rpart.plot)
library(data.table)
library(causalTree)
gbsgdata = read.csv('gbsg.csv')
situation1 = gbsgprob(gbsgdata)
newdata = gbsgweight(situation1, method='Truncated', alpha = 0.1)
formula = rfst ~ age + menostat	+ tumsize	+ tumgrad	+ posnodal + prm + esm
tree <- causalTree(formula, weight=newdata$weigh, data=newdata,
                   treatment=newdata$htreat,split.Rule = "CT", cv.option = "CT",
                   split.Honest = T,cv.Honest = T, split.Bucket = T, xval = 5, 
                   cp = 0, minsize = 20)
opcp <- tree$cptable[,1][which.min(tree$cptable[,4])]
opfit <- prune(tree, opcp)
rpart.plot(opfit)
