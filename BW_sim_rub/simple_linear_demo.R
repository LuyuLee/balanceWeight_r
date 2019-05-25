library(rpart)
library(rpart.plot)
library(data.table)
library(causalTree)
x = simpledata(2)
Alpha = list(0.22, 0.14, 0.12, 0.08)
data = gettreat(x, 4, alpha = Alpha, b = 0.01)
Belta = list(-450, 400, 300, -600)
inputdata = getSurvivalTime(data, 4, Belta, e_random = FALSE)
formula_prob = Treat ~ x_norm + x_unif + x_norm.1 + x_unif.1
situation1 = add_prob(inputdata, formula_prob)
newdata = gbsgweight(situation1, method='Combined')
formula = Y ~ x_norm + x_unif + x_norm.1 + x_unif.1
linear_tree <- causalTree(formula, weight=newdata$weigh, data=newdata,
                   treatment=newdata$Treat,split.Rule = "CT", cv.option = "CT",
                   split.Honest = T,cv.Honest = T, split.Bucket = T, xval = 5, 
                   cp = 0, minsize = 20)
rpart.plot(linear_tree)
opcp <- tree$cptable[,1][which.min(linear_tree$cptable[,4])]
linear_opfit <- prune(linear_tree, opcp)
rpart.plot(linear_opfit)
