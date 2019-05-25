library(rpart)
library(rpart.plot)
library(data.table)
library(causalTree)
set.seed(1)
x = simpledata(2)
Alpha = list(0.22, -0.07, -0.14, 0.15)
Beita = list(0.13, -0.12, -0.02, 0.03)
data = gettreat_sq(x, 4, alpha = Alpha, b = 0.01, beita = Beita, int13 = 0.2, int24 = 0.1)
summary(data)
SAlpha = list(-250, 500, 700, -400)
SBeita = list(-50, 700, 600, -140)
inputdata = sq_SurvivalTime(data, 4, SAlpha, e_random = FALSE, b = 2000, beita = SBeita, int13 = 100, int24 = 100)
summary(inputdata)
formula_prob = Treat ~ x_norm + x_unif + x_norm.1 + x_unif.1
situation1 = add_prob(inputdata, formula_prob)
newdata = gbsgweight(situation1, method='Truncated', alpha = 0.1)
formula = Y ~ x_norm + x_unif + x_norm.1 + x_unif.1
Sq_tree <- causalTree(formula, weight=newdata$weigh, data=newdata,
                   treatment=newdata$Treat,split.Rule = "CT", cv.option = "CT",
                   split.Honest = T,cv.Honest = T, split.Bucket = T, xval = 5, 
                   cp = 0, minsize = 20)
rpart.plot(Sq_tree)
opcp <- Sq_tree$cptable[,1][which.min(Sq_tree$cptable[,4])]
Sq_opfit <- prune(Sq_tree, opcp)
rpart.plot(Sq_opfit)
show(Sq_tree$cptable)

# if adding the random error (0, variance), the result will be better or not?
# if added, what method can we know the true distribution, averange?
# how to use the tree method to simple our simulate data
# How could we know the true effect of treatment about our simulation or in other
# word, how could we judge the method we want to research.