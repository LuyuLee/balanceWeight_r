library(rpart)
library(rpart.plot)
library(data.table)
library(causalTree)
Alpha = list(1, 1)
Beta = list(0.5, -1, 0.55 , 1.5)
Gamma = list(-0.552, 0.725)
bia_sum = 0.0
std_sum = 0.0
set.seed(11)
inputdata = noprob_data(alpha = Alpha, beta = Beta, gama = Gamma,
                      inter34 = 0.2875, inter = 1)
#inputdata = Tree_data(alpha = Alpha, beta = Beta, gama = Gamma,
#                        inter34 = 0.2875, inter = 1)
summary(inputdata)
formula_prob = Treat ~ Z1 + Z2 + X1 + X2
situation1 = add_prob(inputdata, formula_prob)
library(ggplot2)
#ggplot(situation1, aes(x = situation1$prob)) +  geom_histogram(binwidth = 0.05, fill = "lightblue", colour = "black")
#newdata = gbsgweight(situation1, method='Combined')
#newdata = gbsgweight(situation1, method='Truncated', alpha = 0.05)
newdata = gbsgweight(situation1, method='Overlap')
#newdata = gbsgweight(situation1, method='Matching')
ggplot(newdata, aes(x = weigh)) +  geom_histogram(binwidth = 0.2, fill = "lightblue", colour = "black")
formula = Y ~  Z1 + Z2 + X1 + X2
#set.seed(1)
tree_new <- causalTree(formula, weights=newdata$weigh  , data=newdata,
                        treatment=newdata$Treat,split.Rule = "CT", cv.option = "CT",
                        split.Honest = T,cv.Honest = T, split.Bucket = T, xval = 5, 
                        cp = 0, minsize = 40, bucketNum = 10, bucketMax = 100)
#rpart.plot(tree_new)
opcp <- tree_new$cptable[,1][which.min(tree_new$cptable[,4])]
opfit <- prune(tree_new, opcp)
rpart.plot(opfit)
show(tree_new$cptable)
df = opfit
t1 = estimate_method(df)
summary(newdata)
t1


# when I used the new data-process method, overlap and truncation
# the finnal trees tend to be prunted aggressive
# parameter of simulation seems have large influence to the answer of bias.
# truncation is smaller a little than combined when the probility of accepting
# treatment is balance