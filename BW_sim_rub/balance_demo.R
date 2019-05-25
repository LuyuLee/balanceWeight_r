library(rpart)
library(rpart.plot)
library(data.table)
library(causalTree)
library(ggplot2)
Alpha = list(1, 1.2)

situation1 = balance_data(alpha = Alpha, inter = 1)
summary(situation1)
#ggplot(situation1, aes(x = situation1$prob)) +  geom_histogram(binwidth = 0.05, fill = "lightblue", colour = "black")
#newdata = gbsgweight(situation1, method='Combined')
#newdata = gbsgweight(situation1, method='Truncated', alpha = 0.05)
newdata = gbsgweight(situation1, method='Overlap')
#newdata = gbsgweight(situation1, method='Matching')
ggplot(newdata, aes(x = weigh)) +  geom_histogram(binwidth = 0.1, fill = "lightblue", colour = "black")
formula = Y ~  X1 + X2
tree_new <- causalTree(formula, weight=newdata$weigh, data=newdata,
                       treatment=newdata$Treat,split.Rule = "CT", cv.option = "CT",
                       split.Honest = T,cv.Honest = T, split.Bucket = T, xval = 5, 
                       cp = 0, minsize = 40, bucketNum = 10, bucketMax = 100)
#rpart.plot(tree_new)
opcp <- tree_new$cptable[,1][which.min(tree_new$cptable[,4])]
opfit <- prune(tree_new, opcp)
#rpart.plot(opfit)
show(tree_new$cptable)
df = opfit
t1 = estimate_method(df)
summary(newdata)
t1
