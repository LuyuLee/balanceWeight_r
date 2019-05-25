library(data.table)
Alpha = list(1, 1)
Beta = list(0.5, -1, 0.55 , 1.5)
Gamma = list(-0.552, 0.725)
bia_sum = 0.0
std_sum = 0.0

set.seed(11)
inputdata = noprob_data(alpha = Alpha, beta = Beta, gama = Gamma,
                        inter34 = inter_coef, inter = effect)
summary(inputdata)
formula_prob = Treat ~ Z1 + Z2 + X1 + X2
situation1 = add_prob(inputdata, formula_prob)
newdata = gbsgweight(situation1, method='Truncated', alpha = 0.05)

ATE = sum(newdata$Treat * newdata$Y * newdata$weigh) /
  sum(newdata$Treat * newdata$weigh) - 
  sum((1 - newdata$Treat) * newdata$Y * newdata$weigh) /
  sum((1 - newdata$Treat) * newdata$weigh)
bias = sum(ATE) / item - effect

#std = sd(ATE)
RMSE = (sum((ATE - effect)^2) / item)^0.5
tableans = c(bias, RMSE)