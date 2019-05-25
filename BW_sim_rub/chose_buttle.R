library(rpart)
library(rpart.plot)
library(data.table)
library(causalTree)
Alpha = list(1, 1)
Beta = list(0.5, -1, 0.55, 1.5)
Gamma = list(-0.552, 0.725)

E = 1

numchose = array()
for (i in 1 : 20)
{
  num = 1000 / i 
  ans_Overlap = test_Treedata(effect = E, Method = 'Overlap', item = 50,
                               Alpha = Alpha, Beta = Beta, Gama = Gama,
                               inter_coef = 0.2875,BuckN = i, BuckM = num)
  bias_Overlap = ans_Overlap[1] / E
  std_Overlap = ans_Overlap[2]
  numchose[i] = bias_Overlap
}
