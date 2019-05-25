library(rpart)
library(rpart.plot)
library(data.table)
library(causalTree)
Alpha = list(1, 1)
Beta = list(0.5, -1, 0.55 , 1.5)
Gamma = list(-0.552, 0.725)
bia_sum = 0.0
std_sum = 0.0
#set.seed(12)
Effect = c(3, -3, 0)

E = 0
n = 10
m = 100

ans_combined = list()
ans_combined = test_Treedata_ck(effect = E, Method = 'Combined', item = 1000,
                                 Alpha = Alpha, Beta = Beta, Gama = Gama,
                                 inter_coef = 0.2875, BuckN = n, BuckM = m
                                 , ck_effect = Effect, mul = 3.5)
bias_combined = ans_combined[1]
std_combined = ans_combined[2]

ans_Truncated = test_Treedata_ck(effect = E, Method = 'Truncated', 
                                  Truncated.alpha = 0.1, item = 1000,
                                  Alpha = Alpha, Beta = Beta, Gama = Gama,
                                  inter_coef = 0.2875, BuckN = n, BuckM = m
                                  , ck_effect = Effect, mul = 3.5)
bias_Truncated = ans_Truncated[1]
std_Truncated = ans_Truncated[2]

ans_Overlap = test_Treedata_ck(effect = E, Method = 'Overlap', item = 1000,
                                Alpha = Alpha, Beta = Beta, Gama = Gama,
                                inter_coef = 0.2875, BuckN = n, BuckM = m
                                , ck_effect = Effect, mul = 3.5)
bias_Overlap = ans_Overlap[1]
std_Overlap = ans_Overlap[2]

ans = data.frame(Bias_percent = c(bias_combined, bias_Truncated, bias_Overlap),
                 Std = c(std_combined, std_Truncated, std_Overlap))
row.names(ans) = c("Combined", "Truncated", "Overlap")
#write.csv(ans, "linearans_1st.csv")