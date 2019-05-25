library(rpart)
library(rpart.plot)
library(data.table)
library(causalTree)
Alpha = list(1, 1)
Beta = list(0.5, -1, 0.55 , 1.5)
Gamma = list(-0.552, 0.725)
#Alpha = list(1, 1)
#Beta = list(0.5, -0.5, 0.2 , 0.8)
#Gamma = list(-0.552, 0.725)

E = 1
n = 10
m = 100

ATE_Combined = test_Treedata_ATE(effect = 1, item = 2000,
                        Method = "Combined" ,alpha = 0.05,
                        Alpha, Beta, Gama, inter_coef = 0.2875)
bias_ATE_Combined = ATE_Combined[1] / E
std_ATE_Combined = ATE_Combined[2]
RMSE_ATE_Combined = ATE_Combined[3]

ATE_Truncated = test_Treedata_ATE(effect = 1, item = 2000,
                         Method = "Truncated" ,alpha = 0.05,
                         Alpha, Beta, Gama, inter_coef = 0.2875)
bias_ATE_Truncated = ATE_Truncated[1] / E
std_ATE_Truncated = ATE_Truncated[2]
RMSE_ATE_Truncated = ATE_Truncated[3]

ATE_Overlap = test_Treedata_ATE(effect = 1, item = 2000,
                       Method = "Overlap" ,alpha = 0.05,
                       Alpha, Beta, Gama, inter_coef = 0.2875)
bias_ATE_Overlap = ATE_Overlap[1] / E
std_ATE_Overlap = ATE_Overlap[2]
RMSE_ATE_Overlap = ATE_Overlap[3]

ans_ATE = data.frame(Bias_ATE = c(bias_ATE_Combined, bias_ATE_Truncated, bias_ATE_Overlap),
                     Std_ATE = c(std_ATE_Combined, std_ATE_Truncated, std_ATE_Overlap),
                     RMSE_ATE = c(RMSE_ATE_Combined, RMSE_ATE_Truncated, RMSE_ATE_Overlap))
row.names(ans_ATE) = c("Combined", "Truncated", "Overlap")


causalTree_combined = list()
causalTree_combined = test_Treedata(effect = E, Method = 'Combined', item = 2000,
                                     Alpha = Alpha, Beta = Beta, Gama = Gama,
                                     inter_coef = 0.2875, BuckN = n, BuckM = m)
bias_causalTree_combined = causalTree_combined[1] / E
std_causalTree_combined = causalTree_combined[2]
RMSE_causalTree_combined = causalTree_combined[3]


causalTree_Truncated = test_Treedata(effect = E, Method = 'Truncated', 
                                      Truncated.alpha = 0.1, item = 2000,
                                      Alpha = Alpha, Beta = Beta, Gama = Gama,
                                      inter_coef = 0.2875, BuckN = n, BuckM = m)
bias_causalTree_Truncated = causalTree_Truncated[1] / E
std_causalTree_Truncated = causalTree_Truncated[2]
RMSE_causalTree_Truncated = causalTree_Truncated[3]

causalTree_Overlap = test_Treedata(effect = E, Method = 'Overlap', item = 2000,
                                    Alpha = Alpha, Beta = Beta, Gama = Gama,
                                    inter_coef = 0.2875, BuckN = n, BuckM = m)
bias_causalTree_Overlap = causalTree_Overlap[1] / E
std_causalTree_Overlap = causalTree_Overlap[2]
RMSE_causalTree_Overlap = causalTree_Overlap[3]

ans_causalTree = data.frame(bias_causalTree = c(bias_causalTree_combined, bias_causalTree_Truncated, bias_causalTree_Overlap),
                            std_causalTree = c(std_causalTree_combined, std_causalTree_Truncated, std_causalTree_Overlap),
                            RMSE_causalTree = c(RMSE_causalTree_combined, RMSE_causalTree_Truncated, RMSE_causalTree_Overlap))
row.names(ans_causalTree) = c("Combined", "Truncated", "Overlap")
ans =data.frame(ans_ATE,ans_causalTree)
write.csv(ans, "ATE&CT_sq_tree_ans_1st.csv")
