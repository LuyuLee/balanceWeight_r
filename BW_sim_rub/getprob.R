#library(ggplot2)
#load('simulation.1.rda')
# dataname = 'simulation.1.rda'
#inputdata = load('simulation.1.rda')
getprob = function(inputdata)
{
  # simulation.1 = load(dataname)
  n = nrow(simulation.1)
  # conIdx = which(simulation.1$treatment == 0)
  # trIdx = which(simulation.1$treatment == 1)
  # train_idx = c(sample(trIdx, length(trIdx) * 0.7),
  #              sample(conIdx, length(conIdx)* 0.7))
  # train_data = simulation.1[train_idx,]
  # test_data = simulation.1[-train_idx,]
  formula = treatment ~ x1 + x2 + x3 + x4 
#formula = treatment ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10
  logsti = glm(formula, binomial(link = 'logit'), data = simulation.1)
  summary(logsti)
  data.glm<- step(logsti)
  prediction = logsti$linear.predictors
  all.predict = predict(logsti)
  all.prob = 1/(1+exp(-all.predict))
  all.predict = as.factor(ifelse(all.prob >= 0.5,1,0))
  performance = length(which((all.predict==simulation.1$treatment)==TRUE))/nrow(simulation.1)
  prob = all.prob
  new.predict = predict(logsti,newdata=test_data)
  new.prob = 1/(1+exp(-new.predict))
  new.predict = as.factor(ifelse(new.prob >= 0.5,1,0))
  performance = length(which((new.predict==test_data$treatment)==TRUE))/nrow(test_data)
  
  newdata = data.frame(simulation.1, prob)
  return(newdata)
  }

