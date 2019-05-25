# Data = read.csv('gbsg.csv')
add_prob = function(Data, formula, ct = FALSE)
{

  if (ct )
  {
    conIdx = which(Data$Treat == 0)
    trIdx = which(Data$Treat == 1)
    train_idx = c(sample(trIdx, length(trIdx) * 0.7),
                  sample(conIdx, length(conIdx)* 0.7))
    train_data = Data[train_idx,]
    test_data = Data[-train_idx,]
    #formula = htreat ~ age + menostat	+ tumsize	+ tumgrad	+ posnodal + prm + esm
    logsti = glm(formula, binomial(link = 'logit'), data=train_data)
    summary(logsti)
    data.glm<- step(logsti)
    prediction = logsti$linear.predictors
    test.predict = predict(logsti, newdata = test_data)
    test.prob = 1/(1+exp(-test.predict))
    test.predict = as.factor(ifelse(test.prob >= 0.5,1,0))
    test.performance = length(which((test.predict==test_data$Treat)==TRUE))/nrow(test_data)
    show(test.performance)
    all.predict = predict(logsti, newdata = Data)
    all.prob = 1/(1+exp(-all.predict))
    all.predict = as.factor(ifelse(all.prob >= 0.5,1,0))
    all.performance = length(which((all.predict==Data$Treat)==TRUE))/nrow(Data)
    show(all.performance)
    prob = all.prob
    newdata = data.frame(Data, prob)
    return(newdata)
  }
  else
  {
    logsti = glm(formula, binomial(link = 'logit'), data=Data)
    summary(logsti)
    data.glm<- step(logsti)
    prediction = logsti$linear.predictors
    all.predict = predict(logsti, newdata = Data)
    all.prob = 1/(1+exp(-all.predict))
    all.predict = as.factor(ifelse(all.prob >= 0.5,1,0))
    all.performance = length(which((all.predict==Data$Treat)==TRUE))/nrow(Data)
    show(all.performance)
    prob = all.prob
    newdata = data.frame(Data, prob)
    return(newdata)
  }
  
}
