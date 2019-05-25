# gbsgdata = read.csv('gbsg.csv')
gbsgprob = function(gbsgdata)
{
    conIdx = which(gbsgdata$htreat == 0)
    trIdx = which(gbsgdata$htreat == 1)
    train_idx = c(sample(trIdx, length(trIdx) * 0.7),
                  sample(conIdx, length(conIdx)* 0.7))
    train_data = gbsgdata[train_idx,]
    test_data = gbsgdata[-train_idx,]
    formula = htreat ~ age + menostat	+ tumsize	+ tumgrad	+ posnodal + prm + esm
    logsti = glm(formula, binomial(link = 'logit'), data=train_data)
    summary(logsti)
    data.glm<- step(logsti)
    prediction = logsti$linear.predictors
    test.predict = predict(logsti, newdata = test_data)
    test.prob = 1/(1+exp(-test.predict))
    test.predict = as.factor(ifelse(test.prob >= 0.5,1,0))
    test.performance = length(which((test.predict==test_data$htreat)==TRUE))/nrow(test_data)
    all.predict = predict(logsti, newdata = gbsgdata)
    all.prob = 1/(1+exp(-all.predict))
    all.predict = as.factor(ifelse(all.prob >= 0.5,1,0))
    all.performance = length(which((all.predict==gbsgdata$htreat)==TRUE))/nrow(gbsgdata)
    prob = all.prob
    newdata = data.frame(gbsgdata, prob)
    return(newdata)
}