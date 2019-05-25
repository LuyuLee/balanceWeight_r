balanceweight = function(df, method ='Truncated', alpha = 0.05)
{
    #df = newdata
    #method = 'Truncated'
    #alpha = 0.05
    
    id0 = which(df$treatment == 0)
    id1 = which(df$treatment == 1)
    if (method == 'Truncated')
    {
        weight0 = 1 / (1 - df$prob)
        weight1 = 1 / df$prob
        weight0[which((df$prob < alpha) == TRUE) || (df$prob > (1 - alpha)) == TRUE] = 0
        weight1[which((df$prob < alpha) == TRUE) || (df$prob > (1 - alpha)) == TRUE] = 0
    }
          
    if (method == 'Overlap')
    {
        weight0 = df$prob
        weight1 = 1 - df$prob
    }
  
    if (method == 'Combined')
    {
        weight0 = 1 / (1 - df$prob)
        weight1 = 1 / df$prob
    }
    
    if (method == 'Matching')
    {
        weight0[which(df$prob > (1 - df$prob))] = 1 
        weight0[which(df$prob <= (1 - df$prob))] = df$prob / (1 - df$prob)
        weight1[which(df$prob > (1 - df$prob))] = (1 - df$prob) / df$prob
        weight1[which(df$prob <= (1 -df$prob))] = 1
    }
    
    if (method == 'Treated')
    {
        weight0 = df$prob / (1 - df$prob)
        Lis = array()
        for (i in 1:length.POSIXlt(df))
            Lis[i] = 1
        weight1 = Lis
    }
    
    if (method == 'Control')
    {
        weight0 = array()
        for (i in 1:length.POSIXlt(df))
            weight0[i] = 1
        weight1 = (1 - df$prob) / df$prob
    }
    weight = data.frame(c(weight0[id0],weight1[id1]))
    weightt = data.frame(weight0[id0], weight1[id1])
    return(data.frame(df, weight))
}
