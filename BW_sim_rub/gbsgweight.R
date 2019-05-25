gbsgweight = function(df, method ='Truncated', alpha = 0.05)
{
  #df = situation1
  #method = 'Overlap'
  #alpha = 0.05
  weight0 = array()
  weight1 = array()
  id0 = which(df$Treat == 0)
  id1 = which(df$Treat == 1)
  if (method == 'Truncated')
  {
    weight0 = 1 / (1 - df$prob)
    weight1 = 1 / df$prob
    weight0[which(df$prob < alpha)] = 0 
    weight0[which(df$prob >= (1 - alpha))] = 0
    weight1[which(df$prob < alpha)] = 0 
    weight1[which(df$prob >= (1 - alpha))] = 0
  }
  
  if (method == 'Overlap')
  {
    weight0 = df$prob
    weight1 = 1 - df$prob
    #df$prob is the probility of teatment = 1
    #1 - df$prob is the probility of treatment = 0
  }
  
  if (method == 'Combined')
  {
    weight0 = 1 / (1 - df$prob)
    weight1 = 1 / df$prob
  }
  
  if (method == 'Matching')
  {
    #weight0[which(df$prob <= (1 - df$prob))] = df$prob / (1 - df$prob)
    weight0 = df$prob / (1 - df$prob)
    weight0[which(df$prob > (1 - df$prob))] = 1 
    weight1 = (1 - df$prob) / df$prob
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
  
  weigh = array()
  for (i in 1: length.POSIXlt(df))
  {
    if (i %in% id0){
      weigh[i] = weight0[i]
  }   
    else{
      weigh[i] = weight1[i] 
  }} 
  weight = data.frame(weigh)
  # weightt = data.frame(weight0[id0], weight1[id1])
  return(data.frame(df, weight))
}

 
