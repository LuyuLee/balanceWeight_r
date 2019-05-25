estimate_method = function(df)
{
  id = which(df$frame$var == '<leaf>')
  TreeFrame = df[1]$frame
  number = TreeFrame$n[id]
  n = TreeFrame$n[1]
  weigh_n = TreeFrame$wt
  leaf_val = TreeFrame$yval[id]
  w_beta_hat = number *weigh_n[id] / sum(weigh_n[id] * number) * leaf_val 
  bias = sum(w_beta_hat)
  return(bias)
} 
  

estimate_method = function(df)
{
  id = which(df$frame$var == '<leaf>')
  TreeFrame = df[1]$frame
  number = TreeFrame$n[id]
  n = TreeFrame$n[1]
  weigh_n = TreeFrame$wt
  leaf_val = TreeFrame$yval[id]
  w_beta_hat = number * weigh_n[id] / sum(weigh_n[id] * number) * leaf_val 
  bias = sum(w_beta_hat)
  return(bias)
} 
