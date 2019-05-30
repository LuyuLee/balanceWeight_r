get_ave = function(lis, nitem)
{
  a1=0; a2=0; a3=0; a4=0;
  std = array()
  for (i in 1 : nitem)
  {
    a1[i] = lis[[i]][1]
    a2[i] = lis[[i]][2]
    a3[i] = lis[[i]][3]
    a4[i] = lis[[i]][4]
  }
  std[1] = sd(a1)
  std[2] = sd(a2)
  std[3] = sd(a3)
  std[4] = sd(a4)
  ans = c(sum(a1), sum(a2), sum(a3), sum(a4))
  ans = ans / nitem
  anss =list(ans, std)
  return(anss)
}
