analyze_cluster <- function(c,sr,pr)
{
  v1 <- pr[,4]
  v2 <- pr[,7]
  fips <- vector(mode = "character")
  vote <- vector(mode = "numeric")
  num <- 1
  vot <- 0
  temp <- "null"
  for (n in 2:length(v1))
  {
    if(v1[n] != temp)
    {
      if (temp == "null")
      {
        temp <- v1[n]
      }
      else
      {
        fips[num] <- temp
        vote[num] <- vot
        num <- num + 1
        temp <- v1[n]
        vot <- 0
      }
    }
    vot <- vot + as.numeric(v2[n])
    if(n == length(v1))
    {
      fips[num] <- temp
      vote[num] <- vot
    }
  }
  
  asr <- vector(mode <- "numeric" , length <- length(c))
  cv <- vector(mode <- "numeric" , length <- length(c))
  tfi <- "null"
  c_vote <- 0
  for(n in 1 : length(c))
  {
    for(m in 1 : length(c[n][[1]]))
    {
      tfi <- sr[,2][c[n][[1]][m]+1]
      for(l in 1 : length(fips))
      {
        if(tfi == fips[l])
        {
          c_vote <- c_vote + vote[l]
          break
        }
      }
    }
    cv[n] <- c_vote
    c_vote <- 0
  }
  tsr <- 0
  for(n in 1 : length(c))
  {
    for(m in 1 : length(c[n][[1]]))
    {
      tsr <- tsr + sr[,3][c[n][[1]][m]+1]
    }
    asr[n] <- tsr / length(c[n][[1]])
    tsr <- 0
  }
  plot(asr,cv)
}