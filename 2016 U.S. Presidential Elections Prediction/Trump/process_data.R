process_data <- function(pr)
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
  
  fi <- vector(mode <- "character")
  sr <- vector(mode <- "numeric")
  num <- 1
  tmp <- -1
  for(n in 2 : length(pr[,1]))
  {
    if(grepl(pattern <- "Donald Trump",x <- pr[,6][n]))
    {
      fi[num] <- pr[,4][n]
      for(m in 1 : length(fips))
      {
        if(pr[,4][n] ==  fips[m])
        {
          tmp <- vote[m]
          break
        }
      }
      sr[num] <- as.numeric(pr[,7][n]) * 100 / tmp
      if(is.na(sr[num]))
        sr[num] <- 0
      num <- num + 1
    }
  }
  return(data.frame(fi,sr))
}