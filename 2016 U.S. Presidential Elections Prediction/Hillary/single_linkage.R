single_linkage <- function(sr,t)
{
  m_length <- 1000
  ps_matrix <- matrix(data = NA,nrow = m_length,ncol = m_length)
  num <- 1
  for(n in 1 : m_length)
  {
    for(m in 1 : m_length)
    {
      ps_matrix[num] <- abs(sr[,3][n + 1] - sr[,3][m + 1])
      num <- num + 1
    }
  }
  g <- hclust(as.dist(ps_matrix) , "single")
  plot(g)
  c <- rect.hclust(g,k = t)
  return(c)
}