# substituting ECoL function c.Hubs to avoid warnings 
c.Hubs <- function(graph) {
  #1 - mean(igraph::hub.score(graph)$vector)
  1 - mean(igraph::hits_score(graph)$vector)
}

# internal function from package ECoL
# altered so high value -> high complexity
c.B1 <- function(y) {
  c <- -1/log(nlevels(y))
  i <- table(y)/length(y)
  # high value -> high complexity
  aux <- 1 - c*sum(i*log(i))
  # high value -> low complexity
  # aux <- c * sum(i * log(i))
  return(aux)
}

# internal function from package ECoL
c.B2 <- function(y) {
  ii <- summary(y)
  nc <- length(ii)
  aux <- ((nc - 1)/nc) * sum(ii/(length(y) - ii))
  aux <- 1 - (1/aux)
  return(aux)
}

tp <- function(scores, rm = "data"){
  scores_t <- as.data.frame(t(scores[,-which(colnames(scores) %in% rm)]))
  colnames(scores_t) <- scores[,"data"]
  scores_t
}