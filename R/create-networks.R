# install.packages("igraph")
require(igraph)

setwd(system("git rev-parse --show-toplevel", intern=T))
system("mkdir -p data/networks/input")
system("rm data/networks/input/*-graphml.xml")

pref.att <- function(n, .., exponential=F) {
  g <- barabasi.game(n, power=1, m=2, directed=F)
  g$dissemination <- "flooding"
  V(g)$compute <- runif(n)
  E(g)$distance <- runif(length(E(g))) * 9 + 1
  E(g)$delay <- ifelse(exponential,
                       paste("exponential", E(g)$distance),
                       paste("uniform", E(g)$distance * .75, E(g)$distance * 1.25)
  )
  V(g)$closeness <- closeness(g, weights=E(g)$distance)
  V(g)$farness <- 1/V(g)$closeness
  g$activation_delay <- mean(V(g)$farness)
  return(g)
}

set.seed(42)
for (i in 1:10) {
  g <- pref.att(13)
  write.graph(g, sprintf("data/networks/input/%03i-uni-graphml.xml", i), format="graphml")
}
for (i in 1:10) {
  g <- pref.att(13, exponential=T)
  write.graph(g, sprintf("data/networks/input/%03i-exp-graphml.xml", i), format="graphml")
}
