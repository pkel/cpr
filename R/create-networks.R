# install.packages("igraph")
require(igraph)

setwd(system("git rev-parse --show-toplevel", intern=T))
system("mkdir -p data/networks/input")
system("rm -f data/networks/input/*-graphml.xml")

preferential_attachment <- function(n, ..., exponential=F) {
  g <- sample_pa(n, ..., directed=F)
  g$name <- "preferential_attachment"
  g$dissemination <- "flooding"
  g$link_delay_distr <- ifelse(exponential, "exponential", "uniform")
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
  g <- preferential_attachment(13, m=2)
  write.graph(g, sprintf("data/networks/input/%03i-uni-graphml.xml", i), format="graphml")
}
for (i in 1:10) {
  g <- preferential_attachment(13, m=2, exponential=T)
  write.graph(g, sprintf("data/networks/input/%03i-exp-graphml.xml", i), format="graphml")
}
