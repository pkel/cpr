# install.packages("igraph")
require(igraph)

setwd(system("git rev-parse --show-toplevel", intern=T))
system("mkdir -p data/networks/input")
system("rm -f data/networks/input/*-graphml.xml")

# proposed statistic
nodes_net_bias <- function(g) {
}

preferential_attachment <- function(n, ..., distribution="constant") {
  g <- sample_pa(n, ..., directed=F)
  g$name <- "preferential_attachment"
  g$dissemination <- "flooding"
  g$link_delay_distr <- distribution
  V(g)$solving_rate <- rexp(n)
  V(g)$compute <- V(g)$solving_rate / sum(V(g)$solving_rate)
  E(g)$distance <- runif(length(E(g))) * 9 + 1
  if (distribution == "constant") {
    # ev = distance; sd = 0
    E(g)$delay <- paste("constant", E(g)$distance)
  } else if (distribution == "uniform") {
    # ev = distance; sd = 1/sqrt(12) = .288 * distance
    # variance of unif is (b-a)^2 / 12
    E(g)$delay <- paste("uniform",
                        E(g)$distance * 0.5,
                        E(g)$distance * 1.5)
  } else if (distribution == "exponential") {
    # ev = distance = sd
    E(g)$delay <- paste("exponential", E(g)$distance)
  } else {
    stop("unknown distribution")
  }
  # TODO mode=in will cause trouble as soon as we generate and simulate on directed graphs.
  d <- distances(g, weights=E(g)$distance, mode="in")
  # standard statistics
  V(g)$farness <- sapply(V(g), function(n) sum(d[n, V(g)])) / (n - 1)
  V(g)$closeness <- 1 / V(g)$farness
  # proposed statistics
  V(g)$net_bias <- sapply(V(g), function(n) sum(V(g)$compute * d[n, V(g)]))
  # set activation delay relative to what we expect to be the average message delay
  g$activation_delay <- 2 * mean(V(g)$net_bias)
  return(g)
}

set.seed(42)
for (i in 1:10) {
  g <- preferential_attachment(13, m=2, distribution = "constant")
  write.graph(g, sprintf("data/networks/input/%03i-cns-graphml.xml", i), format="graphml")
}
for (i in 1:10) {
  g <- preferential_attachment(13, m=2, distribution = "uniform")
  write.graph(g, sprintf("data/networks/input/%03i-exp-graphml.xml", i), format="graphml")
}
for (i in 1:10) {
  g <- preferential_attachment(13, m=2, distribution = "exponential")
  write.graph(g, sprintf("data/networks/input/%03i-uni-graphml.xml", i), format="graphml")
}
