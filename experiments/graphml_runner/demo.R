require(igraph)

setwd(system("git rev-parse --show-toplevel", intern=T))

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
  V(g)$compute_farness <- sapply(V(g), function(n) sum(V(g)$compute * d[n, V(g)]))
  V(g)$compute_closeness <- 1 / V(g)$compute_farness
  return(g)
}

sim <- function(g, ..., protocol="nakamoto", activation_delay=1, activations=1000, seed=NULL) {
  # copy g
  g$activation_delay <- activation_delay
  g$activations <- activations
  g$protocol <- protocol
  if (!is.null(seed)) g$seed <- seed
  in.file <- tempfile()
  out.file <- tempfile()
  err.file <- tempfile()
  write.graph(g, in.file, format="graphml")
  status <- system2("dune", c("exec", "graphml_runner"), wait=T, stdin=in.file, stdout=out.file, stderr=err.file)
  if (status > 0) {
    cat(read_lines(err.file))
    stop(status)
  }
  else {
    r <- read.graph(out.file, format="graphml")
    return(r)
  }
}

print(sim(preferential_attachment(7)))
