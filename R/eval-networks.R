# install.packages("igraph")
require(igraph)

setwd(system("git rev-parse --show-toplevel", intern=T))
setwd("data/networks/output")

results <- readLines("last-run.txt")

node_df_of_file <- function(f) {
  g <- read.graph(f, format="graphml")
  v <- V(g)
  all_rewards <- sum(v$reward)
  data.frame(# parameters
             network_size = length(v),
             network_kind = g$name,
             protocol = g$protocol,
             pow_per_block = g$pow_per_block,
             link_delay_distr = g$link_delay_distr,
             closeness = v$closeness,
             farness = v$farness,
             compute = v$compute,
             reward_function = g$reward_function,
             # measurements
             activations = v$activations,
             reward = v$reward,
             reward_per_activation = v$reward / v$activations,
             rel_reward = v$reward / all_rewards
  )
}
nodes <- NULL
for (r in results) nodes <- rbind(nodes, node_df_of_file(r))
summary(nodes)

m <- lm(reward_per_activation ~ reward_function + closeness + protocol + pow_per_block, data=nodes)
summary(m)

m <- lm(rel_reward ~ compute + reward_function + closeness + protocol + pow_per_block, data=nodes)
summary(m)
