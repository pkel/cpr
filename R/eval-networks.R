# install.packages("igraph")
require(igraph)

setwd(system("git rev-parse --show-toplevel", intern=T))
setwd("data/networks/output")

results <- readLines("last-run.txt")

factorize <- function(v) {
  v <- cut(v, quantile(v, probs=c(0., .05,.25, .75, .95, 1)), include.lowest=T)
  levels(v) <- c('bot05', 'bot25', 'mid50', 'top75', 'top95')
  return(v)
}

node_df_of_file <- function(f) {
  g <- read.graph(f, format="graphml")
  v <- V(g)
  all_rewards <- sum(v$reward)
  all_activations <- sum(v$all_activations)
  data.frame(# parameters
             networkSize = length(v),
             networkKind = g$name,
             protocolFamily = g$protocol,
             k = g$pow_per_block,
             protocol = sprintf("%s-k%03i-%s", g$protocol, g$pow_per_block, g$reward_function),
             linkDelayDistr = g$link_delay_distr,
             allActivations = all_activations,
             allRewards = all_rewards,
             closeness = v$closeness,
             farness = v$farness,
             compute = v$compute,
             connectivity = factorize(v$closeness),
             strength = factorize(v$compute),
             rewardFn = g$reward_function,
             # measurements
             activations = v$activations,
             reward = v$reward,
             rewardPerActivation = v$reward / v$activations,
             relReward = v$reward / all_rewards
  )
}
nodes <- NULL
for (r in results) nodes <- rbind(nodes, node_df_of_file(r))
nodes$protocolFamily <- relevel(as.factor(nodes$protocolFamily), "nakamoto")
nodes$protocol <- relevel(as.factor(nodes$protocol), "nakamoto-k001-block")
summary(nodes)

m <- lm(reward_per_activation ~ reward_function + closeness + protocol + pow_per_block, data=nodes)
summary(m)

m <- lm(rel_reward ~ compute + reward_function + closeness + protocol + pow_per_block, data=nodes)
summary(m)

# install.packages("dagitty")
require(dagitty)

node_scm_lines <- c('dag {',
                    'compute -> activations',
                    '{rewardPerActivation activations} -> reward',
                    '{k protocolFamily rewardFn} -> protocol',
                    'protocolFamily -> {rewardFn k}',
                    'location -> {closeness farness}',
                    '{protocol location activations} -> rewardPerActivation',
                    # 'network_size -> all_rewards',
                    # 'reward_function -> all_rewards',
                    # 'all_activations -> all_rewards',
                    '}')
node_scm <- dagitty(paste(node_scm_lines, collapse='\n'))
latents(node_scm) <- c('location')
plot(node_scm)
impliedConditionalIndependencies(node_scm)

# install.packages("lavaan")
# library( lavaan )
# corr <- lavCor( nodes )
# https://currentprotocols.onlinelibrary.wiley.com/doi/full/10.1002/cpz1.45

res <- localTests(node_scm, nodes, type="cis.chisq")
plotLocalTestResults(res)

adjustmentSets(node_scm, exposure=c('protocol', 'farness'), outcome='rewardPerActivation', type='minimal')

weak_nodes <- subset(nodes,
                     strength == 'bot05'
                     & (protocolFamily == 'nakamoto' | rewardFn != 'block'))
mw <- lm(rewardPerActivation ~ protocol, data=weak_nodes)
summary(mw)

outer_nodes <- subset(nodes,
                      connectivity == 'bot05'
                      & (protocolFamily == 'nakamoto' | rewardFn != 'block'))
mo <- lm(rewardPerActivation ~ protocol, data=outer_nodes)
summary(mo)

weak_outer_nodes <- subset(nodes,
                           connectivity %in% c('bot05', 'bot25')
                           & strength %in% c('bot05', 'bot25')
                           & (protocolFamily == 'nakamoto' | rewardFn != 'block'))
mwo <- lm(rewardPerActivation ~ protocol, data=weak_outer_nodes)
summary(mwo)
