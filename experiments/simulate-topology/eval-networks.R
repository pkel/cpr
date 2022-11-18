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
  all_activations <- sum(v$activations)
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
             netBias = v$net_bias,
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
nodes$linkDelayDistr <- relevel(as.factor(nodes$linkDelayDistr), "constant")
nodes$protocolFamily <- relevel(as.factor(nodes$protocolFamily), "nakamoto")
nodes$protocol <- relevel(as.factor(nodes$protocol), "nakamoto-k001-block")
summary(nodes)

m <- lm(rewardPerActivation ~ rewardFn + closeness + protocol + k, data=nodes)
summary(m)

m <- lm(relReward ~ compute + rewardFn + closeness + protocol + k, data=nodes)
summary(m)

### build a causal graph

# install.packages("dagitty")
require(dagitty)

node_scm_lines <- c('dag {',
                    'compute -> activations -> reward',
                    'rewardPerActivation -> reward',
                    '{k protocolFamily rewardFn} -> protocol',
                    'protocolFamily -> {rewardFn k}',
                    '{topology linkDelayDistr} -> network',
                    'topology -> {closeness farness netBias}',
                    '{protocol network} -> rewardPerActivation',
                    # 'network_size -> all_rewards',
                    # 'reward_function -> all_rewards',
                    # 'all_activations -> all_rewards',
                    '}')
node_scm <- dagitty(paste(node_scm_lines, collapse='\n'))
latents(node_scm) <- c('network', 'topology')
plot(node_scm)

# install.packages("lavaan")
# library( lavaan )
# corr <- lavCor( nodes )
# https://currentprotocols.onlinelibrary.wiley.com/doi/full/10.1002/cpz1.45

### Check implied independencies

impliedConditionalIndependencies(node_scm)

# This is intended to work on discrete variables only, outliers on continuous variables might be ok
# it is also quite expensive to compute ..
res <- localTests(node_scm, nodes, type="cis.chisq")
plotLocalTestResults(res)

### Run a few simple models for weak and bad connected nodes

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

### How does our proposed net bias metric affect the relative rewards?

adjustmentSets(node_scm, exposure=c('protocol', 'topology'), outcome='rewardPerActivation', type='minimal')

look.at <- subset(nodes, (protocolFamily == 'nakamoto' | rewardFn != 'block'))
mfa <- lm(rewardPerActivation ~ farness + protocol, data=look.at)
summary(mfa)
mnb <- lm(rewardPerActivation ~ netBias + protocol, data=look.at)
summary(mnb)

plot(rewardPerActivation ~ netBias, data=subset(nodes, protocol=='bk-k032-constant'))

### look at nakamoto only

look.at <- subset(nodes, (protocolFamily == 'nakamoto') & linkDelayDistr == 'constant')
mfac <- lm(rewardPerActivation ~ farness, data=look.at)
summary(mfac)
mnbc <- lm(rewardPerActivation ~ netBias, data=look.at)
summary(mnbc)

look.at <- subset(nodes, (protocolFamily == 'nakamoto') & linkDelayDistr == 'uniform')
mfau <- lm(rewardPerActivation ~ farness, data=look.at)
summary(mfau)
mnbu <- lm(rewardPerActivation ~ netBias, data=look.at)
summary(mnbu)

look.at <- subset(nodes, (protocolFamily == 'nakamoto') & linkDelayDistr == 'exponential')
mfae <- lm(rewardPerActivation ~ farness, data=look.at)
summary(mfae)
mnbe <- lm(rewardPerActivation ~ netBias, data=look.at)
summary(mnbe)

lapply(list(mfac, mfau, mfae), coef)
lapply(list(mnbc, mnbu, mnbe), coef)

plot(rewardPerActivation ~ netBias, data=look.at)
plot(rewardPerActivation ~ farness, data=look.at)
hist(nodes$compute)
plot(netBias ~ farness, data=look.at)
