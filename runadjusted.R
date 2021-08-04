subgroup = 'adjusted'
# run_jags_start.R
# called by makemany.R
# July 2021
library(rjags)
source('../stats.R')
source('1_MCMC_parameters.R')

# files
infile = paste('jags_ready_', subgroup, '.RData', sep = '')
outfile = paste('jags_results_', subgroup, '.RData', sep = '')
load(infile)

# run jags
jmodel = jags.model(model.file, data = bdata, inits = inits, n.chains = n_chains)
update(jmodel, n.iter = n_samples) # burn-in
output = jags.samples(jmodel, variable.names = parms, thin = n_thin, n.iter = n_samples*n_thin)
# DIC
dic = dic.samples(jmodel, thin=n_thin, n.iter=n_samples*n_thin)
pD = sum(dic$penalty)
DIC = sum(dic$deviance)


# get the stats from the chains
alpha = stats(output, var = 'alpha', nrow = bdata$P, MCMC = n_samples, num.chains = n_chains, plotchain = FALSE)
if(subgroup %in% c('unadjusted','adjusted') == FALSE){
  gamma = stats(output, var = 'gamma_c', nrow = bdata$C, MCMC = n_samples, num.chains = n_chains, plotchain = FALSE)
  lambda = stats(output, var = 'lambda_c', nrow = bdata$S, MCMC = n_samples, num.chains = n_chains, plotchain = FALSE)
}

# save
if(subgroup %in% c('unadjusted','adjusted') == FALSE){
 save(alpha, gamma, lambda, output, pD, DIC, file = outfile)
}
if(subgroup %in% c('unadjusted','adjusted') == TRUE){
 save(alpha, output, pD, DIC, file = outfile)
}
