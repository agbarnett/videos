# 1_bayes_model_trump_interaction.R
# Bayesian model of trump interaction using JAGS software on high-performance-computer
# July 2021

# move to high-performance-computer
here = getwd()
setwd("//hpc-fs/barnetta/vaccine")

# make model file for JAGS software
source('1_model_file.R')
source('1_MCMC_parameters.R') # get the number of chains and thinning for MCMC run

## prepare the data
# need to make binary versions of key variables
pretest_final = mutate(pretest_final, 
                       statenum = as.numeric(as.factor(state)),
                       poolnum = as.numeric(as.factor(pool)),
                       trumppercent = (Trumppercent-0.5) / 0.1, # scaled to 10%
                       age_c = (age-40) / 10, # standardise age to 10 years
                       male = as.numeric(gender=='Male'),
                       trt2_c = as.numeric(as.numeric(Treatment2)==2), # cash
                       trt2_l = as.numeric(as.numeric(Treatment2)==3), # lottery
                       int1 =trt2_c*trumppercent, # make interaction
                       
                       int2 =trt2_l*trumppercent,
                       Education = ifelse(is.na(Education)==TRUE, 'Low', Education)) # impute 11 missing

# prepare data as a list for JAGS
N = nrow(pretest_final) # 
formula = clicked ~ trt2_c + trt2_l + male + int1 + int2 + age_c + Race + Education + trumppercent 
X = model.matrix(formula, pretest_final) # includes intercept
P = ncol(X)
C = max(pretest_final$poolnum)
S = max(pretest_final$statenum)
bdata = with(pretest_final, list(N = N, P = P, X = X, C = C, S = S, click = clicked, pool = poolnum, state = statenum))
# initial values
inits = list(alpha = rep(0, P), gamma=rep(0, C), lambda=rep(0, S), tau.gamma = 1, tau.lambda = 1)

# save for running in JAGS
parms = c('alpha','gamma_c','lambda_c')
subgroup = 'trump'
outfile = paste('jags_ready_', subgroup, '.RData', sep='')
save(model.file, parms, bdata, inits, file=outfile)

# move back
setwd(here)
