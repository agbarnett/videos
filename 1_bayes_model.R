# 1_bayes_model.R
# Bayesian model using JAGS on HPC
# July 2021

# move to HPC
here = getwd()
setwd("//hpc-fs/barnetta/vaccine")

# make model file
source('1_model_file.R')
source('1_MCMC_parameters.R')

## prepare the data
# need to make binary versions of key variables
pretest_final = mutate(pretest_final, 
                       statenum = as.numeric(as.factor(state)),
                       poolnum = as.numeric(as.factor(pool)),
                       trumppercent = (Trumppercent-0.5) / 0.1,
                       age_c = (age-40) / 10, # standardise age
                       male = as.numeric(gender=='Male'),
                       trt2_c = as.numeric(as.numeric(Treatment2)==2), # cash
                       trt2_l = as.numeric(as.numeric(Treatment2)==3), # lottery
                       int1 =trt2_c*male, # make interactions
                       int2 =trt2_l*male,
                       Education = ifelse(is.na(Education)==TRUE, 'Low', Education)) # impute 11 missing

# prepare list for winbugs
N = nrow(pretest_final) # 
formula = clicked ~ trt2_c + trt2_l + male + age_c + Race + Education + trumppercent 
X = model.matrix(formula, pretest_final) # includes intercept
P = ncol(X)
C = max(pretest_final$poolnum)
S = max(pretest_final$statenum)
bdata = with(pretest_final, list(N = N, P = P, X = X, C = C, S = S, click = clicked, pool = poolnum, state = statenum))
# initial values
inits = list(alpha = rep(0, P), gamma=rep(0, C), lambda=rep(0, S), tau.gamma = 1, tau.lambda = 1)

# save for running in jags
parms = c('alpha', 'gamma_c', 'lambda_c')
subgroup = 'full'
outfile = paste('jags_ready_', subgroup, '.RData', sep='')
save(model.file, parms, bdata, inits, file=outfile)

# move back
setwd(here)
