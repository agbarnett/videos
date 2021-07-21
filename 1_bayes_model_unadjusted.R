# 1_bayes_model_unadjusted.R
# Bayesian model using JAGS on HPC
# version with just treatment
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
formula = clicked ~ trt2_c + trt2_l
X = model.matrix(formula, pretest_final) # includes intercept
P = ncol(X)
bdata = with(pretest_final, list(N = N, P = P, X = X, click = clicked))
# initial values
inits = list(alpha = rep(0, P))

# save for running in jags
parms = c('alpha')
subgroup = 'unadjusted'
outfile = paste('jags_ready_', subgroup, '.RData', sep='')
model.file = 'bugs_model_no_random.txt'
save(model.file, parms, bdata, inits, file=outfile)

# move back
setwd(here)
