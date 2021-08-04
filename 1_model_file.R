# 1_model_file.R
# create logistic regression model file ready for use in JAGS software
# July 2021

# a) logistic model without random effects
model.file = 'bugs_model_no_random.txt'
bugs = file(model.file, 'w')
cat('model{
for (j in 1:N) {
  click[j] ~ dbern(mu[j])
  logit(mu[j]) <- inprod(alpha[1:P],X[j,])
}
for (i in 1:P){ # 
  alpha[i] ~ dnorm(0, 0.001)
}
}\n', file=bugs)
close(bugs)

# b) logistic model with random effects for state and pool
model.file = 'bugs_model.txt'
bugs = file(model.file, 'w')
cat('model{
for (j in 1:N) {
  click[j] ~ dbern(mu[j])
  logit(mu[j]) <- inprod(alpha[1:P],X[j,]) + gamma_c[pool[j]] + lambda_c[state[j]]
}
for (i in 1:P){ # 
  alpha[i] ~ dnorm(0, 0.001)
}
for (i in 1:C){ # 
  gamma[i] ~ dnorm(0, tau.gamma)
  gamma_c[i] <- gamma[i] - mu.gamma
}
mu.gamma = mean(gamma[1:C])
tau.gamma ~ dgamma(0.1, 0.1)
for (i in 1:S){ # 
  lambda[i] ~ dnorm(0, tau.lambda)
  lambda_c[i] <- lambda[i] - mu.lambda # centre
}
mu.lambda = mean(lambda[1:S])
tau.lambda ~ dgamma(0.1, 0.1)
}\n', file=bugs)
close(bugs)
