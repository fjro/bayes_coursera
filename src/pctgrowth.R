library(ProjectTemplate)
load.project()

# first use a simple inear model to test for difference between groups
boxplot(y ~ grp, data=pctgrowth)
lm1 <- lm(y ~ I(grp), data = pctgrowth)
anova(lm1)
summary(lm1)
# plot looks like diffence between groups 1 and 4 but model is inconclusive

# now try a hierarchical model

mod_string = " model {
for (i in 1:length(y)) {
  y[i] ~ dnorm(theta[grp[i]], invsig)
}

for (j in 1:max(grp)) {
  theta[j] ~ dnorm(mu, tau)
}


mu ~ dnorm(0, 1e6)
tau ~ dgamma(1/2, 3/2)
invtau <- 1/tau
sig ~ dgamma(2/2, 2/2)
invsig <- 1/sig

} "

set.seed(1)

data_jags = as.list(pctgrowth)

params = c("theta", "mu", "sig", 'tau')

mod = jags.model(textConnection(mod_string), data=data_jags, n.chains=3)
update(mod, 1e3)

mod_sim = coda.samples(model=mod,
                       variable.names=params,
                       n.iter=5e3)
mod_csim = as.mcmc(do.call(rbind, mod_sim))

## convergence diagnostics
plot(mod_sim, ask=T)

gelman.diag(mod_sim)
autocorr.diag(mod_sim)
autocorr.plot(mod_sim)
effectiveSize(mod_sim)
summary(mod_sim)

## compute DIC
dic = dic.samples(mod, n.iter=1e3)
dic

means_anova = tapply(pctgrowth$y, INDEX=pctgrowth$grp, FUN=mean)
plot(means_anova)
(pm_params = colMeans(mod_csim))
means_theta <- pm_params[4:9]
points(pm_params[4:9], col="red") 

dimnames(mod_csim)
mod_csim[[2]][[1]]

## where means_theta are the posterior point estimates for the industry means.
