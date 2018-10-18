# anova with separate variance

# fit a cell means model with separate variance
mod_string_2 = " model {
for (i in 1:length(y)) {
y[i] ~ dnorm(mu[grp[i]], prec[grp[i]])
}

for (j in 1:3) {
mu[j] ~ dnorm(0.0, 1.0/1.0e6)
}

for (j in 1:3) {
prec[j] ~ dgamma(5/2.0, 5*1.0/2.0)
sig[j] = sqrt( 1.0 / prec[j] )
}

} "
set.seed(82)
str(PlantGrowth)
data_jags = list(y=PlantGrowth$weight, 
                 grp=as.numeric(PlantGrowth$group))

params = c("mu", "sig")

inits2= function() {
  inits = list("mu"=rnorm(3,0.0,100.0), "prec"=rgamma(3,1.0,1.0))
}

mod2 = jags.model(textConnection(mod_string_2), data=data_jags, inits=inits2, n.chains=3)
update(mod2, 1e3)

mod_sim2 = coda.samples(model=mod2,
                       variable.names=params,
                       n.iter=5e3)
mod_csim2 = as.mcmc(do.call(rbind, mod_sim2)) # combined chains

# model diagnostics
plot(mod_sim2)
HPDinterval(mod_csim2)

gelman.diag(mod_sim2)
autocorr.diag(mod_sim2)
effectiveSize(mod_sim2)

(pm_params2 = colMeans(mod_csim2))
yhat = pm_params2[1:3][data_jags$grp]
resid = data_jags$y - yhat
plot(resid)
plot(yhat, resid)

summary(mod_sim)
summary(mod_sim2)
# We are interested to know if one of the treatments increases mean yield. It is clear 
# that treatment 1 does not. What about treatment 2?
mean(mod_csim[,3] > mod_csim[,1])

#It may be the case that treatment 2 would be costly to put into production. 
# Suppose that to be worthwhile, this treatment must increase mean yield by 10%. 
# What is the posterior probability that the increase is at least that?
mean(mod_csim[,3] > 1.1*mod_csim[,1])

HPDinterval(mod_csim)
HPDinterval(mod_csim2)

dic1 <- dic.samples(mod, n.iter=10000)
dic2 <- dic.samples(mod2, n.iter=10000)
dic2
dic1 -dic2

quantile(mod_csim[,3] - mod_csim[,1], c(0.025, 0.975))

mod_cm = lm(weight ~ -1 + group, data=PlantGrowth)
summary(mod_cm)
