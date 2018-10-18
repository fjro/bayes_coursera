llam = 1.5 -0.3*0.8 +1*1.2
exp(llam)

sum(dpois(1:21, 2*15))
mean(rpois(100000, 2*15) <22)

dat <- readr::read_csv('data/callers.csv')
head(dat)
pairs(dat)
plot(dat$calls/dat$days_active, dat$isgroup2)

################################################
mod_string = " model {
    
for (i in 1:length(calls)) {
	calls[i] ~ dpois( days_active[i] * lam[i] )
  log(lam[i]) = b0 + b1*age[i] + b2*isgroup2[i]
}

b0 ~ dnorm(0.0, 1.0/1e2)
b1 ~ dnorm(0.0, 1.0/1e2)
b2 ~ dnorm(0.0, 1.0/1e2)
} "

set.seed(102)

data_jags = as.list(dat)

params = c("b0", "b1", "b2")

mod = jags.model(textConnection(mod_string), data=data_jags, n.chains=3)
update(mod, 1e3)

mod_sim = coda.samples(model=mod,
                       variable.names=params,
                       n.iter=5e3)
mod_csim = as.mcmc(do.call(rbind, mod_sim))

## convergence diagnostics
plot(mod_csim)
mean(mod_csim[,2] > 0)
HPDinterval(mod_csim)

gelman.diag(mod_sim)
autocorr.diag(mod_sim)
autocorr.plot(mod_sim)
effectiveSize(mod_sim)

## compute DIC
dic = dic.samples(mod, n.iter=1e3)
dic
head(dat)
summary(mod_sim)
head(mod_csim)
X <- dat[,-1]
head(X)
(pmed_coef = apply(mod_csim, 2, mean))
llam_hat = pmed_coef["b0"] + X %*% pmed_coef[c("b_badh", "b_age", "b_intx")]

plot(density(mod_csim[,2]))
lam_hat = exp(llam_hat)

hist(lam_hat)