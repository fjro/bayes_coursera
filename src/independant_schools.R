library(rstan)
library(bayesplot)

schools_data <- list(
  y = c(28,  8,-3,  7,-1,  1, 18, 12),
  sigma = c(15, 10, 16, 11,  9, 11, 10, 18),
  J = 8
)
ind_mod <- stan("stan/independent_schools.stan", data = schools_data, seed = 1000)
ind_schools_draws <- as.array(ind_mod)
schools_diagnostics <- nuts_params(ind_mod)


# parallel coordinates
color_scheme_set("darkgray")
theme_update(axis.text = element_text(size = 20))
div_style <- parcoord_style_np(div_color = "green", div_size = 0.15, div_alpha = 0.4)
parcoord_schools <-
  mcmc_parcoord(
    ind_schools_draws,
    size = 0.15,
    alpha = 0.2,
    np = schools_diagnostics,
    np_style = div_style
  ) +
  scale_x_discrete(
    expand = c(0, 0),
    labels = parse(text = dimnames(ind_schools_draws)[[3]])
  )

plot(parcoord_schools)

# scatterplot with divergences
div_style <- scatter_style_np(div_color = "green", div_size = 2.5, div_alpha = 0.75)
scatter_schools <-
  mcmc_scatter(
    schools_draws,
    size = 1.5,
    alpha = 2/3,
    pars = c("theta[2]", "tau"),
    transform = list(tau = "log"),
    np = schools_diagnostics,
    np_style = div_style
  ) +
  labs(x = expression(theta[2]), y = expression(log(tau))) +
  xaxis_title(size = rel(1.25)) +
  yaxis_title(size = rel(1.25))

plot(scatter_schools)

print(ind_mod)
plot(schools_mod)
plot(ind_mod)
#lp = log posterior
pairs(ind_mod, pars = c("theta", "lp__"))

la <- extract(ind_mod, permuted = TRUE) # return a list of arrays 
mu <- la$mu 

### return an array of three dimensions: iterations, chains, parameters 
a <- extract(schools_mod, permuted = FALSE) 

### use S3 functions on stanfit objects
a2 <- as.array(schools_mod)
m <- as.matrix(schools_mod)
d <- as.data.frame(schools_mod)
print(schools_mod, digits = 1)
