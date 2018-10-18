library(rstan)
library(bayesplot)

schools_data <- list(
  y = c(28,  8,-3,  7,-1,  1, 18, 12),
  sigma = c(15, 10, 16, 11,  9, 11, 10, 18),
  J = 8
)
pooled_mod <- stan("stan/pooled_schools.stan", data = schools_data, seed = 1000)
pooled_schools_draws <- as.array(pooled_mod)
pooled_diagnostics <- nuts_params(pooled_mod)


# parallel coordinates
color_scheme_set("darkgray")
theme_update(axis.text = element_text(size = 20))
div_style <- parcoord_style_np(div_color = "green", div_size = 0.15, div_alpha = 0.4)
parcoord_schools <-
  mcmc_parcoord(
    pooled_schools_draws,
    size = 0.15,
    alpha = 0.2,
    np = pooled_diagnostics,
    np_style = div_style
  ) +
  scale_x_discrete(
    expand = c(0, 0),
    labels = parse(text = dimnames(pooled_schools_draws)[[3]])
  )

plot(parcoord_schools)

plot(pooled_mod)

print(pooled_mod)
plot(schools_mod)
plot(ind_mod)

la <- extract(ind_mod, permuted = TRUE) # return a list of arrays 
mu <- la$mu 

### return an array of three dimensions: iterations, chains, parameters 
a <- extract(schools_mod, permuted = FALSE) 

### use S3 functions on stanfit objects
a2 <- as.array(schools_mod)
m <- as.matrix(schools_mod)
d <- as.data.frame(schools_mod)
print(pooled_mod, digits = 1)
