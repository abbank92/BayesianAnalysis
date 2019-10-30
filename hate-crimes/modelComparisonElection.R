setwd("~/Documents/STAT365/FinalProject/hate-crimes")

fit2 <- lm(hate_crimes_per_100k_splc ~ median_household_income + share_unemployed_seasonal + share_population_in_metro_areas + share_population_with_high_school_degree + share_non_citizen + share_white_poverty + gini_index + share_non_white + share_voters_voted_trump,
          data = hate_crimes)
summary(fit2)




code <- nimbleCode({
  b0 ~ dnorm(0, sd=10000)
  for (i in 1:N_predictors) {
    b[i] ~ dnorm(0, sd=10000)
  }
  sigma_obs ~ dunif(0, 10000)
  
  for (i in 1:N_rows) {
    log(mu[i]) <- b0 + sum(b[1:N_predictors] * X_data[i,1:N_predictors])
    y[i] ~ dnorm(mu[i], sd=sigma_obs)
  }
})
constants <- list(N_predictors = ncol(X_data3),
                  N_rows = nrow(hate_crimes),
                  X_data = X_data3)
data <- list(y = hate_crimes$hate_crimes_per_100k_splc)
inits <- list(b0 = 0, b = rep(0,ncol(X_data3)),
              sigma_obs = 1)


Rmodel <- nimbleModel(code, constants, data, inits)
conf <- configureMCMC(Rmodel)
Rmcmc <- buildMCMC(conf)
compiledList <- compileNimble(list(Rmodel, Rmcmc))
Cmodel <- compiledList[[1]]
Cmcmc <- compiledList[[2]]

set.seed(10)
out <- runMCMC(Cmcmc, niter = 1000000, nburnin = 10000, nchains = 3,
               samplesAsCodaMCMC = TRUE)

gelman.diag(out)

all_samples2 <- rbind(out$chain1,out$chain2,out$chain3)
effectiveSize(all_samples2)

samplesSummary(all_samples2)

# b[1] = share_population_with_high_school_degree
# b[2] = share_non_citizen
# b[3] = share_white_poverty
# b[4] = gini_index
# b[5] = share_non_white
# b[6] = share_voters_voted_trump





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Now we use the same predictors to see which ones most effect the difference
# between 2010-2015 data and 2016 data

code <- nimbleCode({
  b0 ~ dnorm(0, sd=10000)
  for (i in 1:N_predictors) {
    b[i] ~ dnorm(0, sd=10000)
  }
  sigma_obs ~ dunif(0, 10000)
  
  for (i in 1:N_rows) {
    log(mu[i]) <- b0 + sum(b[1:N_predictors] * X_data[i,1:N_predictors])
    y[i] ~ dnorm(mu[i], sd=sigma_obs)
  }
})
constants <- list(N_predictors = ncol(X_data3),
                  N_rows = nrow(hate_crimes),
                  X_data = X_data3)
data <- list(y = hate_crimes$hate_crimes_per_100k_splc*(366/10) - hate_crimes$avg_hatecrimes_per_100k_fbi)
inits <- list(b0 = 0, b = rep(0,ncol(X_data3)),
              sigma_obs = 1)


Rmodel <- nimbleModel(code, constants, data, inits)
conf <- configureMCMC(Rmodel)
Rmcmc <- buildMCMC(conf)
compiledList <- compileNimble(list(Rmodel, Rmcmc))
Cmodel <- compiledList[[1]]
Cmcmc <- compiledList[[2]]

set.seed(10)
out <- runMCMC(Cmcmc, niter = 1000000, nburnin = 10000, nchains = 3,
               samplesAsCodaMCMC = TRUE)

gelman.diag(out)

all_samples3 <- rbind(out$chain1,out$chain2,out$chain3)
effectiveSize(all_samples3)

samplesSummary(all_samples3)

# b[1] = share_population_with_high_school_degree
# b[2] = share_non_citizen
# b[3] = share_white_poverty
# b[4] = gini_index
# b[5] = share_non_white
# b[6] = share_voters_voted_trump

