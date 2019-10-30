#Final project

setwd("~/Documents/STAT365/FinalProject/hate-crimes")
list.files()

hate_crimes <- read.csv("hate_crimes2.csv")
X_data <- as.matrix(hate_crimes[,2:10])
names(hate_crimes)

library(nimble)
library(basicMCMCplots)
library(coda) 

fit <- lm(avg_hatecrimes_per_100k_fbi ~ median_household_income + share_unemployed_seasonal + share_population_in_metro_areas + share_population_with_high_school_degree + share_non_citizen + share_white_poverty + gini_index + share_non_white + share_voters_voted_trump,
          data = hate_crimes)
summary(fit)






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## First model using all predictors

code <- nimbleCode({
  b0 ~ dnorm(0, sd=10000 )
  for (i in 1:N_predictors) {
    b[i] ~ dnorm(0, sd=10000)
  }
  sigma_state ~ dunif(0, 10000)
  sigma_obs ~ dunif(0, 10000)
  
  for (i in 1:N_rows) {
    x_state[i] ~ dnorm(0, sd=sigma_state)
    mu[i] <- b0 + sum(b[1:N_predictors] * X_data[i,1:N_predictors]) + x_state[i]
    y[i] ~ dnorm(mu[i], sd=sigma_obs)
  }
})
constants <- list(N_predictors = ncol(X_data),
                  N_rows = nrow(hate_crimes),
                  X_data = X_data)
data <- list(y = hate_crimes$avg_hatecrimes_per_100k_fbi)
inits <- list(b0 = 0, b = rep(0,ncol(X_data)),
              sigma_state = 1, sigma_obs = 1,
              x_state = rep(0, nrow(hate_crimes)))


Rmodel <- nimbleModel(code, constants, data, inits)
conf <- configureMCMC(Rmodel)
Rmcmc <- buildMCMC(conf, enableWAIC = TRUE)
compiledList <- compileNimble(list(Rmodel, Rmcmc))
Cmcmc <- compiledList[[2]]

set.seed(0)
out <- runMCMC(Cmcmc, WAIC = TRUE)

out$WAIC #51027.56







#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Second model removing random effect for state

code <- nimbleCode({
  b0 ~ dnorm(0, sd=10000)
  for (i in 1:N_predictors) {
    b[i] ~ dnorm(0, sd=10000)
  }
  sigma_obs ~ dunif(0, 10000)
  
  for (i in 1:N_rows) {
    mu[i] <- b0 + sum(b[1:N_predictors] * X_data[i,1:N_predictors])
    y[i] ~ dnorm(mu[i], sd=sigma_obs)
  }
})
constants <- list(N_predictors = ncol(X_data),
                  N_rows = nrow(hate_crimes),
                  X_data = X_data)
data <- list(y = hate_crimes$avg_hatecrimes_per_100k_fbi)
inits <- list(b0 = 0, b = rep(0,ncol(X_data)),
              sigma_obs = 1)


Rmodel <- nimbleModel(code, constants, data, inits)
conf <- configureMCMC(Rmodel)
Rmcmc <- buildMCMC(conf, enableWAIC = TRUE)
compiledList <- compileNimble(list(Rmodel, Rmcmc))
Cmcmc <- compiledList[[2]]

set.seed(0)
out <- runMCMC(Cmcmc, WAIC = TRUE)

out$WAIC #189.54







#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Third model using log(mu)

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
constants <- list(N_predictors = ncol(X_data),
                  N_rows = nrow(hate_crimes),
                  X_data = X_data)
data <- list(y = hate_crimes$avg_hatecrimes_per_100k_fbi)
inits <- list(b0 = 0, b = rep(0,ncol(X_data)),
              sigma_obs = 1)


Rmodel <- nimbleModel(code, constants, data, inits)
conf <- configureMCMC(Rmodel)
Rmcmc <- buildMCMC(conf, enableWAIC = TRUE)
compiledList <- compileNimble(list(Rmodel, Rmcmc))
Cmcmc <- compiledList[[2]]

set.seed(0)
out <- runMCMC(Cmcmc, WAIC = TRUE)

out$WAIC #178.64


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Fourth model using only variables that look more significant
## and center some

X_data3 <- as.matrix(hate_crimes[,c(5:10)])
# X_data3[,1] <- X_data3[,1] - mean(X_data3[,1])
# X_data3[,3] <- X_data3[,3] - mean(X_data3[,3])

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
data <- list(y = hate_crimes$avg_hatecrimes_per_100k_fbi)
inits <- list(b0 = 0, b = rep(0,ncol(X_data3)),
              sigma_obs = 1)


Rmodel <- nimbleModel(code, constants, data, inits)
conf <- configureMCMC(Rmodel)
Rmcmc <- buildMCMC(conf, enableWAIC = TRUE)
compiledList <- compileNimble(list(Rmodel, Rmcmc))
Cmcmc <- compiledList[[2]]

set.seed(0)
out <- runMCMC(Cmcmc, WAIC = TRUE)

out$WAIC #172.21






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## One more

X_data4 <- as.matrix(hate_crimes[,c(3,5:6,8:9)])

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
constants <- list(N_predictors = ncol(X_data4),
                  N_rows = nrow(hate_crimes),
                  X_data = X_data4)
data <- list(y = hate_crimes$avg_hatecrimes_per_100k_fbi)
inits <- list(b0 = 0, b = rep(0,ncol(X_data4)),
              sigma_obs = 1)


Rmodel <- nimbleModel(code, constants, data, inits)
conf <- configureMCMC(Rmodel)
Rmcmc <- buildMCMC(conf, enableWAIC = TRUE)
compiledList <- compileNimble(list(Rmodel, Rmcmc))
Cmcmc <- compiledList[[2]]

set.seed(0)
out <- runMCMC(Cmcmc, WAIC = TRUE)

out$WAIC #187.24
#cannot seem to make it better than 170, so we select model 4
