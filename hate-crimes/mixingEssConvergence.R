setwd("~/Documents/STAT365/FinalProject/hate-crimes")

hate_crimes <- read.csv("hate_crimes2.csv")
X_data <- as.matrix(hate_crimes[,2:10])
names(hate_crimes)

library(nimble)
library(basicMCMCplots)
library(coda)

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
Rmcmc <- buildMCMC(conf)
compiledList <- compileNimble(list(Rmodel, Rmcmc))
Cmodel <- compiledList[[1]]
Cmcmc <- compiledList[[2]]

set.seed(0)
out <- runMCMC(Cmcmc, niter = 100000, nburnin = 10000, nchains = 3,
               samplesAsCodaMCMC = TRUE)


gelman.diag(out)

all_samples_bad <- rbind(out$chain1,out$chain2,out$chain3)

samplesPlot(scale(all_samples_bad), ind=260000:270000, file = "samplesPlotBad.pdf")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## So now we're gonna center some covariants
## And try slice sampling

X_data3 <- as.matrix(hate_crimes[,c(5:10)])
X_data3[,1] <- X_data3[,1] - mean(X_data3[,1])
X_data3[,3] <- X_data3[,3] - mean(X_data3[,3])
X_data3[,4] <- X_data3[,4] - mean(X_data3[,4])

code <- nimbleCode({
  b0 ~ dnorm(0, sd=10000)
  for (i in 1:N_predictors) {
    b[i] ~ dnorm(0, sd=10000)
  }
  sigma_obs ~ dunif(0, 10000)
  
  for (i in 1:N_rows) {
    log(mu[i]) <- b0 + 
      sum(b[1:N_predictors] * X_data[i,1:N_predictors])
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
conf <- configureMCMC(Rmodel, onlySlice = TRUE)
Rmcmc <- buildMCMC(conf)
compiledList <- compileNimble(list(Rmodel, Rmcmc))
Cmcmc <- compiledList[[2]]

set.seed(0)
out <- runMCMC(Cmcmc, niter = 100000, nburnin = 10000, nchains = 3,
               samplesAsCodaMCMC = TRUE)

#before I got this beautiful result,
#I was not getting convergence until I centered three covariants
#(centering the gini_index coefficient made the biggest difference)

gelman.diag(out)
all_samples <- rbind(out$chain1,out$chain2,out$chain3)
effectiveSize(all_samples)


samplesPlot(scale(all_samples), ind=260000:270000, file = "samplesPlot.pdf")
