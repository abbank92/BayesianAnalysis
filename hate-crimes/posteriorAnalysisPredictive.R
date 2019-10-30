setwd("~/Documents/STAT365/FinalProject/hate-crimes")

samplesSummary(all_samples)

# b[1] = share_population_with_high_school_degree
# b[2] = share_non_citizen
# b[3] = share_white_poverty
# b[4] = gini_index
# b[5] = share_non_white
# b[6] = share_voters_voted_trump

# b[1], b[4] only predictors with a 95% BCI all positive

Cmodel$resetData()

nreps <- 100
N <- length(hate_crimes$avg_hatecrimes_per_100k_fbi)
y_replicated <- array(NA, c(nreps, N))

for (i in 1:nreps) {
  print(i)
  ind <- sample(5000:10000, 1)
  
  for (peram in colnames(all_samples)) {
    Cmodel[[peram]] <- all_samples[ind, peram]
  }
  
  Cmodel$calculate() #propagates everything all the way down
  
  Cmodel$simulate("y")
  y_replicated[i, 1:N] <- Cmodel$y
}

#Visual Checks

par(mfrow = c(5,4), mai = rep(.2,4))
xlim <- c(-5, 17)
breaks <- seq(-5, 17, by=2)
hist(hate_crimes$avg_hatecrimes_per_100k_fbi, main="", xlim=xlim, breaks=breaks, xaxt="n", yaxt="n", border='blue')
for (i in 1:19) hist(y_replicated[i,], main = "", xlim = xlim, breaks = breaks, xaxt = "n", yaxt = "n")


#Numerical Checks

par (mfrow = c(1,1), mar = c(2,5,5,3))

#try different summary statistics
T <- function(y) mean(y, na.rm = TRUE)
T_rep <- numeric(nreps)
for (i in 1:nreps) {
  T_rep[i] <- T(y_replicated[i, 1:N])
}
hist(T_rep, breaks = 20, main = "Histogram of Mean")
abline(v = T(hate_crimes$avg_hatecrimes_per_100k_fbi), col = "red", lwd = 2, lty = 2)


T <- function(y) diff(range(y, na.rm = TRUE))
T_rep <- numeric(nreps)
for (i in 1:nreps) {
  T_rep[i] <- T(y_replicated[i, 1:N])
}
hist(T_rep, breaks = 20, main = "Histogram of Range")
abline(v = T(hate_crimes$avg_hatecrimes_per_100k_fbi), col = "red", lwd = 2, lty = 2)
