
library(ggplot2)
library(coda) 

#PSet 4
tfuel <- 182
s_mu <- 34
s_sigma <- 20
userate <- 18
use_sd <- 2


#prior
prior <- function(fuel) {
  ifelse(fuel >= 0 & fuel <= tfuel, 1/tfuel, 0)
}

#likelihood
likely <- function(fuel, measurement) {
  dnorm(measurement, mean = fuel, sd = s_sigma)
}

#Metropolis Hastings
metropolis_hastings <- function(sim, initial_fuel, measurement) {
  samples <- numeric(sim)
  current_fuel <- initial_fuel
  samples[1] <- current_fuel
  
  for (i in 2:sim) {
  
    proposed_fuel <- rnorm(1, mean = current_fuel, sd = 10) 
    
   
    if (proposed_fuel >= 0 && proposed_fuel <= tfuel) {
      acceptance_ratio <- (likely(proposed_fuel, measurement) * prior(proposed_fuel)) / 
        (likely(current_fuel, measurement) * prior(current_fuel))
      
      if (runif(1) < min(1, acceptance_ratio)) {
        current_fuel <- proposed_fuel
      }
    }
    samples[i] <- current_fuel
  }
  return(samples)
}


#Implementing Hastings algorithm
sim = 15000  
initial_fuel <- runif(1, 0, tfuel) 
measurement <- s_mu 
post_hastings <- metropolis_hastings(sim, initial_fuel, measurement)
#removing burn-in
burn_in = 1000  # Remove the first 1000 samples as burn-in
post_hastings <- post_hastings[(burn_in + 1):sim]


#assessing convergence visually
plot(post_hastings, type = "l", main = "Trace Plot of Fuel Samples", xlab = "Iteration", ylab = "Fuel Level")

#assessing accuracy of convergence with effective sample size approach
effectiveSize(post_hastings)

#Assessing accuracy using positive control
true_fuel <- 44 #using sensor reading as positive control
simulated_measurement <- rnorm(1, mean = true_fuel, sd = s_sigma)

# Running Hastings with the simulated measurement
fuel_positive <- metropolis_hastings(sim, initial_fuel, simulated_measurement)
fuel_positivex <- fuel_positive[(burn_in + 1):sim]

# Estimating mode 
d_positive <- density(fuel_positivex)
est_positive <- d_positive$x[which.max(d_positive$y)]

# Calculating error
error <- abs(est_positive - true_fuel)
cat("Error (Positive Control):", error, "\n")

#determining number of runs for convergence
chains = 4
fuel_chains <- matrix(NA, nrow = sim, ncol = chains)
for (i in 1:chains) {
  initial_fuel <- runif(1, 0, tfuel)
  fuel_chains[, i] <- metropolis_hastings(sim, initial_fuel, measurement)
}

#plotting
matplot(fuel_chains, type = "l", main = "Trace Plots of Multiple Chains", xlab = "Iteration", ylab = "Fuel Level", col = 1:chains)

#comparison with BMC Samples
set.seed(42)
sim = 15000
fuel_samples = runif(sim, 0, tfuel)
sensor_noise = rnorm(sim, mean = 0, sd = s_sigma)
measured_fuel = fuel_samples + sensor_noise

#Likelihood
likely_mc <- dnorm(measured_fuel, mean = s_mu, sd = s_sigma)
weights_mc <- likely_mc / sum(likely_mc)
post_mc <- sample(fuel_samples, size = sim, replace = TRUE, prob = weights_mc)

#Estimating mode and error
density_bmc <- density(post_mc)
est_bmc <- density_bmc$x[which.max(density_bmc$y)]
#results_bmc$estimated_mode[i] <- estimated_mode_bmc

# Check convergence criterion (example: tolerance on mode)
error_bmc <- abs(est_bmc - true_fuel) # true_fuel from your positive control
cat("BMC Error =", error_bmc, "\n")







