read.csv(DA_Course.Rproj)
htmlpreview.github.io

#install.packages("decisionSupport")
source("monte_carlo_simulation.R")

install.packages("truncnorm")
# Load required library
library(truncnorm)

# Simulations
n_sim <- 10000

# Helper function for positive normal
rposnorm <- function(n, mean, sd, min, max) {
  rtruncnorm(n, a = min, b = max, mean = mean, sd = sd)
}

# Inputs from CSV (converted manually from the data)
soil_inputs <- rposnorm(n_sim, mean = (42+83)/2, sd = (83-42)/4, min = 42, max = 83)
intercrop_seeds <- rposnorm(n_sim, mean = (13+25)/2, sd = (25-13)/4, min = 13, max = 25)
training <- rposnorm(n_sim, mean = (17+42)/2, sd = (42-17)/4, min = 17, max = 42)
labour <- rposnorm(n_sim, mean = (25+67)/2, sd = (67-25)/4, min = 25, max = 67)
soil_maintenance <- rposnorm(n_sim, mean = (25+42)/2, sd = (42-25)/4, min = 25, max = 42)

# Crop yield (kg/acre) and price ($/kg)
maize_yield <- rposnorm(n_sim, mean = (345+518)/2, sd = (518-345)/4, min = 345, max = 518)
maize_price <- rposnorm(n_sim, mean = (0.2+0.3)/2, sd = (0.3-0.2)/4, min = 0.2, max = 0.3)

cowpea_yield <- rposnorm(n_sim, mean = (150+217)/2, sd = (217-150)/4, min = 150, max = 217)
cowpea_price <- rposnorm(n_sim, mean = (0.5+0.6)/2, sd = (0.6-0.5)/4, min = 0.5, max = 0.6)

bean_yield <- rposnorm(n_sim, mean = (200+300)/2, sd = (300-200)/4, min = 200, max = 300)
bean_price <- rposnorm(n_sim, mean = (0.4+0.5)/2, sd = (0.5-0.4)/4, min = 0.4, max = 0.5)

# Revenue
revenue <- (maize_yield * maize_price) + 
  (cowpea_yield * cowpea_price) + 
  (bean_yield * bean_price)

# Total cost
total_cost <- soil_inputs + intercrop_seeds + training + labour + soil_maintenance

# Net Present Value (NPV)
npv <- revenue - total_cost

# Summary and visualization
summary(npv)
hist(npv, breaks = 50, col = "skyblue", main = "NPV Distribution", xlab = "NPV (USD)")
cat("Probability of positive NPV:", mean(npv > 0) * 100, "%\n")
install.packages("truncnorm")  # Run once
library(truncnorm)             # Load
# Install package if needed
# install.packages("truncnorm")

# Load required library
library(truncnorm)

# Define helper function for truncated normal
rposnorm <- function(n, mean, sd, min, max) {
  rtruncnorm(n, a = min, b = max, mean = mean, sd = sd)
}

# Number of simulations
n_sim <- 10000
# Cost components (from your CSV)
soil_inputs <- rposnorm(n_sim, mean = 62.5, sd = 10.25, min = 42, max = 83)
intercrop_seeds <- rposnorm(n_sim, mean = 19, sd = 3, min = 13, max = 25)
training <- rposnorm(n_sim, mean = 29.5, sd = 6.25, min = 17, max = 42)
labour <- rposnorm(n_sim, mean = 46, sd = 10.5, min = 25, max = 67)
soil_maintenance <- rposnorm(n_sim, mean = 33.5, sd = 4.25, min = 25, max = 42)

# Crop yields and prices
maize_yield <- rposnorm(n_sim, mean = 431.5, sd = 43.25, min = 345, max = 518)
maize_price <- rposnorm(n_sim, mean = 0.25, sd = 0.025, min = 0.2, max = 0.3)

cowpea_yield <- rposnorm(n_sim, mean = 183.5, sd = 16.75, min = 150, max = 217)
cowpea_price <- rposnorm(n_sim, mean = 0.55, sd = 0.025, min = 0.5, max = 0.6)

bean_yield <- rposnorm(n_sim, mean = 250, sd = 25, min = 200, max = 300)
bean_price <- rposnorm(n_sim, mean = 0.45, sd = 0.025, min = 0.4, max = 0.5)

# Revenue
revenue <- (maize_yield * maize_price) + 
  (cowpea_yield * cowpea_price) + 
  (bean_yield * bean_price)

# Total cost
total_cost <- soil_inputs + intercrop_seeds + training + labour + soil_maintenance

# Net Present Value (NPV)
npv <- revenue - total_cost

# Results
summary(npv)
hist(npv, breaks = 50, col = "skyblue", main = "NPV Distribution", xlab = "NPV (USD)")
cat("Probability of positive NPV:", round(mean(npv > 0) * 100, 2), "%\n")
