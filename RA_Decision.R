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
hist(npv, breaks = 50, col = "green", main = "NPV Distribution", xlab = "NPV (USD)")
cat("Probability of positive NPV:", round(mean(npv > 0) * 100, 2), "%\n")
write.csv(data.frame(NPV = npv), "npv_results.csv", row.names = FALSE)
library(truncnorm)

rposnorm <- function(n, mean, sd, min, max) {
  rtruncnorm(n, a = min, b = max, mean = mean, sd = sd)
}

n_sim <- 10000
n_years <- 5
discount_rate <- 0.10

# Initial investment (Year 0 only)
soil_inputs <- rposnorm(n_sim, mean = 62.5, sd = 10.25, min = 42, max = 83)
intercrop_seeds <- rposnorm(n_sim, mean = 19, sd = 3, min = 13, max = 25)
training <- rposnorm(n_sim, mean = 29.5, sd = 6.25, min = 17, max = 42)

initial_investment <- soil_inputs + intercrop_seeds + training

# Annual recurring cost (Year 1–5)
labour <- rposnorm(n_sim, mean = 46, sd = 10.5, min = 25, max = 67)
soil_maintenance <- rposnorm(n_sim, mean = 33.5, sd = 4.25, min = 25, max = 42)
annual_cost <- labour + soil_maintenance

# Annual revenue (Year 1–5)
maize_yield <- rposnorm(n_sim, mean = 431.5, sd = 43.25, min = 345, max = 518)
maize_price <- rposnorm(n_sim, mean = 0.25, sd = 0.025, min = 0.2, max = 0.3)

cowpea_yield <- rposnorm(n_sim, mean = 183.5, sd = 16.75, min = 150, max = 217)
cowpea_price <- rposnorm(n_sim, mean = 0.55, sd = 0.025, min = 0.5, max = 0.6)

bean_yield <- rposnorm(n_sim, mean = 250, sd = 25, min = 200, max = 300)
bean_price <- rposnorm(n_sim, mean = 0.45, sd = 0.025, min = 0.4, max = 0.5)

annual_revenue <- (maize_yield * maize_price) +
  (cowpea_yield * cowpea_price) +
  (bean_yield * bean_price)

# Discount factors for each year
discount_factors <- 1 / (1 + discount_rate)^(1:n_years)

# Cash flow simulation for each year
npv <- numeric(n_sim)
for (i in 1:n_sim) {
  cash_flows <- (annual_revenue[i] - annual_cost[i]) * discount_factors
  npv[i] <- sum(cash_flows) - initial_investment[i]
}

# Summary and visualization
summary(npv)
hist(npv, breaks = 50, col = "skyblue", main = "NPV Distribution (5-year cash flow)", xlab = "NPV (USD)")
cat("Probability of positive NPV:", round(mean(npv > 0) * 100, 2), "%\n")
# Already calculated: Base case
EVwCI <- mean(npv)  # Expected Value with Current Info

# Now calculate EVwPI (Expected Value with Perfect Info)
# Simulate decision as if you knew which simulations give best outcomes

EVwPI <- mean(pmax(npv, 0))  # Perfect decision: only invest if NPV > 0

# Value of Information
EVPI <- EVwPI - EVwCI

# Output
cat("EVwCI (Base Expected NPV):", round(EVwCI, 2), "\n")
cat("EVwPI (Perfect Info Expected NPV):", round(EVwPI, 2), "\n")
cat("EVPI (Value of Information):", round(EVPI, 2), "\n")
# 1. Expected Value with Current Information (EVwCI)
EVwCI <- mean(npv)  # Average NPV from all simulations

# 2. Expected Value with Perfect Information (EVwPI)
# Perfect decision = only proceed if NPV > 0
EVwPI <- mean(pmax(npv, 0))  # Take project only when it's profitable

# 3. Value of Perfect Information (EVPI)
EVPI <- EVwPI - EVwCI

# 4. Output results
cat("Expected Value with Current Information (EVwCI):", round(EVwCI, 2), "\n")
cat("Expected Value with Perfect Information (EVwPI):", round(EVwPI, 2), "\n")
cat("Expected Value of Perfect Information (EVPI):", round(EVPI, 2), "\n")
hist(npv, breaks = 50, col = ifelse(npv > 0, "green", "red"),
     main = "NPV Distribution (Profitable in Green)",
     xlab = "Net Present Value")
abline(v = 0, col = "black", lty = 2)
install.packages("decisionSupport")
library(decisionSupport)
mc_results <- data.frame(npv = npv)
compound_figure_(mc_results)
mc_results <- data.frame(
  npv = npv,
  revenue = revenue * 5,  # if over 5 years
  cost = total_cost,
  roi = (revenue * 5 - total_cost - initial_investment) / total_cost
)

compound_figure(mc_results)


