library(ggplot2)
library(decisionSupport)
input_estimates <- data.frame(
  variable = c(
    "sheep_income",
    "sheep_cost",
    "apple_income",
    "apple_cost",
    "discount_rate"
  ),
  lower = c(35, 15, 25, 20, 10),
  median = NA,
  upper = c(5500, 2500, 65000, 30000, 10),
  distribution = c("posnorm", "posnorm", "posnorm", "posnorm", "const"),
  label = c(
    "Income from sheep (euro/year)",
    "Cost of sheep (euro/year)",
    "Income from apple (euro/year)",
    "Cost of apple (euro/year)",
    "Discount Rate"
  )
)

# show the result
input_estimates


model_function <- function() {
  # Estimate the income in a normal season
  AF_income <- sheep_income + apple_income
  AF_cost <- sheep_cost + apple_cost
  # Estimate the final results from the model
  AF_final_result <- AF_income - AF_cost
  # baseline with sheep only
  sheep_only <- sheep_income - sheep_cost
  # should I plant trees in the sheep pastures? 
  Decision_benefit <- AF_final_result - sheep_only
  #Calculating NPV
  #AF System
  AF_NPV <- discount(AF_final_result,
                     discount_rate = discount_rate,
                     calculate_NPV = TRUE)
  #NVP of AF system
  
  NPV_sheep_only <- discount(sheep_only,
                             discount_rate = discount_rate,
                             calculate_NPV = TRUE) 
  #NVP of grassland
  
  NPV_decision <- discount(Decision_benefit,
                           discount_rate = discount_rate,
                           calculate_NPV = TRUE)
  # Generate the list of outputs from the Monte Carlo simulation
  return(list(NPV_Agroforestry_System = AF_NPV,
              NPV_Treeless_System = NPV_sheep_only,
              NPV_decision = NPV_decision))
}



apple_sheep_mc_simulation <- mcSimulation(estimate = as.estimate(input_estimates),
                                          model_function = model_function,
                                          numberOfModelRuns = 5000,
                                          functionSyntax = "plainNames"
                                          )



plot_distributions(
  mcSimulation_object = apple_sheep_mc_simulation,
  vars = c("NPV_decision"),
  method = 'smooth_simple_overlay',
  base_size = 7,
  x_axis_name = "Outcome of AF intervention",
  scale_x_continuous(
    labels = function(x)
      x / 100000
  ),
  ggtitle("Net Present Value of the system"),
  legend.position = "bottom")
# assign the results of x and y to a data frame

df <- data.frame(apple_sheep_mc_simulation$x, apple_sheep_mc_simulation$y[1:3])
EVPI <- multi_EVPI(mc = df, first_out_var = "NPV_Agroforestry_System")

# plot the EVPI results for the decision
plot_evpi(EVPIresults = EVPI, decision_vars = "NPV_decision")
