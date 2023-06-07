# Monty Hall Problem Simulation in R

library(parallel)
library(plotly)

# Detect the number of cores
numCores <- detectCores()

# Use max cores - 2
numCoresToUse <- if(numCores > 2) numCores - 2 else 1

set.seed(123)

# Number of simulations
n_sim <- 100000

# Interval for calculating probabilities
interval <- 1000

# Generate doors for the prizes (1: Car, 0: Goat)
prize_door <- sample(1:3, n_sim, replace = TRUE)

# Generate initial choices
initial_choice <- sample(1:3, n_sim, replace = TRUE)

# Generate the door that Monty will open
monty_opens <- unlist(mclapply(1:n_sim, function(i) {
  possible_doors <- setdiff(1:3, c(prize_door[i], initial_choice[i]))
  sample(possible_doors, 1)
}, mc.cores = numCoresToUse))

# Switch door strategy
switch_door <- unlist(mclapply(1:n_sim, function(i) setdiff(1:3, c(initial_choice[i], monty_opens[i])), mc.cores = numCoresToUse))

# Evaluate the outcome of staying with initial choice or switching
stay_outcome <- ifelse(prize_door == initial_choice, 1, 0)
switch_outcome <- ifelse(prize_door == switch_door, 1, 0)

# Initialize vectors to store cumulative probabilities
stay_prob <- numeric(n_sim %/% interval)
switch_prob <- numeric(n_sim %/% interval)

# Initialize a plotly graph
p <- plot_ly() %>%
  layout(title = "Convergence of Win Probabilities in Monty Hall Problem",
         xaxis = list(title = "Iteration"),
         yaxis = list(title = "Win Probability"))

# Calculate cumulative probabilities at intervals
for (i in seq(interval, n_sim, by = interval)) {
  stay_prob[i %/% interval] <- sum(stay_outcome[1:i]) / i
  switch_prob[i %/% interval] <- sum(switch_outcome[1:i]) / i
  
  # Add data to the plot
  p <- add_trace(p, x = ~i, y = ~stay_prob[i %/% interval], mode = "lines", name = "Stay")
  p <- add_trace(p, x = ~i, y = ~switch_prob[i %/% interval], mode = "lines", name = "Switch")
  
  # Render the plot
  print(p)
  
  # Allow the plot to update
  Sys.sleep(0.01)
}
