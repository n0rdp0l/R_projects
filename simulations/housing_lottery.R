
calculate_probability <- function(weeks, overall_prob) {
  # Calculate the minimum weekly probability of winning
  weekly_prob_win <- 1 - (1 - overall_prob)^(1/weeks)
  
  return(weekly_prob_win)
}

weeks <- 12

# Generate a sequence of overall probabilities
overall_probs <- seq(0, 1, by = 0.01)

# Calculate the corresponding weekly probabilities
weekly_probs <- sapply(overall_probs, calculate_probability, weeks = weeks)


# Plot the results
plot(weekly_probs, overall_probs, type = 'l', ylab = 'Overall Probability of Winning', xlab = 'Weekly Probability of Winning', main = paste('Relationship for', weeks, 'weeks'))

weekly_probability(weeks, 0.30)



