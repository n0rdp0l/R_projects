
calculate_probability <- function(weeks, overall_prob) {
  # Calculate the minimum weekly probability of winning (Geometric CDF)
  weekly_prob_win <- 1 - (1 - overall_prob)^(1/weeks)
  
  # Calculate alongside the number of contestants that equate this probability 
  # (2/x = prop)
  contestants <- (1/weekly_prob_win) * 2
  
  return(c(weekly_prob_win,contestants))
}



weeks <- 12

# Generate a sequence of overall probabilities
overall_probs <- seq(0, 1, by = 0.01)


# Calculate the corresponding weekly probabilities and contestants using the modified function
weekly_probs <- sapply(overall_probs, calculate_probability, weeks = weeks)

# Plot the results
plot(weekly_probs[1, ], overall_probs, type = 'l', 
     ylab = 'Overall Probability of Winning', 
     xlab = 'Weekly Probability of Winning', 
     main = paste('Relationship for', weeks, 'weeks'))

plot(weekly_probs[2, ], overall_probs, type = 'l', 
     ylab = 'Overall Probability of Winning', 
     xlab = 'Number of (weekly) Contestants',
     xlim = c(0,200),
     main = paste('Relationship for', weeks, 'weeks'))

calculate_probability(weeks, 0.50)



