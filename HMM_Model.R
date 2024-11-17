library(tidyverse)
library(HMM)
combined_all_features <- combined_all_features %>% filter(rank !=6)
prop.table(table(targetted_rank$rank,targetted_rank$rank), margin = 1)
prop.table(table(combined_all_features$score_bucket, combined_all_features$rank), margin = 1)[1,]
prop.table(table(combined_all_features$down, combined_all_features$rank), margin = 1)
prop.table(table(combined_all_features$yardsToGo, combined_all_features$rank), margin = 1)
prop.table(table(combined_all_features$pff_manZone, combined_all_features$rank), margin = 1)
prop.table(table(combined_all_features$defendersInBox, combined_all_features$rank), margin = 1)
prop.table(table(combined_all_features$pff_runPassOption, combined_all_features$rank), margin = 1)
prop.table(table(combined_all_features$personnelD, combined_all_features$rank), margin = 1)

#write.csv(combined_all_features,"features.csv")
ggplot(combined_all_features, aes(x=diff_1)) + 
  geom_density()
ggplot(combined_all_features, aes(x=lag_1)) + 
  geom_density()
ggplot(combined_all_features, aes(x=(s_1))) + 
  geom_density()
ggplot(combined_all_features, aes(x=diff_2)) + 
  geom_density()
ggplot(combined_all_features, aes(x=lag_2)) + 
  geom_density()
ggplot(combined_all_features, aes(x=s_2)) + 
  geom_density()


#get weibull shape and scale to model speed
s_1_dist <- combined_all_features$s_1
s_1_dist[s_1_dist==0]<- 0.0001
s_2_dist <- combined_all_features$s_2
s_2_dist[s_2_dist==0]<- 0.0001
s_3_dist <- combined_all_features$s_3
s_3_dist[s_3_dist==0]<- 0.0001
s_4_dist <- combined_all_features$s_4
s_4_dist[s_4_dist==0]<- 0.0001
s_5_dist <- combined_all_features$s_5
s_5_dist[s_5_dist==0]<- 0.0001
library(MASS)


# Fit Weibull distribution using MASS
s_1_fit <- fitdistr(s_1_dist, densfun = "weibull")$estimate
s_2_fit <- fitdistr(s_2_dist, densfun = "weibull")$estimate
s_3_fit <- fitdistr(s_3_dist, densfun = "weibull")$estimate
s_4_fit <- fitdistr(s_4_dist, densfun = "weibull")$estimate
s_5_fit <- fitdistr(s_5_dist, densfun = "weibull")$estimate
#get dist for diff(laplace)
calc_laplace_params <- function(x) {
  # Location parameter (μ) is the median
  location <- median(x)
  
  # Scale parameter (b) is the mean absolute deviation from the median
  scale <- mean(abs(x - location))
  
  return(list(
    location = location,
    scale = scale
  ))
}
diff_1_fit <- calc_laplace_params(combined_all_features$diff_1)
diff_2_fit <- calc_laplace_params(combined_all_features$diff_2)
diff_3_fit <- calc_laplace_params(combined_all_features$diff_3)
diff_4_fit <- calc_laplace_params(combined_all_features$diff_4)
diff_5_fit <- calc_laplace_params(combined_all_features$diff_5)
lag_1_fit <- calc_laplace_params(combined_all_features$lag_1)
lag_2_fit <- calc_laplace_params(combined_all_features$lag_2)
lag_3_fit <- calc_laplace_params(combined_all_features$lag_3)
lag_4_fit <- calc_laplace_params(combined_all_features$lag_4)
lag_5_fit <- calc_laplace_params(combined_all_features$lag_5)
# Define the states and observation symbols
states <- c("WR1", "WR2", "WR3","WR4","WR5")
symbols <- c("1","2","3","4","5")

# Define the transition matrix


transitionMatrix <- matrix(c(0.2953958,0.2240041386,0.1989136058,0.1639937920,0.1176927056,
                             0.2953958,0.2240041386,0.1989136058,0.1639937920,0.1176927056,
                             0.2953958,0.2240041386,0.1989136058,0.1639937920,0.1176927056,
                             0.2953958,0.2240041386,0.1989136058,0.1639937920,0.1176927056,
                             0.2953958,0.2240041386,0.1989136058,0.1639937920,0.1176927056),
                           nrow = 5, byrow = TRUE)
#define categorical and continuous features
# Example parameters for each hidden state

# Categorical emission probabilities for each state
categorical_probs <- list(
  S1 = rep(1/20, 20),  # Uniform probabilities for 20 categories in state S1
  S2 = runif(20),      # Random probabilities for state S2 (normalize to sum to 1)
  S3 = runif(20)       # Random probabilities for state S3 (normalize to sum to 1)
)

# Normalize the probabilities to sum to 1 for each state
categorical_probs$S2 <- categorical_probs$S2 / sum(categorical_probs$S2)
categorical_probs$S3 <- categorical_probs$S3 / sum(categorical_probs$S3)

# Continuous emission parameters for each state
# Use mean and sd for Gaussian, shape and scale for Weibull, and location and scale for Laplace
continuous_params <- list(
  S1 = list(type = "Gaussian", mean = 0, sd = 2),            # Gaussian
  S2 = list(type = "Weibull", shape = 2, scale = 3),         # Weibull
  S3 = list(type = "Laplace", location = -1, scale = 1.5)    # Laplace
)

# Custom Laplace density function
dlaplace <- function(x, location = 0, scale = 1) {
  1 / (2 * scale) * exp(-abs(x - location) / scale)
}

# Function to calculate emission probability given state, categorical value, and continuous value
emission_probability <- function(state, categorical_val, continuous_val) {
  # Get categorical probability for the given category
  cat_prob <- categorical_probs[[state]][categorical_val]
  
  # Get continuous probability based on the type of distribution
  cont_params <- continuous_params[[state]]
  if (cont_params$type == "Gaussian") {
    cont_prob <- dnorm(continuous_val, mean = cont_params$mean, sd = cont_params$sd)
  } else if (cont_params$type == "Weibull") {
    cont_prob <- dweibull(continuous_val, shape = cont_params$shape, scale = cont_params$scale)
  } else if (cont_params$type == "Laplace") {
    cont_prob <- dlaplace(continuous_val, location = cont_params$location, scale = cont_params$scale)
  } else {
    stop("Unknown distribution type.")
  }
  
  # Return joint probability assuming independence
  return(cat_prob * cont_prob)
}

# Example: Calculate the emission probability for each state
cat("S1 (Gaussian):", emission_probability("S1", categorical_val = 5, continuous_val = 3.2), "\n")
cat("S2 (Weibull):", emission_probability("S2", categorical_val = 5, continuous_val = 3.2), "\n")
cat("S3 (Laplace):", emission_probability("S3", categorical_val = 5, continuous_val = 3.2), "\n")


# Create the HMM model
hmm <- initHMM(states, symbols, transProbs = transitionMatrix, emissionProbs = emissionMatrix)

# Simulate an observation sequence
set.seed(42)
obsSeq <- simHMM(hmm, 10)$observation

# Suppose the final state is known to be "S3"
final_state <- "S3"

# Modified Viterbi function to consider known final state
viterbi_modified <- function(hmm, observation, final_state) {
  
  # Get the number of states and the length of the observation sequence
  nStates <- length(hmm$States)
  nObservations <- length(observation)
  
  # Initialize the probability and path matrices
  viterbiMatrix <- matrix(NA, nrow = nStates, ncol = nObservations)
  pathMatrix <- matrix(NA, nrow = nStates, ncol = nObservations)
  
  # Initial probabilities for the first state
  for (i in 1:nStates) {
    viterbiMatrix[i, 1] <- hmm$startProbs[i] * hmm$emissionProbs[i, match(observation[1], hmm$Symbols)]
    pathMatrix[i, 1] <- 0
  }
  
  # Fill the Viterbi matrix for the sequence except the last step
  for (t in 2:(nObservations - 1)) {
    for (j in 1:nStates) {
      maxProb <- -Inf
      maxState <- NA
      for (i in 1:nStates) {
        prob <- viterbiMatrix[i, t-1] * hmm$transProbs[i, j] * hmm$emissionProbs[j, match(observation[t], hmm$Symbols)]
        if (prob > maxProb) {
          maxProb <- prob
          maxState <- i
        }
      }
      viterbiMatrix[j, t] <- maxProb
      pathMatrix[j, t] <- maxState
    }
  }
  
  # Enforce the known final state
  finalStateIdx <- match(final_state, hmm$States)
  for (i in 1:nStates) {
    if (i == finalStateIdx) {
      viterbiMatrix[i, nObservations] <- max(viterbiMatrix[, nObservations-1] * hmm$transProbs[, i])
    } else {
      viterbiMatrix[i, nObservations] <- -Inf
    }
    pathMatrix[i, nObservations] <- which.max(viterbiMatrix[, nObservations-1] * hmm$transProbs[, i])
  }
  
  # Backtrack to find the most likely sequence of states
  path <- rep(NA, nObservations)
  path[nObservations] <- finalStateIdx
  for (t in (nObservations - 1):1) {
    path[t] <- pathMatrix[path[t + 1], t + 1]
  }
  
  # Convert state indices to state names
  stateSequence <- hmm$States[path]
  
  return(stateSequence)
}

# Run the modified Viterbi algorithm
state_sequence <- viterbi_modified(hmm, obsSeq, final_state)

# Print results
print(paste("Observation sequence:", paste(obsSeq, collapse = " ")))
print(paste("Most likely state sequence:", paste(state_sequence, collapse = " ")))
