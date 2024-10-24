library(HMM)

# Define the states and observation symbols
states <- c("WR1", "WR2", "WR3","WR4","WR5","CHECKDOWN")
symbols <- c("1","2","3","4","5","CD")

# Define the transition matrix
transitionMatrix <- matrix(c(0.7, 0.2, 0.1,
                             0.1, 0.6, 0.3,
                             0.2, 0.3, 0.5),
                           nrow = 3, byrow = TRUE)

# Define the emission matrix
emissionMatrix <- matrix(c(0.5, 0.4, 0.1,
                           0.1, 0.3, 0.6,
                           0.4, 0.3, 0.3),
                         nrow = 3, byrow = TRUE)

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
