# Load required library
library(depmixS4)
library(tidyverse)
set.seed(12345)

df <- train_features 
df <- droplevels(combined_all_features)

# Convert columns to factors
factor_columns <- c( "defendersInBox","yardsToGo","down","offenseFormation",
                    "personnelD")
                    

# Define the number of states
n_states <- 5

# Define the transition matrix



trans_matrix <- matrix(c(0.35555556,.28888889,.17777778,0.11111111,0.06666667,
                         0.35555556,.28888889,.17777778,0.11111111,0.06666667,
                         0.35555556,.28888889,.17777778,0.11111111,0.06666667,
                         0.35555556,.28888889,.17777778,0.11111111,0.06666667,
                         0.35555556,.28888889,.17777778,0.11111111,0.06666667
                
                         
                        ),
                       nrow = 5, byrow = TRUE)

# Define the initial state probabilities (using any row from the transition matrix)
initial_probs <- trans_matrix[1, ]
ntimes <- nrow(df)
# Create HMM model with covariates
#some features used multiple times to boost weight.
hmm_model <- depmix(list(  yardsToGo ~ 1, down~ 1,
                           defendersInBox ~ 1, defendersInBox ~ 1, defendersInBox ~ 1, 
                          personnelD ~ 1,personnelD ~ 1,personnelD ~ 1, offenseFormation ~1),
                    data = df, nstates = n_states, family = c(rep(list(multinomial()), length(factor_columns)+4)),ntimes=ntimes, 
                    trstart = as.vector(trans_matrix), instart= initial_probs)


# Fit the model
fitted_model <- fit(hmm_model, emcontrol = em.control(maxit = 5, tol = 1e-8), verbose = TRUE)


# Summary of the fitted model
summary(fitted_model)

getpars(fitted_model)

state_probs <- posterior(fitted_model)
combined_all_features$preds <-state_probs$state
combined_all_features$S1 <-state_probs$S1
combined_all_features$S2 <-state_probs$S2
combined_all_features$S3 <-state_probs$S3
combined_all_features$S4 <-state_probs$S4
combined_all_features$S5 <-state_probs$S5
#df <- df %>%
#  group_by(comb_id) %>%                           # Group by comb_id
#  mutate(last_frame = ifelse(frameId == max(frameId), 1, 0)) %>%  # Create binary marker
#  ungroup()   
# Create a new depmix model structure for the unseen data
#hmm_new <- depmix(
#  response = list( yardsToGo ~ 1, down~ 1,
#                   defendersInBox ~ 1, 
#                   personnelD ~ 1),
#  data = test_features,
#  nstates = n_states,  # Use the same number of states
#  family = rep(list(multinomial()), length(factor_columns)),  # Same family
#  ntimes = nrow(test_features))# Adjust for new data length


#hmm_new <- setpars(hmm_new, getpars(fitted_model))
#validObject(hmm_new)
# Adjust parameter lengths
#state_probs_new <- posterior(hmm_new)
#predicted_states_new <- state_probs_new$state
#combined_all_features$preds <-predicted_states_new
#test_preds <- combined_all_features %>% filter(week==9) %>% 
#  group_by(comb_id) %>%                           # Group by comb_id
#  mutate(last_frame = ifelse(frameId == max(frameId), 1, 0)) %>%  # Create binary marker
#  ungroup()   
#test_preds <-merge(test_preds, plays[,c("comb_id","timeToThrow")], by="comb_id")

combined_all_features<-merge(combined_all_features, plays[,c("comb_id","timeToThrow")], by="comb_id")

last_frame_all <- combined_all_features %>% #filter(week>7) %>% 
  group_by(comb_id) %>%                           # Group by comb_id
  mutate(last_frame = ifelse(frameId == max(frameId), 1, 0)) %>%  # Create binary marker
  ungroup()    %>% filter(last_frame ==1)

table(combined_all_features$rank,combined_all_features$preds)
library(caret)
cm <- caret::confusionMatrix(table(last_frame_all$rank,last_frame_all$preds))
cm
library(Metrics)
predicted <- as.factor(last_frame_all$preds)
actual <- as.factor(last_frame_all$rank)
conf_matrix <- caret::confusionMatrix(predicted, actual)

# Per-class metrics
conf_matrix$byClass

# Overall accuracy
conf_matrix$overall

# Macro-averaged F1-score (manually calculated)
macro_f1 <- mean(conf_matrix$byClass[, "F1"])
cat("Macro-averaged F1:", macro_f1, "\n")

