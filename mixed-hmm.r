# Load required library
library(depmixS4)
library(tidyverse)
# Example data - replace this with your actual dataframe
# Ensure your data is preprocessed and has these columns as factors
df <- train_features #%>% filter(week<4) # %>% select(c(defendersInBox,
                     #              personnelID, speed_s_1))# Replace with your actual data

# Convert columns to factors
factor_columns <- c( "down","yardsToGo","score_bucket","pff_manZone","defendersInBox",
                    "personnelD", "speed_s_1", "speed_s_2", "speed_s_3", "speed_s_4", "speed_s_5","bucket_lag_1",
                    "bucket_lag_2","bucket_lag_3","bucket_lag_4","bucket_lag_5")

#df[factor_columns] <- lapply(df[factor_columns], as.factor)

# Define the number of states
n_states <- 5

# Define the transition matrix
trans_matrix <- matrix(c(0.2953958, 0.2240041386, 0.1989136058, 0.1639937920, 0.1176927056,
                         0.2953958, 0.2240041386, 0.1989136058, 0.1639937920, 0.1176927056,
                         0.2953958, 0.2240041386, 0.1989136058, 0.1639937920, 0.1176927056,
                         0.2953958, 0.2240041386, 0.1989136058, 0.1639937920, 0.1176927056,
                         0.2953958, 0.2240041386, 0.1989136058, 0.1639937920, 0.1176927056),
                       nrow = 5, byrow = TRUE)

# Define the initial state probabilities (using any row from the transition matrix)
initial_probs <- trans_matrix[1, ]
ntimes <- nrow(df)
# Create HMM model with covariates
hmm_model <- depmix(list( down ~ 1, yardsToGo ~ 1, score_bucket ~ 1, 
                          pff_manZone ~ 1,  defendersInBox ~ 1, 
                          personnelD ~ 1, speed_s_1 ~ 1, speed_s_2 ~ 1, speed_s_3 ~ 1, speed_s_4 ~ 1, speed_s_5 ~ 1,bucket_lag_1 ~ 1,
                          bucket_lag_2 ~ 1,bucket_lag_3 ~ 1,bucket_lag_4 ~ 1,bucket_lag_5 ~ 1),
                    data = df, nstates = n_states, family = rep(list(multinomial()), length(factor_columns)),ntimes=ntimes
)


# Fit the model
fitted_model <- fit(hmm_model, emcontrol = em.control(maxit = 10, tol = 1e-8), verbose = TRUE)


# Summary of the fitted model
summary(fitted_model)

getpars(fitted_model)

state_probs <- posterior(fitted_model)
train_features$preds <-state_probs$state
df <- df %>%
  group_by(comb_id) %>%                           # Group by comb_id
  mutate(last_frame = ifelse(frameId == max(frameId), 1, 0)) %>%  # Create binary marker
  ungroup()   
write.csv(df,"train_preds.csv")
# Create a new depmix model structure for the unseen data
hmm_new <- depmix(
  response = list(down ~ 1, yardsToGo ~ 1, score_bucket ~ 1, 
                  pff_manZone ~ 1,  defendersInBox ~ 1, 
                  personnelD ~ 1, speed_s_1 ~ 1, speed_s_2 ~ 1, speed_s_3 ~ 1, speed_s_4 ~ 1, speed_s_5 ~ 1,bucket_lag_1 ~ 1,
                  bucket_lag_2 ~ 1,bucket_lag_3 ~ 1,bucket_lag_4 ~ 1,bucket_lag_5 ~ 1),
  data = combined_all_features,
  nstates = fitted_model@nstates,  # Use the same number of states
  family = rep(list(multinomial()), length(factor_columns)),  # Same family
  ntimes = nrow(combined_all_features),
  trstart = as.vector(trans_matrix), instart=as.vector(trans_matrix[1,])# Adjust for new data length
)

hmm_new <- setpars(hmm_new, getpars(fitted_model))
validObject(hmm_new)
# Adjust parameter lengths
state_probs_new <- posterior(hmm_new)
predicted_states_new <- state_probs_new$state
combined_all_features$preds <-predicted_states_new
test_preds <- combined_all_features %>% filter(week==9) %>% 
  group_by(comb_id) %>%                           # Group by comb_id
  mutate(last_frame = ifelse(frameId == max(frameId), 1, 0)) %>%  # Create binary marker
  ungroup()   
write.csv(test_preds,"test_preds2.csv")
