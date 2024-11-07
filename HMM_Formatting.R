######################
#continued from initial_cleaning.R
########################
library(tidyverse)
#what I need
#df with for every frame
#combid, frameid, 
#wr1s, wr1dist,
#wr2s, wr2dist,
#wr3s, wr3dist,
#wr4s, wr4dist,
#wr5s, wr5dist,
#down, distance, scorediff or buckets

#existing components:
#route_counter has wr ranks ####needs to be tied to target, 
#shift_Direction and mim_direction quantify movement
#dataframe has everything else (down, dist, score diff)

#get one row for each categorical feature
dataframe %>%
  select(gameId, playId, frameId, down, yardsToGo, scorediff, score_bucket, pff_manZone, pff_runPassOption, defendersInBox, personnelD) %>%
  group_by(gameId, playId, frameId) %>%
  summarise(across(everything(), first), .groups = "drop")
#get ranked nflIds to put movement features in order
route_counter %>%
  select(gameId, playId, nflId, rank) %>%
  pivot_wider(names_from = rank, values_from = nflId, names_prefix = "rank_") %>%
  arrange(gameId, playId) %>% select(-rank_6)
