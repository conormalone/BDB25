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
features <- dataframe %>%
  select(gameId, playId, frameId, down, yardsToGo, scorediff, score_bucket, pff_manZone, pff_runPassOption, defendersInBox, personnelD) %>%
  group_by(gameId, playId, frameId) %>%
  summarise(across(everything(), first), .groups = "drop")
#get ranked nflIds to put movement features in order
route_rank_wide <-route_counter %>%
  select(gameId, playId, nflId, rank) %>%
  pivot_wider(names_from = rank, values_from = nflId, names_prefix = "rank_") %>%
  arrange(gameId, playId) %>% select(-rank_6)

route_rank_wide$gameId <- as.factor(route_rank_wide$gameId)
route_rank_wide$playId <- as.factor(route_rank_wide$playId)
# Perform a left join to bring in y_diff values from the tracking_data based on matching gameId/playId and nflId
route_rank_wide %>% select(c(gameId, playId, rank_1)) %>% 
  left_join(shift_direction %>% select(gameId, playId, frameId, nflId, y_diff,s), by = c("gameId"="gameId","playId"="playId", "rank_1"="nflId")) %>% 
  filter(!is.na(y_diff))%>% rename(diff_1=y_diff,s_1=s)


route_rank_wide %>% select(c(gameId, playId, rank_1)) %>% 
  left_join(mim_direction %>% select(gameId, playId, frameId, nflId, y_diff,s), by = c("gameId"="gameId","playId"="playId", "rank_1"="nflId")) %>% 
  filter(!is.na(y_diff)) %>% rename(diff_1=y_diff,s_1=s)
move_direction <- rbind(mim_direction, shift_direction)
######
#get stats for wr1 to wr5
library(purrr)
r_1 <-route_rank_wide %>% select(c(gameId, playId, rank_1)) %>% 
  left_join(move_direction %>% select(gameId, playId, frameId, nflId, y_diff,s), by = c("gameId"="gameId","playId"="playId", "rank_1"="nflId")) %>% 
  filter(!is.na(y_diff)) %>% rename(diff_1=y_diff,s_1=s)
r_2 <-route_rank_wide %>% select(c(gameId, playId, rank_2)) %>% 
  left_join(move_direction %>% select(gameId, playId, frameId, nflId, y_diff,s), by = c("gameId"="gameId","playId"="playId", "rank_2"="nflId")) %>% 
  filter(!is.na(y_diff)) %>% rename(diff_2=y_diff,s_2=s)
r_3 <-route_rank_wide %>% select(c(gameId, playId, rank_3)) %>% 
  left_join(move_direction %>% select(gameId, playId, frameId, nflId, y_diff,s), by = c("gameId"="gameId","playId"="playId", "rank_3"="nflId")) %>% 
  filter(!is.na(y_diff)) %>% rename(diff_3=y_diff,s_3=s)
r_4 <-route_rank_wide %>% select(c(gameId, playId, rank_4)) %>% 
  left_join(move_direction %>% select(gameId, playId, frameId, nflId, y_diff,s), by = c("gameId"="gameId","playId"="playId", "rank_4"="nflId")) %>% 
  filter(!is.na(y_diff)) %>% rename(diff_4=y_diff,s_4=s)
r_5 <-route_rank_wide %>% select(c(gameId, playId, rank_5)) %>% 
  left_join(move_direction %>% select(gameId, playId, frameId, nflId, y_diff,s), by = c("gameId"="gameId","playId"="playId", "rank_5"="nflId")) %>% 
  filter(!is.na(y_diff)) %>% rename(diff_5=y_diff,s_5=s)

l <- list(r_1,r_2,r_3,r_4,r_5)
combined_movement_wide <- reduce(.x = l, merge, by = c('gameId', 'playId','frameId'), all = T)
combined_movement_wide[is.na(combined_movement_wide )] <- 0
combined_movement_wide <- combined_movement_wide %>% select(-c(rank_1,rank_2,rank_3,rank_4,rank_5))
#combing movement and other features
all_features <- features %>% merge(.,combined_movement_wide, by=c('gameId', 'playId','frameId'), all.x=TRUE)

#get targets and combine with route_rank_wide
just_targets <- player_play %>% filter(wasTargettedReceiver==1) %>% 
  select(c(gameId,playId,nflId))
just_targets %>% merge(.,route_counter, by= c('gameId', 'playId',"nflId"),all.x=TRUE) %>% 
  select(c('gameId', 'playId',"nflId","rank"))

         