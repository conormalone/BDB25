######################
#continued from initial_cleaning.R
########################
library(tidyverse)

#get targets and combine with route_rank_wide
just_targets <- player_play %>% filter(wasTargettedReceiver==1) %>% 
  select(c(gameId,playId,nflId))
targetted_rank <- just_targets %>% merge(.,route_counter, by= c('gameId', 'playId',"nflId"),all.x=TRUE) %>% 
  select(c('gameId', 'playId',"nflId","rank"))

         