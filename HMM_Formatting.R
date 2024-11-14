######################
#continued from initial_cleaning.R
########################
library(tidyverse)

#get targets and combine with route_rank_wide
player_play$comb_id <- as.factor(paste0(as.character(player_play$gameId)," ", as.character(player_play$playId)))
just_targets <- player_play %>% filter(wasTargettedReceiver==1) %>% 
  filter(comb_id %in% short_plays$comb_id) %>% select(c(gameId,playId,nflId))

targetted_rank <- just_targets %>% merge(.,route_counter, by= c('gameId', 'playId',"nflId"),all.x=TRUE) %>% 
  select(c('gameId', 'playId',"nflId","rank"))

prop.table(table(targetted_rank$rank,targetted_rank$rank) )       

combined_all_features <- combined_all_features %>% merge(.,targetted_rank,by=c("gameId","playId"))
