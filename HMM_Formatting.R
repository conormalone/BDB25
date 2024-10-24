######################
#continued from initial_cleaning.R
########################
#transition matrix
#add ranks for every play
#see how often receivers one to 6 targetted
targets_w_rank <- data.frame()
df_targetted$comb_id<-droplevels(df_targetted$comb_id)
for (i in 1:length(levels(df_targetted$comb_id))){
subset <- df_targetted %>% filter(comb_id==levels(df_targetted$comb_id)[i])
  play_results <-rank_wr_players(df_targetted, subset$nflId, posTeam=subset$posTeam[1],week = subset$week[1], gameId = subset$gameId[1], playId = subset$playId[1])
  targets_w_rank <-rbind(targets_w_rank, play_results)
}