library(tidyverse)
set.seed(1234)
plays <- read.csv("plays.csv")
player_play <- read.csv("player_play.csv")
week1 <- read.csv("tracking_week_1.csv")
#week2 <- read.csv("tracking_week_2.csv")
#week3 <- read.csv("tracking_week_3.csv")
#week4 <- read.csv("tracking_week_4.csv")
#week5 <- read.csv("tracking_week_5.csv")
#week6 <- read.csv("tracking_week_6.csv")
#week7 <- read.csv("tracking_week_7.csv")
#week8 <- read.csv("tracking_week_8.csv")
#week9 <- read.csv("tracking_week_9.csv")
tracking_dirty <- rbind(week1)#,week2,week3,week4,week5,week6,week7,week8,week9)
rm(week1)#,week2,week3,week4,week5,week6,week7,week8,week9)
#####################
#preprocess tracking data
pre_processing_function <- function(df_input){
  dataframe <- df_input %>% 
    mutate(toLeft = playDirection == "left",
           x_std = ifelse(toLeft, 120-x, x) - 10, ## Standardizes X
           y_std = ifelse(toLeft, 160/3-y, y) ## Standardized Y
    )   
  
  
  return(dataframe)
}
tracking_clean <-pre_processing_function(tracking_dirty)
rm(tracking_dirty)
#merge tracking and pff to attach roles to tracking
play_clean <- plays %>% select(c(gameId,playId,possessionTeam, offenseFormation,receiverAlignment,passResult,playAction,pff_runPassOption,pff_passCoverage,pff_manZone))
play_track <- merge(tracking_clean, play_clean, how = "left",on = c(gameId, playId))
player_play_clean <- player_play %>% select(c(gameId,playId,nflId, hadPassReception,wasTargettedReceiver,
                                              inMotionAtBallSnap,shiftSinceLineset,motionSinceLineset,wasRunningRoute,
                                              routeRan,pff_defensiveCoverageAssignment,pff_primaryDefensiveCoverageMatchupNflId,pff_secondaryDefensiveCoverageMatchupNflId))
dataframe <- merge(play_track, player_play_clean, how = "left",on = c(gameId, playId, nflId))

#I always make comb_id, giving each play a unique ID
dataframe$comb_id <- as.factor(paste0(as.character(dataframe$gameId)," ", as.character(dataframe$playId)))
plays$comb_id <- as.factor(paste0(as.character(plays$gameId)," ", as.character(plays$playId)))
player_play$comb_id <- as.factor(paste0(as.character(player_play$gameId)," ", as.character(player_play$playId)))
dataframe$gameId <- as.factor(dataframe$gameId)
dataframe$playId <- as.factor(dataframe$playId)
dataframe$event <- as.factor(dataframe$event)
dataframe$passResult <- as.factor(dataframe$passResult)
#just pass or run pass option plays, no sacks and not after the snap
dataframe <- dataframe %>% filter((pff_runPassOption==1) | (passResult !="")| (passResult !="S")| (frameType !="AFTER_SNAP"))

rm(list=c("play_track","play_clean","player_play_clean","tracking_clean"))
#end preprocess
###########################################
#feature 1
#direction of shift & man in motion
movers_and_shakers <- dataframe %>% filter((inMotionAtBallSnap==1|shiftSinceLineset==1|motionSinceLineset==1))
plays_with_motion <- dataframe %>% filter(inMotionAtBallSnap==1)%>% select(c(comb_id,nflId)) %>% unique()
plays_with_shift <- dataframe %>% filter(shiftSinceLineset==1) %>% select(c(comb_id,nflId)) %>% unique()
just_mim_tracking <- dataframe %>%  filter(comb_id %in% plays_with_motion$comb_id & nflId %in% plays_with_motion$nflId)
just_shift_tracking <- dataframe %>%  filter(comb_id %in% plays_with_shift$comb_id & nflId %in% plays_with_shift$nflId )
#get direction of travel of man in motion
start_mim <- just_mim_tracking %>% filter(event=="man_in_motion")%>% select(c(comb_id, frameId,nflId,y_std)) %>% rename( start_y =y_std)
end_mim <- just_mim_tracking %>% filter(event=="ball_snap")%>% select(c(comb_id, frameId,nflId,y_std)) %>% rename( end_y =y_std)
#merge start and end mim, get diff, if larger they move right, if smaller left
mim_dir <- start_mim %>% merge(., end_mim, by=c("comb_id","nflId")) %>% mutate(y_diff = start_y - end_y) %>% 
  select(-c(frameId.x,frameId.y,start_y,end_y)) %>% filter(abs(y_diff) >0.25) %>% 
  mutate(motion_dir = ifelse(y_diff>0,"Right","Left")) %>% select(-y_diff)
#get direction of travel of shift
start_shift <- just_shift_tracking %>% filter(event=="shift")%>% select(c(comb_id, frameId,nflId,y_std)) %>% rename( start_y =y_std)
end_shift <- just_shift_tracking %>% filter(event=="ball_snap")%>% select(c(comb_id, frameId,nflId,y_std)) %>% rename( end_y =y_std)
#merge start and end shift, get diff, if larger they move right, if smaller left
shift_dir <- start_shift %>% merge(., end_shift, by=c("comb_id","nflId")) %>% mutate(y_diff = start_y - end_y) %>% 
  select(-c(frameId.x,frameId.y,start_y,end_y)) %>% filter(abs(y_diff) >0.1) %>% 
  mutate(motion_dir = ifelse(y_diff>0,"Right","Left")) %>% select(-y_diff)   
######################################
#feature2
#target metrics to rank players by most popular (HMM starts with most pop)
#import games to get week counts
player_play_games <- player_play %>% merge(.,games, by="gameId") %>% filter(wasRunningRoute==1 | wasTargettedReceiver==1)
player_play_games$teamAbbr <-as.factor(player_play_games$teamAbbr)

track_routes_and_targets <- function(data) {
  
  # Arrange data by week, gameId, and playId to ensure chronological order
  data <- data %>% arrange(week, playId)
  
  # Compute cumulative counts for routes run and targets for each play, up to that play
  data <- data %>%
    group_by(teamAbbr, nflId) %>%
    mutate(
      routes_run_to_date = cumsum(ifelse(wasRunningRoute== 1, 1, 0)) - ifelse(wasRunningRoute== 1, 1, 0),
      targets_to_date = cumsum(ifelse(wasTargettedReceiver == 1, 1, 0)) - ifelse(wasTargettedReceiver == 1, 1, 0)
    ) %>%
    ungroup()%>%
    
    # Group by each play and rank players by targets_to_date
    group_by(week, playId, teamAbbr) %>%
    mutate(rank= dense_rank(desc(targets_to_date))) %>%
    ungroup()
  
  return(data)
}


route_counter <- track_routes_and_targets(player_play_games)
route_counter <- route_counter%>% select(c(gameId,playId,nflId,teamAbbr,routes_run_to_date,targets_to_date,rank))


#highlight checkdown plays
players <- read.csv("players.csv") %>% select(c(nflId,position))
rb_te_player_plays <- player_play %>% merge(.,players) %>% 
  filter((position=="TE" |position=="RB")&routeRan=="FLAT")
rb_te_player_plays 
#next step
#pull components together
