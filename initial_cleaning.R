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
dataframe$comb_id <- paste0(as.character(dataframe$gameId)," ", as.character(dataframe$playId))
dataframe$comb_id <- as.factor(dataframe$comb_id)
dataframe$gameId <- as.factor(dataframe$gameId)
dataframe$playId <- as.factor(dataframe$playId)
dataframe$event <- as.factor(dataframe$event)
rm(list=c("play_track","play_clean","player_play_clean","tracking_clean","plays","player_play"))
movers_and_shakers <- dataframe %>% filter((inMotionAtBallSnap==1|shiftSinceLineset==1|motionSinceLineset==1))
plays_with_motion <- dataframe %>% filter(inMotionAtBallSnap==1)%>% select(c(comb_id,nflId)) %>% unique()
plays_with_shift <- dataframe %>% filter(shiftSinceLineset==1) %>% select(c(comb_id,nflId)) %>% unique()
just_mim_tracking <- dataframe %>%  filter(comb_id %in% plays_with_motion$comb_id & nflId %in% plays_with_motion$nflId)
just_shift_tracking <- dataframe %>%  filter(comb_id %in% plays_with_shift$comb_id & nflId %in% plays_with_shift$nflId )

start_mim <- just_mim_tracking %>% filter(event=="man_in_motion")%>% select(c(comb_id, frameId,nflId,y_std))
end_mim <- just_mim_tracking %>% filter(event=="ball_snap")%>% select(c(comb_id, frameId,nflId,y_std))
