library(tidyverse)
set.seed(1234)
plays <- read.csv("plays.csv")
player_play <- read.csv("player_play.csv")
games <- read.csv("games.csv")
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
tracking_football <-tracking_clean %>% filter(displayName=="football")
rm(tracking_dirty)
#merge tracking and pff to attach roles to tracking
play_clean <- plays %>% select(c(gameId,playId,possessionTeam, down,yardsToGo, preSnapHomeScore,preSnapVisitorScore,offenseFormation,receiverAlignment,passResult,playAction,pff_runPassOption,pff_passCoverage,pff_manZone))
play_track <- merge(tracking_clean, play_clean, how = "left",on = c(gameId, playId))
player_play_clean <- player_play %>% select(c(gameId,playId,nflId, hadPassReception,wasTargettedReceiver,
                                              inMotionAtBallSnap,shiftSinceLineset,motionSinceLineset,wasRunningRoute,
                                              routeRan,pff_defensiveCoverageAssignment,pff_primaryDefensiveCoverageMatchupNflId,pff_secondaryDefensiveCoverageMatchupNflId))
dataframe <- merge(play_track, player_play_clean, how = "left",on = c(gameId, playId, nflId))
games$gameId <- as.factor(games$gameId)
dataframe <- dataframe %>% merge(.,games, how = "left",on = c(gameId))
#I always make comb_id, giving each play a unique ID
dataframe$comb_id <- as.factor(paste0(as.character(dataframe$gameId)," ", as.character(dataframe$playId)))
tracking_football$comb_id <- as.factor(paste0(as.character(tracking_football$gameId)," ", as.character(tracking_football$playId)))
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
#movers_and_shakers <- dataframe %>% filter((inMotionAtBallSnap==1|shiftSinceLineset==1|motionSinceLineset==1))
#plays_with_motion <- dataframe %>% filter(inMotionAtBallSnap==1)%>% select(c(comb_id,nflId)) %>% unique()
#plays_with_shift <- dataframe %>% filter(shiftSinceLineset==1) %>% select(c(comb_id,nflId)) %>% unique()
#just_mim_tracking <- dataframe %>%  filter(comb_id %in% plays_with_motion$comb_id & nflId %in% plays_with_motion$nflId)
#just_shift_tracking <- dataframe %>%  filter(comb_id %in% plays_with_shift$comb_id & nflId %in% plays_with_shift$nflId )
  
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
      routes_run_to_date = cumsum(ifelse(wasRunningRoute == 1, 1, 0)) - ifelse(wasRunningRoute == 1, 1, 0),
      targets_to_date = cumsum(ifelse(wasTargettedReceiver == 1, 1, 0)) - ifelse(wasTargettedReceiver == 1, 1, 0)
    ) %>%
    ungroup() %>%
    
    # Group by each play and rank players by targets_to_date and nflId as secondary criterion
    group_by(week, playId, teamAbbr) %>%
    arrange(desc(targets_to_date), nflId) %>%
    mutate(rank = row_number()) %>%
    ungroup()
  
  return(data)
}

# Apply the function to your dataset
route_counter <- track_routes_and_targets(player_play_games)

# Select the desired columns
route_counter <- route_counter %>% 
  select(gameId, playId, nflId, teamAbbr, routes_run_to_date, targets_to_date, rank)
###########################################
#highlight checkdown plays
players <- read.csv("players.csv") %>% select(c(nflId,position))
rb_te_player_plays <- player_play %>% merge(.,players) %>% 
  filter((position=="TE" |position=="RB")&routeRan=="FLAT")
rb_te_player_plays 
#############
#continous shifter location
# Filter data for players in motion or shift
#movers_and_shakers <- dataframe %>%
#  filter(inMotionAtBallSnap == 1 | shiftSinceLineset == 1 | motionSinceLineset == 1)

# Get unique plays with motion and shift events
#plays_with_motion <- dataframe %>% filter(inMotionAtBallSnap == 1) %>% select(c(comb_id, nflId)) %>% unique()
#plays_with_shift <- dataframe %>% filter(shiftSinceLineset == 1) %>% select(c(comb_id, nflId)) %>% unique()

# Filter tracking data for only players involved in motion or shift
#just_mim_tracking <- dataframe %>% filter(comb_id %in% plays_with_motion$comb_id & nflId %in% plays_with_motion$nflId)
#just_shift_tracking <- dataframe %>% filter(comb_id %in% plays_with_shift$comb_id & nflId %in% plays_with_shift$nflId)

# Extract football location for each frame to use as reference
football_tracking <- tracking_football %>% 
  select(comb_id, frameId, x_std, y_std) %>% 
  rename(football_x = x_std, football_y = y_std)

# Calculate frame-by-frame relative position for man in motion
#mim_relative_position <- just_mim_tracking %>%
#  left_join(football_tracking, by = c("comb_id", "frameId")) %>%
#  mutate(
#    y_diff = y_std - football_y
#  ) %>%
#  select(comb_id, gameId, playId,frameId, nflId, s, y_diff)
# Calculate frame-by-frame relative position for man in motion
relative_position <- dataframe %>%
  left_join(football_tracking, by = c("comb_id", "frameId")) %>%
  mutate(
    y_diff = y_std - football_y
  ) %>%
  select(comb_id, gameId, playId,frameId, nflId, s, y_diff)
# Calculate frame-by-frame relative position for players in shift
#shift_relative_position <- just_shift_tracking %>%
#  left_join(football_tracking, by = c("comb_id", "frameId")) %>%
#  mutate(
 #   y_diff = y_std - football_y
 # ) %>%
  #select(comb_id, gameId, playId, frameId, nflId, s, y_diff)

# Determine direction of travel for man in motion based on y_diff change over frames
direction <- relative_position %>%
  group_by(comb_id, nflId) %>%
  arrange(frameId) %>%
  mutate(y_shift = y_diff - lag(y_diff)) %>%
  mutate(motion_dir = ifelse(y_shift > 0, "Right", ifelse(y_shift < 0, "Left", NA))) %>%
  filter(!is.na(motion_dir)) %>%
  ungroup()
#mim_direction <- mim_relative_position %>%
#  group_by(comb_id, nflId) %>%
#  arrange(frameId) %>%
#  mutate(y_shift = y_diff - lag(y_diff)) %>%
#  mutate(motion_dir = ifelse(y_shift > 0, "Right", ifelse(y_shift < 0, "Left", NA))) %>%
#  filter(!is.na(motion_dir)) %>%
#  ungroup()

# Determine direction of travel for players in shift based on y_diff change over frames
#shift_direction <- shift_relative_position %>%
#  group_by(comb_id, nflId) %>%
#  arrange(frameId) %>%
#  mutate(y_shift = y_diff - lag(y_diff)) %>%
#  mutate(motion_dir = ifelse(y_shift > 0, "Right", ifelse(y_shift < 0, "Left", NA))) %>%
#  filter(!is.na(motion_dir)) %>%
#  ungroup()
#pull components together
#shift_direction$gameId <- as.factor(shift_direction$gameId)
#shift_direction$playId <- as.factor(shift_direction$playId)
#mim_direction$gameId <- as.factor(mim_direction$gameId)
#mim_direction$playId <- as.factor(mim_direction$playId)
direction$gameId <- as.factor(direction$gameId)
direction$playId <- as.factor(direction$playId)

# List all meninbox and personnelID files in the directory
meninbox_files <- list.files( pattern = "meninbox", full.names = TRUE)
personnelD_files <- list.files( pattern = "personnelD", full.names = TRUE)

# Function to read and concatenate files
read_and_combine <- function(files) {
  do.call(rbind, lapply(files, read.csv))
}

# Read and combine the meninbox and personnelID files
meninbox_data <- read_and_combine(meninbox_files)
personnelD_data <- read_and_combine(personnelD_files)

meninbox_data <- meninbox_data %>% select(c(gameId,playId,frameId,y_pred)) %>% rename(defendersInBox = y_pred)
personnelD_data <- personnelD_data %>% select(c(gameId,playId,frameId,y_pred))%>% rename(personnelD = y_pred)
meninbox_data$comb_id <- as.factor(paste0(as.character(meninbox_data$gameId)," ", as.character(meninbox_data$playId)))
personnelD_data$comb_id <- as.factor(paste0(as.character(personnelD_data$gameId)," ", as.character(personnelD_data$playId)))
meninbox_data <- meninbox_data %>% select(-c(gameId,playId))
personnelD_data<- personnelD_data %>% select(-c(gameId,playId))
dataframe<- dataframe %>% merge(meninbox_data, by = c("comb_id","frameId")) 
dataframe<- dataframe %>% merge(personnelD_data, by = c("comb_id","frameId"))
rm(list=c("meninbox_data","personnelD_data"))
#################
#add some play state stuff
dataframe <- dataframe %>%
  mutate(
    scorediff = ifelse(club == homeTeamAbbr, preSnapHomeScore - preSnapVisitorScore, preSnapVisitorScore - preSnapHomeScore),
    score_bucket = case_when(
      scorediff < -14 ~ "Down more than 14",
      scorediff >= -14 & scorediff < -7 ~ "Down 7 to 14",
      scorediff >= -7 & scorediff < -3 ~ "Down 3 to 7",
      scorediff >= -3 & scorediff < 0 ~ "Down 1 to 3",
      scorediff == 0 ~ "Tie",
      scorediff > 0 & scorediff <= 3 ~ "Up 1 to 3",
      scorediff > 3 & scorediff <= 7 ~ "Up 3 to 7",
      scorediff > 7 & scorediff <= 14 ~ "Up 7 to 14",
      scorediff > 14 ~ "Up more than 14"
    )
  )

ggplot(direction, aes(x=y_diff)) + 
  geom_density()
