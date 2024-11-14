library(tidyverse)
set.seed(1234)
plays <- read.csv("plays.csv")
play_clean <- plays %>% select(c(gameId,playId,possessionTeam, down,yardsToGo, preSnapHomeScore,preSnapVisitorScore,passResult,pff_runPassOption,pff_manZone))#,playAction,offenseFormation,receiverAlignment,pff_passCoverage,

player_play <- read.csv("player_play.csv")
player_play_clean <- player_play %>% select(c(gameId,playId,nflId, wasTargettedReceiver,#hadPassReception,
                                              inMotionAtBallSnap,shiftSinceLineset,motionSinceLineset,wasRunningRoute))


games <- read.csv("games.csv")
games$gameId <- as.factor(games$gameId)
##########################################################################
#feature1
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
########################################################################################
#preprocess tracking data
pre_processing_function <- function(df_input){
  dataframe <- df_input %>% 
    mutate(toLeft = playDirection == "left",
           x_std = ifelse(toLeft, 120-x, x) - 10, ## Standardizes X
           y_std = ifelse(toLeft, 160/3-y, y) ## Standardized Y
    )   
  
  
  return(dataframe)
}
#####################
combined_all_features <- data.frame()
processed_weeks <- list()

for (week_num in 1:9) {
  
  file_name <- paste0("tracking_week_", week_num, ".csv")
  
  if (file.exists(file_name)) {
    
    week_data <- read.csv(file_name)
    
    tracking_clean <- pre_processing_function(week_data)
    
    #processed_weeks[[week_num]] <- processed_week
    
    rm(week_data)  # Clear the week data from memory
    
    gc()           # Run garbage collection to free up memory
    
  }
  


tracking_football <-tracking_clean %>% filter(displayName=="football")
#merge tracking and pff to attach roles to tracking
play_track <- merge(tracking_clean, play_clean, how = "left",on = c(gameId, playId))
dataframe <- merge(play_track, player_play_clean, how = "left",on = c(gameId, playId, nflId))

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

#rm(list=c("play_track","play_clean","player_play_clean","tracking_clean"))

# Extract football location for each frame to use as reference
football_tracking <- tracking_football %>% 
  select(comb_id, frameId, x_std, y_std) %>% 
  rename(football_x = x_std, football_y = y_std)

# Calculate frame-by-frame relative position for man in motion
relative_position <- dataframe %>%
  left_join(football_tracking, by = c("comb_id", "frameId")) %>%
  mutate(
    y_diff = y_std - football_y
  ) %>%
  select(comb_id, gameId, playId,frameId, nflId, s, y_diff)


# Determine direction of travel for man in motion based on y_diff change over frames
direction <- relative_position %>%
  group_by(comb_id, nflId) %>%
  arrange(frameId) %>%
  mutate(y_shift = y_diff - lag(y_diff)) %>%
  mutate(motion_dir = ifelse(y_shift > 0, "Right", ifelse(y_shift < 0, "Left", NA))) %>%
  filter(!is.na(motion_dir)) %>%
  ungroup()

direction$gameId <- as.factor(direction$gameId)
direction$playId <- as.factor(direction$playId)

if (week_num %% 2 == 0) {
  # For even weeks, use the previous week's meninbox file name
  previous_week_num <- week_num - 1
  meninbox_files <- list.files(pattern = paste0("week", previous_week_num, "_", week_num, "_meninbox.*\\.csv"), full.names = TRUE)
  personnelD_files <- list.files(pattern = paste0("week", previous_week_num, "_", week_num, "_personnelD.*\\.csv"), full.names = TRUE)
  } else {
    next_week_num <- week_num + 1
  # For odd weeks, search for the meninbox file with the current week number
    meninbox_files <- list.files(pattern = paste0("week", week_num, "_", next_week_num, "_meninbox.*\\.csv"), full.names = TRUE)
    personnelD_files<- list.files(pattern = paste0("week", week_num, "_", next_week_num, "_personnelD.*\\.csv"), full.names = TRUE)
}
#meninbox_files <- list.files(pattern = paste0("week", week_num, ".*meninbox.*\\.csv"), full.names = TRUE)
#personnelD_files <- list.files(pattern = paste0("week", week_num, ".*personnelD.*\\.csv"), full.names = TRUE)
# Function to read and concatenate files
read_and_combine <- function(files) {
  do.call(rbind, lapply(files, read.csv))
}

# Read and combine the meninbox and personnelID files
meninbox_data <- read.csv("week9_meninbox_tracking.csv")#read_and_combine(meninbox_files)
personnelD_data <- read.csv("week9_personnelD_tracking.csv")#read_and_combine(personnelD_files)

meninbox_data <- meninbox_data %>% select(c(gameId,playId,frameId,y_pred)) %>% rename(defendersInBox = y_pred)
personnelD_data <- personnelD_data %>% select(c(gameId,playId,frameId,y_pred))%>% rename(personnelD = y_pred)
meninbox_data$comb_id <- as.factor(paste0(as.character(meninbox_data$gameId)," ", as.character(meninbox_data$playId)))
personnelD_data$comb_id <- as.factor(paste0(as.character(personnelD_data$gameId)," ", as.character(personnelD_data$playId)))
meninbox_data <- meninbox_data %>% select(-c(gameId,playId))
personnelD_data<- personnelD_data %>% select(-c(gameId,playId))
dataframe<- dataframe %>% merge(meninbox_data, by = c("comb_id","frameId")) #by = c("comb_id","frameId")
rm("meninbox_data")
dataframe<- dataframe %>% merge(personnelD_data, by = c("comb_id","frameId"))#by = c("comb_id","frameId")
rm("personnelD_data")
#######################
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
#route_rank_wide %>% select(c(gameId, playId, rank_1)) %>% 
#  left_join(shift_direction %>% select(gameId, playId, frameId, nflId, y_diff,s), by = c("gameId"="gameId","playId"="playId", "rank_1"="nflId")) %>% 
#  filter(!is.na(y_diff))%>% rename(diff_1=y_diff,s_1=s)


route_rank_wide %>% select(c(gameId, playId, rank_1)) %>% 
  left_join(direction %>% select(gameId, playId, frameId, nflId, y_diff,s), by = c("gameId"="gameId","playId"="playId", "rank_1"="nflId")) %>% 
  filter(!is.na(y_diff)) %>% rename(diff_1=y_diff,s_1=s)
move_direction <- direction#rbind(mim_direction, shift_direction)
######
#get stats for wr1 to wr5
library(purrr)
r_1 <-route_rank_wide %>% select(c(gameId, playId, rank_1)) %>% 
  left_join(move_direction %>% select(gameId, playId, frameId, nflId, y_diff,y_shift,s), by = c("gameId"="gameId","playId"="playId", "rank_1"="nflId")) %>% 
  filter(!is.na(y_diff)) %>% rename(diff_1=y_diff,lag_1 = y_shift,s_1=s)
r_2 <-route_rank_wide %>% select(c(gameId, playId, rank_2)) %>% 
  left_join(move_direction %>% select(gameId, playId, frameId, nflId, y_diff,y_shift,s), by = c("gameId"="gameId","playId"="playId", "rank_2"="nflId")) %>% 
  filter(!is.na(y_diff)) %>% rename(diff_2=y_diff,lag_2 = y_shift,s_2=s)
r_3 <-route_rank_wide %>% select(c(gameId, playId, rank_3)) %>% 
  left_join(move_direction %>% select(gameId, playId, frameId, nflId, y_diff,y_shift,s), by = c("gameId"="gameId","playId"="playId", "rank_3"="nflId")) %>% 
  filter(!is.na(y_diff)) %>% rename(diff_3=y_diff,lag_3 = y_shift,s_3=s)
r_4 <-route_rank_wide %>% select(c(gameId, playId, rank_4)) %>% 
  left_join(move_direction %>% select(gameId, playId, frameId, nflId, y_diff,y_shift,s), by = c("gameId"="gameId","playId"="playId", "rank_4"="nflId")) %>% 
  filter(!is.na(y_diff)) %>% rename(diff_4=y_diff,lag_4 = y_shift,s_4=s)
r_5 <-route_rank_wide %>% select(c(gameId, playId, rank_5)) %>% 
  left_join(move_direction %>% select(gameId, playId, frameId, nflId, y_diff,y_shift,s), by = c("gameId"="gameId","playId"="playId", "rank_5"="nflId")) %>% 
  filter(!is.na(y_diff)) %>% rename(diff_5=y_diff,lag_5 = y_shift,s_5=s)

l <- list(r_1,r_2,r_3,r_4,r_5)
combined_movement_wide <- reduce(.x = l, merge, by = c('gameId', 'playId','frameId'), all = T)
combined_movement_wide[is.na(combined_movement_wide )] <- 0
combined_movement_wide <- combined_movement_wide %>% select(-c(rank_1,rank_2,rank_3,rank_4,rank_5))
#combing movement and other features
all_features <- features %>% merge(.,combined_movement_wide, by=c('gameId', 'playId','frameId'), all.x=TRUE)
all_features[is.na(all_features)] <- 0

combined_all_features <-rbind(combined_all_features,all_features)
}
#################
#add some play state stuff

ggplot(direction, aes(x=y_diff)) + 
  
  geom_density()
