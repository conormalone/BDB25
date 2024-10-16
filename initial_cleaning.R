library(tidyverse)
set.seed(1234)
plays <- read.csv("plays.csv")
player_play <- read.csv("player_play.csv")
week1 <- read.csv("tracking_week_1.csv")
week2 <- read.csv("tracking_week_2.csv")
week3 <- read.csv("tracking_week_3.csv")
week4 <- read.csv("tracking_week_4.csv")
week5 <- read.csv("tracking_week_5.csv")
week6 <- read.csv("tracking_week_6.csv")
week7 <- read.csv("tracking_week_7.csv")
week8 <- read.csv("tracking_week_8.csv")
week9 <- read.csv("tracking_week_9.csv")
tracking_dirty <- rbind(week1,week2,week3,week4,week5,week6,week7,week8,week9)
rm(week1,week2,week3,week4,week5,week6,week7,week8,week9)
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
play_clean <- plays %>% select(c(gameId,playId,quarter, down,yardsToGo))
play_track <- merge(tracking_clean, play_clean, how = "left",on = c(gameId, playId))
tracking_pff <- merge(play_track, pff, how = "left",on = c(gameId, playId, nflId))
tracking_pff <- tracking_pff %>% 
  filter(pff_role == "Pass Rush" | pff_role == "Pass Block" | pff_role == "Pass")
tracking_pff$event <- as.factor(tracking_pff$event)
#I always make comb_id, giving each play a unique ID
tracking_pff$comb_id <- paste0(as.character(tracking_pff$gameId)," ", as.character(tracking_pff$playId))
tracking_pff$comb_id <- as.factor(tracking_pff$comb_id)
tracking_pff$gameId <- as.factor(tracking_pff$gameId)
tracking_pff$playId <- as.factor(tracking_pff$playId)