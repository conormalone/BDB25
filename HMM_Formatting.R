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
combined_all_features$comb_id <- as.factor(paste0(as.character(combined_all_features$gameId)," ", as.character(combined_all_features$playId)))
combined_all_features <- combined_all_features %>% filter(comb_id %in% short_plays$comb_id)
combined_all_features[factor_columns] <- lapply(combined_all_features[factor_columns], as.factor)
train_features <- combined_all_features %>% filter(week>1 & week<9)
test_features <- combined_all_features %>% filter(week==9)
categorize_speed <- function(s) {
  case_when(
    s == 0 ~ "0",
    s < 1 ~ "1",
    s < 3 ~ "2-3",
    s < 5 ~ "3-5",
    s < 7 ~ "5-7",
    s < 8 ~ "8",
    s < 9 ~ "9",
    s >= 9 ~ "Over 9"
  )
}
categorize_lag <- function(s) {
  case_when(
    s <=-0.75 ~ "less than -0.75",
    s < -0.5 ~ "-1 to -0.5",
    s < 0 ~ "-0.5 TO 0",
    s ==0  ~ "0",
    s < 0.5 ~ "0 to 0.5",
    s < 1 ~ "0.5 to 1",
    s >= 0.75 ~ "Over 0.75"
  )
}

combined_all_features <- combined_all_features %>%
  mutate(across(starts_with("s_"), categorize_speed, .names = "speed_{.col}")) %>% 
  mutate(      across(starts_with("lag_"), categorize_lag, .names = "bucket_{.col}"))


bucket_counts <- combined_all_features %>%
  pivot_longer(cols = bucket_lag_1, names_to = "source", values_to = "bucket") %>%
  count(bucket)

# Plot the histogram
ggplot(bucket_counts, aes(x = bucket, y = n)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Histogram of Buckets",
    x = "Bucket",
    y = "Count"
  ) +
  theme_minimal()


