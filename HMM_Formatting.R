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