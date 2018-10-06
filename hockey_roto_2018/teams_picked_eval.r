#Following draft, this integrates the actual teams that were picked with the player list created in fantasy_nhl_evaluation_18.r
library(tidyverse)

#Pull in rosters as downloaded from CBS after the draft
teams_as_picked <- read.csv("~/Downloads/teams_picked.csv", header=FALSE)

teams_as_picked <- teams_as_picked[4:nrow(teams_as_picked),]
teams_as_picked <- teams_as_picked %>% separate(V2, c("Player", "Team"), sep = "\\|") %>% mutate(Player = trimws(Player)) %>% 
  separate(Player, c("Player", "Pos"), sep = "[:upper:]+$") %>% mutate(Player = trimws(Player)) %>%
  select(V1, Player)

#Combine with player list
teams_picked_full <- left_join(teams_as_picked, player_list, by = "Player")




