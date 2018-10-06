library(tidyverse)
#Script for simulating a draft and picking players according to Z scores ranking as determined in the NHL_analysis script.
#Also contains method for evaluating picks and comparing teams that were picked. 

#Specify which draft position to simulate and other draft features
draft_pos <- 6
num_teams <- 9
snake <- TRUE

#ranking_method <- c("n","n","n","n","n","n","n","n","z") #normal vs. z-score

#######################################################################################
#######################################################################################
# Download datasets from CBS fantasy in order to add the rank in the system of each player for mock-draft
###

skaters_rank <- read.csv("~/Downloads/skater_proj.csv", header=FALSE)
skaters_rank <- skaters_rank %>% select(c(2,15)) 
skaters_rank <- skaters_rank[3:nrow(skaters_rank),]
skaters_rank <- skaters_rank %>% separate(V2, c("Player", "Team"), sep = "\\|") %>% mutate(Player = trimws(Player)) %>% 
  separate(Player, c("Player", "Pos"), sep = "[:upper:]+$") %>% mutate(Player = trimws(Player)) %>%
  select(Player, V15)

goalies_rank.df <- read.csv("~/Downloads/goalie_proj.csv", header=FALSE)
goalies_rank.df <- goalies_rank.df %>% select(c(2,15)) 
goalies_rank.df <- goalies_rank.df[3:nrow(goalies_rank.df),]
goalies_rank.df <- goalies_rank.df %>% separate(V2, c("Player", "Team"), sep = "\\|") %>% mutate(Player = trimws(Player)) %>% 
  separate(Player, c("Player", "Pos"), sep = "[:upper:]+$") %>% mutate(Player = trimws(Player)) %>% 
  select(Player, V15)

cbs_rank <- full_join(skaters_rank, goalies_rank.df) %>% mutate(Rank = as.integer(V15)) %>%  arrange(Rank)


#######################################################################################
#######################################################################################
##Prep for later Get position of each player
player_with_pos <- cbs_data.df %>% select(Player, Pos)
goalie_with_pos <- cbs_data_G.df %>% select(Player, Pos)
all_with_pos <- rbind(player_with_pos, goalie_with_pos)

# Create list of players to pick from 
player_list <- left_join(all_joined, cbs_rank)
p_w_salary <- left_join(player_list, aav, by = "Player")
p_w_salary <- p_w_salary %>% mutate(Zsal = Zavg / CapHit * 1000000)
player_list <- p_w_salary

#Create output dataframe
df_out <- data.frame()

#Function for picking players
pick_player <- function(draft_turn, draft_pos, player_list, team){
  #Check if it is your turn to draft
  if(draft_turn == draft_pos){
    print("equal to draft_pos")
    #Check what the average salary of the picked players is. Salary cap ~80 million for 20 players. If avg >5 million, then sort
    #list according to those players providing best value.
    # If average team salary evaluates as NA, then rank by Zavg
    if(is.na(mean(as.numeric(unlist(df_out[df_out[,59] == team,][57])), na.rm = T) > 5000000)){
      player_list <- player_list[order(player_list$Zavg, decreasing=T),] 
    } else {
      # If avg >5 mil and more than 8 players already picked, then rank according to value picks
      if(mean(as.numeric(unlist(df_out[df_out[,59] == team,][57])), na.rm = T) > 5000000 && nrow(df_out[df_out[,59] == team,]) > 8){
        print("salary trouble")
        player_list <- player_list[order(player_list$Zsal, decreasing=T),]
      } else {
        player_list <- player_list[order(player_list$Zavg, decreasing=T),]
      }
    }
  } else {
    # if not the team using my method, pick according to the CBS rank
    print("not equal to draft_pos")
    player_list <- player_list[order(player_list$Rank, decreasing=F),]    
  }
  selected_player <- c(player_list[1,],team)
  player_list <- player_list[-1,]
  #Return the selected player and the player list without that player. This list will be used to draft from again
  return(list(selected_player, player_list))
}

#Draft - first range specifies how many draft rounds
for(i in 1:32){
  print(i)
  if(snake == TRUE){
    if(i %% 2 == 0){
      draft_order <- num_teams:1
    } else {
      draft_order <- 1:num_teams
    }
  } else {
    draft_order <- 1:num_teams
  }
  for(j in draft_order){
    player_and_list <- pick_player(j, draft_pos,player_list,j)
    print("Player")
    print(player_and_list[[1]][[1]])
    player <- data.frame(player_and_list[[1]])
    names(player) <- names(df_out) 
    df_out <- rbind(df_out, player)
    
    player_list <- player_and_list[[2]]
  }
}

#Set column names
names(df_out) <- c(names(player_list), "Team")

# Join with player position 
df_out <- left_join(df_out, all_with_pos, by ="Player")

#Calculate summary stats for each category
skate_agg <- df_out %>% mutate(PIMPred = as.numeric(PIM.y)) %>% group_by(Team) %>% 
  summarise_at(c("PlusMinus.x", "APred", "BLK", "GPred", "HIT", 
                 "STA", "STG", "TOI", "PIMPred", "WSO"), 
               funs(sum(., na.rm = TRUE)))

goalie_gg <- df_out %>% mutate(PIMPred = as.numeric(PIM.y)) %>% group_by(Team) %>% 
  summarise_at(c("GAA", "SPCT"), 
               funs(mean(., na.rm = TRUE))) %>% 
  mutate(GAA = GAA * -1)

team_agg <- cbind(skate_agg, goalie_gg[,-1])

#average rank (higher = better)
team_agg %>%  mutate_all(dense_rank) %>% 
  rowwise() %>% 
  mutate(mean(c(PlusMinus.x, APred, BLK, GPred, HIT, STA, STG, TOI, PIMPred, GAA, SPCT, WSO)))

## Inspect team
## Add salaries
#with_salary <- left_join(df_out, m, by = "Player")

all_teams <- c()
for(i in 1:9){
  all_teams[[i]] <- df_out %>% filter(Team == i) %>% select(Player, Rank, PlusMinus.x, GPred, APred, BLK, HIT, STA,
                                        STG, TOI, PIM.y, GAA, SPCT, WSO, Team, CapHit, Pos.y)
  
}

for(i in 1:9){
  print(i)
  print(all_teams[[i]] %>% group_by(Pos.y) %>% summarise(count = n()))
}

for(i in 1:9){
  print(i)
  print(sum(all_teams[[i]]$CapHit[1:20], na.rm = T))
}
