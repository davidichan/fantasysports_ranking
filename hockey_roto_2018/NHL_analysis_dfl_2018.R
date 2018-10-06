library(RCurl)
library(XML)
library(ggplot2)
library(tidyverse)

#Script that will grab historical and predicted data, combine the datasets and combine value over replacement
#along with Z scores of values over replacement. The Z score average can then be used to have one metric for 
#evaluating all players, including goalies.

#######################################################################################
#######################################################################################
# Grab salary information for all players
cf_url <- "https://www.capfriendly.com/browse/active/2019/caphit/all/all/all/desc/"
cf_tables <- readHTMLTable(getURL(cf_url))
pages <- c(1:5)
categories <- c("Player", "Team", "Term", "Status", "Age", "Pos_real", "Shoots", "Salary", "CapHit")

#Function that downloads NHL player salary data for the given year and for as many pages as indicated. 
salary_data <- function(pages, year){
  pred_URL_stem <- paste0("https://www.capfriendly.com/browse/active/",year,"/caphit/all/all/all/desc/","")
  pred_table <- readHTMLTable(getURL(paste0(pred_URL_stem,"1")), header=FALSE)
  cols <- ncol(pred_table[[1]]) + 1
  pred_data <- data.frame(matrix(NA, nrow = 0, ncol = cols))
  
  for(i in pages){
    pred_URL <- paste0(pred_URL_stem,i)
    urldata <- getURL(pred_URL)
    pred_table <- readHTMLTable(urldata, header=FALSE)
    pred_data <- rbind(pred_data, pred_table[[1]])
  }
  colnames(pred_data) <- categories
  return(pred_data)
}

#NB: this works before the season, after the season has started, additional stats columns are added
t <- salary_data(pages,2020)

#Extract aav, player names, and position from salary list
aav <- t %>% separate(Player, c("Num", "Player"), sep = "[:digit:]\\.\\s") %>% 
  separate(CapHit, c("df", "CapHit"), sep = "\\$") %>% 
  mutate(CapHit = as.numeric(gsub("\\,", "", CapHit)))  %>% select(Player, CapHit, Pos_real)  


#######################################################################################
#######################################################################################
## Hockey Reference historical data for certain scoring categories
########################
# Grab last season's stats
url <- "https://www.hockey-reference.com/leagues/NHL_2018_skaters.html"
tables <- readHTMLTable(getURL(url), header=FALSE)
data.df <- tables[[1]]

#Grab column names; row 27 in this case contained most of the column names needed
cn <- data.df[27,]
cn <- lapply(cn, as.character)
colnames(data.df) <- cn
colnames(data.df)[c(10,17:19)] <- c("PlusMinus","EV_A","PP_A","SH_A")

#Clean up and remove rows not containing real data
data.df <- data.df[data.df$Rk!='Rk',]
data.df <- droplevels(data.df)

#Only keep one row per player
data.df <- data.df %>% distinct(Player, .keep_all = TRUE)

## change to numeric, but since factor, need to change to a value first
index <- c(1,3,6:(ncol(data.df)-1))
for(i in index) {
  data.df[,i] <- as.numeric(as.character(data.df[,i]))
}

## trim out non-scoring categories
#[1] "Rk"     "Player" "Age"    "Tm"     "Pos"    "GP"     "G"      "A"      "PTS"    "PlusMinus"    "PIM"    "PS"     "EV"    
#[14] "PP"     "SH"     "GW"     "EV_A"   "PP_A"   "SH_A"   "S"      "S%"     "TOI"    "ATOI"   "BLK"    "HIT"    "FOW"   
#[27] "FOL"    "FO%"  
scoring_only.df <- data.df %>% select(Player, GP, G, A, PlusMinus, PIM, BLK, HIT, PP, PP_A, SH, SH_A, TOI) %>% 
  mutate(STA = PP_A + SH_A, STG = PP + SH) %>% 
  select(-PP_A, -SH_A, -PP, -SH)

#######################################################################################
#######################################################################################
## CBS Predicted data 
########################
# Grab predicted stats for this season
#pred_URL <- "https://www.cbssports.com/fantasy/hockey/stats/sortable/points/C/advanced/projections/season?&print_rows=9999"
pred_URL <- "https://www.cbssports.com/fantasy/hockey/stats/sortable/points/C/advanced/projections/2018/ytd&print_rows=9999"
pred_table <- readHTMLTable(getURL(pred_URL), header=F)

#Specify postions to download and column names for labelling of downloaded data
positions <- c("C", "RW","LW", "D")
stat_type <- "advanced"
categories <- c("Player", "GP", "GPred", "APred", "PTS", "PlusMinus", "S", "SPCT", "GWG", "SOG", "PIM", "MAJ", "MNR", "FPTS", "Pos")

# Function to download predicted data
cbs_data <- function(pos, sport, stat_type){
  pred_URL_stem <- paste0("https://www.cbssports.com/fantasy/",sport,"/stats/sortable/points/","")
  pred_URL_tail <- "/projections/2018/ytd?&print_rows=9999"
  pred_URL <- paste0(pred_URL_stem,pos[1],"/",stat_type,pred_URL_tail,"")
  pred_table <- readHTMLTable(getURL(pred_URL), header=FALSE)
  cols <- ncol(pred_table[[1]]) + 1
  pred_data <- data.frame(matrix(NA, nrow = 0, ncol = cols))
  
  for(i in pos){
    pred_URL <- paste0(pred_URL_stem,i,"/",stat_type,pred_URL_tail,"")
    urldata <- getURL(pred_URL)
    pred_table <- readHTMLTable(urldata, header=FALSE)
    pred_table[[1]]$position <- i
    pred_data <- rbind(pred_data, pred_table[[1]])
  }
  colnames(pred_data) <- categories
  pred_data <- pred_data[!is.na(pred_data$GP),]
  pred_data <-  pred_data[pred_data$Player!='Player',]
  return(pred_data)
}


cbs_data.df <- cbs_data(positions, "hockey","advanced")
cbs_data.df <- cbs_data.df %>% separate(Player, c("Player", "Team"), sep = ",")

## Remove non-scoring columns
cbs_scoring_only.df <- cbs_data.df %>% select(Player, GPred, APred, PlusMinus, PIM)

## change to numeric, but since factor, need to change to a value first
index <- c(2:5)
for(i in index) {
  cbs_scoring_only.df[,i] <- as.numeric(as.character(cbs_scoring_only.df[,i]))
}


#######################################################################################
#######################################################################################
#Calculate value over replacement and Z-scores

#Combine the historical and predicted datasets
##
data_joined <- left_join(scoring_only.df, cbs_scoring_only.df, by = "Player")

###
# Calculate vors for both datasets combined
###

#Function for calculating position independent vor. Could also use based on position or both, depending on how many of each 
#position are available and needed.
vor_pos_indep <- function(repl_rank, df, score_cat){
  #order df based on scoring category
  df_order <- df[order(df[,score_cat], decreasing = T),]
  #which column is the scored cat?
  fpoints.ndx <- match(score_cat, colnames(df_order))
  #Find the mean vor based on the replacement rank
  vor_mean <- mean(df_order[c(repl_rank-1, repl_rank, repl_rank + 1),fpoints.ndx])
  print(vor_mean)
  #Add a vor column with the score - mean
  new_name <- paste0("vor",score_cat)
  #df_order[,new_name][df_order$Pos == pos] <- sapply(df_order[df_order$Pos == pos, fpoints.ndx], function(x) x - vor_mean)
  if (new_name %in% names(df_order)) {
    df_order[,new_name] <- sapply(df_order[, fpoints.ndx], function(x) round(x - vor_mean, 3))
  } else {
    df_order$vor <- sapply(df_order[, fpoints.ndx], function(x) round(x - vor_mean, 3))  
    names(df_order)[ncol(df_order)] <- paste0("vor",score_cat)
  }
  return(df_order)
}


categories <- colnames(data_joined)
score_cols <- c(3:15)

#Copy original dataset for comparison
data.working <- data_joined


## Calculate vors
for (i in score_cols) {
  print(categories[i])
  data.working <- vor_pos_indep(288, data.working, categories[i])
}

## Calculate zscores
for (i in score_cols){
  score_cat <- categories[i]
  new_name <- paste0("vor",score_cat)
  z_name <- paste0("Z", new_name)
  data.working[,z_name] <- scale(data.working[,new_name], center = T, scale = T)
}

## Average Z-scores
data.working$Zavg_skater <- apply(data.working[,c(31,39,33,38,34,32,36,37,35)], 1, mean)
#data.working %>% arrange(desc(Zavg_skater))
#data.working[order(data.working$Zavg_skater, decreasing=T),][1:20,c(1,42)]

skater_data <- data.working

#######################################################################################
#######################################################################################
# Download and label goalie data for HR and CBS
url_G <- "https://www.hockey-reference.com/leagues/NHL_2018_goalies.html"
tables_G <- readHTMLTable(getURL(url_G), header=FALSE)

HR_data_G.df <- tables_G[[1]]

cn <- HR_data_G.df[44,]
cn <- lapply(cn, as.character)
colnames(HR_data_G.df) <- cn
colnames(HR_data_G.df)[c(13)] <- c("SPCT")

HR_data_G.df <- HR_data_G.df[HR_data_G.df$Rk!='Rk',]
HR_data_G.df <- droplevels(HR_data_G.df)

HR_data_G.df <- HR_data_G.df %>% distinct(Player, .keep_all = TRUE)
## change to numeric, but since factor, need to change to a value first
index <- c(1,3,5:(ncol(HR_data_G.df)-1))

for(i in index) {
  HR_data_G.df[,i] <- as.numeric(as.character(HR_data_G.df[,i]))
}

## trim out non-scoring categories
#[1] "Rk"     "Player" "Age"    "Tm"     "GP"     "GS"     "W"      "L"      "T/O"    "GA"     "SA"     "SV"    
#[13] "SV%"    "GAA"    "SO"     "GPS"    "MIN"    "QS"     "QS%"    "RBS"    "GA%-"   "GSAA"   "G"      "A"     
#[25] "PTS"    "PIM" 
HR_G_scoring_only.df <- HR_data_G.df %>% select(Player, GP, GAA, SPCT, W, SO) %>% 
  mutate(WSO = W + SO) %>% arrange(desc(WSO))

#Predicted data from CBS
categories <- c("Player", "GP", "GS", "W", "L", "SO", "GAA", "S", "SOGA", "SPCT", "PIM", "A", "MIN", "FPTS", "Pos")
cbs_data_G.df <- cbs_data("G", "hockey","advanced")

cbs_data_G.df <- cbs_data_G.df %>% separate(Player, c("Player", "Team"), sep = ",")

## Remove non-scoring columns
cbs_data_G.df <- cbs_data_G.df %>% mutate_at(3:15, as.character) %>% mutate_at(3:15, as.numeric)
cbs_G_scoring_only.df <- cbs_data_G.df %>% mutate(WSO = W + SO) %>% select(Player, GAA, SPCT, WSO, Pos)

########################
## Calculate vors
#NB: only cbs data was used
categories <- colnames(cbs_G_scoring_only.df)
data.working <- cbs_G_scoring_only.df
for (i in 2:4) {
  print(categories[i])
  data.working <- vor_pos_indep(36, data.working, categories[i])
}

## Calculate zscores
for (i in 2:4){
  score_cat <- categories[i]
  new_name <- paste0("vor",score_cat)
  z_name <- paste0("Z", new_name)
  data.working[,z_name] <- scale(data.working[,new_name], center = T, scale = T)
}
data.working$ZvorGAA <- data.working$ZvorGAA * -1

## Average Z-scores
data.working$Zavg_G <- apply(data.working[,c(9:11)], 1, mean)
#data.working %>% arrange(desc(Zavg_G))
data.working[order(data.working$Zavg_G, decreasing=T),]#[1:20,c(1,42)]

goalie_data <- data.working

#######################################################################################
#######################################################################################
## Prep and merge skater and goalie datasets
cols <- c(9:11)
goalie_data[,cols] = apply(goalie_data[,cols], 2, function(x) as.numeric(as.character(x)))
goalie_data <- goalie_data %>% mutate(Zavg = Zavg_G)

cols <- c(29:41)
skater_data[,cols] = apply(skater_data[,cols], 2, function(x) as.numeric(as.character(x)))
skater_data <- skater_data %>% mutate(Zavg = Zavg_skater)

all_joined <- full_join(skater_data, goalie_data)
