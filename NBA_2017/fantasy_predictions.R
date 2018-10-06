##############################################################################
#Adapted from a tutorial of @BrockTibert 

library(RCurl)
library(XML)
library(ggplot2)  ## loads all sort of great packages from @HadleyWickham

positions <- c("C", "G","F")
stat_type <- "standard"
sport <- "basketball"
categories <- c("Player", "G", "MPG", "FG", "FGP", "FT", "FTP", "3PT", "3PTP", "RPG", "APG", "SPG", "TPG", "BPG", "PPG", "FPTS", "Pos")
cbs_data <- function(pos, sport, stat_type){
  pred_URL_stem <- paste0("http://www.cbssports.com/fantasy/",sport,"/stats/sortable/points/","")
  pred_URL_tail <- "/projections/season?&print_rows=9999"
  pred_URL <- paste0(pred_URL_stem,pos[1],"/",stat_type,pred_URL_tail,"")
  pred_table <- readHTMLTable(pred_URL, header=FALSE)
  cols <- ncol(pred_table[[1]]) + 1
  pred_data <- data.frame(matrix(NA, nrow = 0, ncol = cols))
  
  for(i in pos){
    pred_URL <- paste0(pred_URL_stem,i,"/",stat_type,pred_URL_tail,"")
    pred_table <- readHTMLTable(pred_URL, header=FALSE)
    pred_table[[1]]$position <- i
    pred_data <- rbind(pred_data, pred_table[[1]])
  }
  colnames(pred_data) <- categories
  pred_data <- pred_data[!is.na(pred_data$MPG),]
  pred_data <-  pred_data[pred_data$Player!='Player',]
  return(pred_data)
}

data.df <- cbs_data(positions, "basketball","standard")

#Scored categories: (FG%),  (FT%),  (3PTM), (PTS), (REB), (AST),  (ST), (BLK),  (TO)
#categories <- c("Player", "G", "MPG", "FG", "FGP", "FT", "FTP", "3PT", "3PTP", "RPG", "APG", "SPG", "TPG", "BPG", "PPG", "FPTS", "Position")

index <- c(2:(ncol(data.df)-1))
#data.df <- droplevels(data.df)

## change to numeric, but since factor, need to change to a value first
for(i in index) {
  data.df[,i] <- as.numeric(as.character(data.df[,i]))
}
data.working <- data.df

score_cols <- c(5,7,8,10:15)

#For each scored category, for each position, calculate vor
vor_generic <- function(pos, repl_rank, df, score_cat){
  #order df based on scoring category
  df_order <- df[order(df[,score_cat], decreasing = T),]
  #which column is the scored cat?
  fpoints.ndx <- match(score_cat, colnames(df_order))
  #Find the mean vor based on the replacement rank
  vor_mean <- mean(df_order[df_order$Pos == pos,][c(repl_rank-1, repl_rank, repl_rank + 1),fpoints.ndx])
  print(vor_mean)
  #Add a vor column with the score - mean
  new_name <- paste0("vor",score_cat)
  #df_order[,new_name][df_order$Pos == pos] <- sapply(df_order[df_order$Pos == pos, fpoints.ndx], function(x) x - vor_mean)
  if (new_name %in% names(df_order)) {
    df_order[,new_name][df_order$Pos == pos] <- sapply(df_order[df_order$Pos == pos, fpoints.ndx], function(x) round(x - vor_mean, 3))
  } else {
    df_order$vor[df_order$Pos == pos] <- sapply(df_order[df_order$Pos == pos, fpoints.ndx], function(x) round(x - vor_mean, 3))  
    names(df_order)[ncol(df_order)] <- paste0("vor",score_cat)
  }
  return(df_order)
}

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

#######################################################
#Position independent
#######################################################
for (i in score_cols) {
  print(categories[i])
  data.working <- vor_pos_indep(30, data.working, categories[i])
}
data.working$vorTPG <- data.working$vorTPG * -1

for (i in score_cols){
  score_cat <- categories[i]
  new_name <- paste0("vor",score_cat)
  z_name <- paste0("Z", new_name)
  data.working[,z_name] <- scale(data.working[,new_name], center = T, scale = T)
}

#[1] "Player"  "G"       "MPG"     "FG"      "FGP"     "FT"      "FTP"     "3PT"     "3PTP"    "RPG"     "APG"     "SPG"    
#[13] "TPG"     "BPG"     "PPG"     "FPTS"    "Pos"     "vorFGP"  "vorFTP"  "vor3PT"  "vorRPG"  "vorAPG"  "vorSPG"  "vorTPG" 
#[25] "vorBPG"  "vorPPG"  "ZvorFGP" "ZvorFTP" "Zvor3PT" "ZvorRPG" "ZvorAPG" "ZvorSPG" "ZvorTPG" "ZvorBPG" "ZvorPPG"

data.working$Zavg_noTPG3PTM <- apply(data.working[,c(27:28,30:32,34:35)], 1, mean)
data.working[order(data.working$Zavg_noTPG3PTM, decreasing=T),][1:20,c(1,36)]

#######################################################
#For each position, for each scoring category, execute vor_generic
#######################################################
for (i in score_cols) {
  print(categories[i])
  data.working <- vor_generic("C", 20, data.working, categories[i])
  data.working <- vor_generic("G", 30, data.working, categories[i])
  data.working <- vor_generic("F", 30, data.working, categories[i])
}

data.working$vorTPG <- data.working$vorTPG * -1

#Take each vor, calcualte z-score for each vor
#??is this right? for each position, seems like it would need to be evaluated on a per position basis.
for (i in score_cols){
  score_cat <- categories[i]
  new_name <- paste0("vor",score_cat)
  z_name <- paste0("Z", new_name)
  data.working[,z_name] <- scale(data.working[,new_name], center = T, scale = T)
}

#Average the z-scores for the features you care about
#[1] "Player"     "G"          "MPG"        "FG"         "FGP"        "FT"         "FTP"        "3PT"        "3PTP"       "RPG"       
#[11] "APG"        "SPG"        "TPG"        "BPG"        "PPG"        "FPTS"       "Pos"        "vorFGP"     "vorFTP"     "vor3PT"    
#[21] "vorRPG"     "vorAPG"     "vorSPG"     "vorTPG"     "vorBPG"     "vorPPG"     27 "ZvorFGP"    "ZvorFTP"    "Zvor3PT"    "ZvorRPG"   
#[31] "ZvorAPG"    "ZvorSPG"    "ZvorTPG"    "ZvorBPG"    "ZvorPPG" 
data.working$Zvor_avg <- apply(data.working[,27:35], 1, mean)
data.working[order(data.working$Zvor_avg, decreasing=T),][1:30,c(1,36)]

data.working$Zavg_noAPG <- apply(data.working[,c(27:30,32:35)], 1, mean)
data.working[order(data.working$Zavg_noAPG, decreasing=T),][1:20,c(1,37)]

data.working$Zavg_noTPG <- apply(data.working[,c(27:32,34:35)], 1, mean)
data.working[order(data.working$Zavg_noTPG, decreasing=T),][1:20,c(1,38)]

data.working$Zavg_noSPG <- apply(data.working[,c(27:31,33:35)], 1, mean)
data.working[order(data.working$Zavg_noSPG, decreasing=T),][1:20,c(1,39)]

data.working$Zavg_noTPG3PTM <- apply(data.working[,c(27:28,30:32,34:35)], 1, mean)
data.working[order(data.working$Zavg_noTPG3PTM, decreasing=T),][1:20,c(1,40)]


aggregate(Zvor_avg ~ Pos, data = data.working[1:150,], summary)

pairs(data.working[1:150,c(5,7,8,10:15)])

####
#Non-vor z-score
####
data.working <- data.working[,c(-3,-4,-6,-9,-16)]
categories <- categories[c(-3,-4,-6,-9,-16)]

score_cols <- c(3:11)

data.working[data.working$FTP == 0,4]  <- mean(data.working$FTP)

for (i in score_cols){
  score_cat <- categories[i]
  new_name <- paste0("Z",score_cat)
  #data.order <- data.working[order(data.working[,score_cat], decreasing=T),]
  #z_name <- paste0("Z", new_name)
  data.working[,new_name] <- scale(data.working[,score_cat], center = T, scale = T)
}

data.working$Zavg_noTPG3PTM <- apply(data.working[,c(13,15:17,19:21)], 1, mean)
data.working <- data.working[order(data.working$Zavg_noTPG3PTM, decreasing=T),]

#3 FGP   FTP 3PT   RPG   APG  SPG  TPG  BPG   PPG

score_cols <- c(13:21)

data.checkpoint <- data.working

for (i in score_cols) {
  category <- colnames(data.working)[i]
  print(category)
  data.working <- vor_generic("C", 40, data.working, category)
  data.working <- vor_generic("G", 64, data.working, category)
  data.working <- vor_generic("F", 40, data.working, category)
}

data.working$vorTPG <- data.working$vorTPG * -1


data.working$ZPos_noTPG3PTM <- apply(data.working[,c(23,24,26:28,30,31)], 1, mean)
data.working <- data.working[order(data.working$ZPos_noTPG3PTM, decreasing=T),]


#Team analysis
davidsteam <- c("Kyrie","Batum","Dwyane","Evan Turner","Anthony Davis", "Blake Griffin", "Greg Monroe", "Dwight Howard","Deron Williams", "Elfrid Payton","Jahlil","Mozgov","Zaza")
davidsteam2 <- c("Kyrie","Batum","Dwyane","Evan Turner","Anthony Davis", "Blake Griffin", "Greg Monroe", "Enes Kanter","Deron Williams", "Elfrid Payton","Jahlil","Mozgov","Zaza")
carnsteam <- c("Harden","Monta Ellis", "Kemba Walker", "Draymond Green", "Andre Drummond", "Marvin Williams", "Valanciunas", "Enes Kanter", "Aminu", "Kaminsky", "Ish Smith", "Thon Maker", "Dante Exum")
harlemsteam <- c("Russell Westbrook", "Devin Booker", "Rajon Rondo", "Chandler Parsons", "DeMarcus Cousins", "Biyombo", "DeAndre Jordan", "Rudy Gobert", "Marc Gasol", "Jeremy Lin", "Derrick Rose", "Courtney Lee", "Porzingis")
seansteam <- c("John Wall", "Giannis", "elo Russell", "Carmelo Anthony", "Al Horford", "Tobias Harris", "Towns", "Robin Lopez", "Brandon Ingram", "Mudiay", "Stanley Johnson", "Dellavedova", "Korver")

teamstats <- function(playerlist){
  output.df <- data.frame()
  for (i in playerlist){
    player <- all.ordered[grep(i,all.ordered$Player),]
    print(player$Player)
    output.df <- rbind(output.df, player)
  }
  return(output.df)
}

david.df <- teamstats(davidsteam)
david.mean <- colMeans(david.df[,c(-1,-12)])

david2.df <- teamstats(davidsteam2)
david2.mean <- colMeans(david2.df[,c(-1,-12)])

carn.df <- teamstats(carnsteam)
carn.mean <- colMeans(carn.df[,c(-1,-12)])

harlem.df <- teamstats(harlemsteam)
harlem.mean <- colMeans(harlem.df[,c(-1,-12)])

sean.df <- teamstats(seansteam)
sean.mean <- colMeans(sean.df[,c(-1,-12)])
