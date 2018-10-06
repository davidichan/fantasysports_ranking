library(RCurl)
library(XML)
library(ggplot2)

url <- "https://www.hockey-reference.com/leagues/NHL_2016_skaters.html"
tables <- readHTMLTable(getURL(url), header=FALSE)

pred_URL <- "https://www.cbssports.com/fantasy/hockey/stats/sortable/points/C/advanced/projections/season?&print_rows=9999"
pred_table <- readHTMLTable(getURL(pred_URL), header=F)

## for tutorial sake -- its the first table in the list
data <- tables[[1]]

data.df <- data
cn <- data.df[21,]
cn <- lapply(cn, as.character)
colnames(data.df) <- cn
colnames(data.df)[c(16,17,18)] <- c("EV-A","PP-A","SH-A")

data.df <- data.df[data.df$Rk!='Rk',]
index <- c(1,3,6:(ncol(data.df)-1))
data.df <- droplevels(data.df)

## change to numeric, but since factor, need to change to a value first
for(i in index) {
  data.df[,i] <- as.numeric(as.character(data.df[,i]))
}

score_cols <- c(7,8,10,11,13,14,15,19,25,24)
score_facs <- c(1,1,1,0.25,1,2,2,0.25,0.1,0.25)

data.df$Fpoints <- apply(data.df[,score_cols],1, function(x) sum(x * score_facs))

aggregate(Fpoints ~ Pos, data = data.df, summary)
data.df$vor <- 0

vor <- function(pos, repl_rank, df){
  df_order <- df[order(df$Fpoints, decreasing = T),]
  fpoints.ndx <- match("Fpoints", colnames(df_order))
  vor_mean <- mean(df_order[df_order$Pos == pos,][c(repl_rank-1, repl_rank, repl_rank + 1),fpoints.ndx])
  df_order$vor[df_order$Pos == pos] <- sapply(df_order[df_order$Pos == pos, fpoints.ndx], function(x) x - vor_mean)
  return(df_order)
}

data.df <- vor("RW", 16, data.df)
data.df <- vor("LW", 16, data.df)
data.df <- vor("C", 16, data.df)
data.df <- vor("D", 32, data.df)

#Goalies
URL.g <- "https://www.hockey-reference.com/leagues/NHL_2016_goalies.html"
tables.g <- readHTMLTable(getURL(URL.g), header=FALSE)

data.g <- tables.g[[1]]
cn.g <- data.g[21,]
cn.g <- lapply(cn.g, as.character)
colnames(data.g) <- cn.g

data.g <- data.g[data.g$Rk!='Rk',]
index <- c(1,3,5:(ncol(data.g)))
data.g <- droplevels(data.g)

## change to numeric, but since factor, need to change to a value first
for(i in index) {
  data.g[,i] <- as.numeric(as.character(data.g[,i]))
}

data.g$Pos <- "G"

score_cols.g <- c(7,12,15)
score_facs.g <- c(2.5,0.1,5)
data.g$Fpoints <- apply(data.g[,score_cols.g],1, function(x) sum(x * score_facs.g))

data.g$vor <- 0
data.g <- vor("G", 16, data.g)

g.select <- data.g[,c("Rk", "Player","Pos","Fpoints","vor")]
skate.select <- data.df[,c("Rk", "Player","Pos","Fpoints","vor")]

all.select <- rbind(g.select, skate.select)
all.select <- all.select[order(all.select$vor, decreasing = T),]

vor_upto <- 100
half_one <- all.select[c(1:vor_upto),]
half_two <- all.select[c((vor_upto+1):nrow(all.select)),][order(all.select[c((vor_upto+1):nrow(all.select)),]$Fpoints,decreasing = T),]
all.select.order <- rbind(half_one, half_two)


################################################
positions <- c("C", "G","F")
stat_type <- "standard"
sport <- "basketball"
categories <- c("Player", "G", "MPG", "FG", "FGP", "FT", "FTP", "3PT", "3PTP", "RPG", "APG", "SPG", "TPG", "BPG", "PPG", "FPTS", "Position")
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

