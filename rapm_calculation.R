library(bigballR)
library(tidyverse)
library(Matrix)
library(glmnet)
library(RMariaDB)

conn <- dbConnect(
  RMariaDB::MariaDB(),
  user = Sys.getenv('USER_ID'),
  password = Sys.getenv('PASSWORD'),
  dbname = Sys.getenv('DB_NAME'),
  host = "localhost"
)

season <- "2020-21"
pbp <- readRDS(paste0('/Users/jake/ncaa_pbp_data/pbp', gsub("-","",season),'.Rds'))

pbp_correct <- pbp %>%
  filter(!isGarbageTime, Sub_Deviate <= 50, Status != "NO_PLAYER", across(matches("Home\\.|Away\\."), ~!is.na(.x))) %>%
  dplyr::rename_with(~gsub("_","\\.",.x), matches("Home_|Away_"))
rm(pbp)

players_thresh <- dbGetQuery(conn, paste0("SELECT Player FROM player_stats WHERE Season = '", season, "'"))$Player
players_pbp <- pbp_correct %>% select(Home.1:Away.5) %>% unlist(use.names = F) %>% unique()
players_final <- players_pbp[players_pbp %in% players_thresh]

poss <- get_possessions(pbp_correct, T)
# Throw away all possessions with low minute players --- not sure about this ehhh?
poss_final <- poss %>%
  filter(across(matches("Home\\.|Away\\."), ~.x%in% players_final))
rm(poss)

cols <- c(paste0(players_final,"_O"), paste0(players_final, "_D"))

# Store possessions in sparse matrix format
row_id <- rep(1:nrow(poss_final), each=10)
len <- length(players_final)

home <- apply(poss_final[,7:11], 2, function(x){
  match(x, players_final)+ifelse(poss_final$Home==poss_final$Poss_Team, 0, len)
})
away <- apply(poss_final[,12:16], 2, function(x){
  match(x, players_final)+ifelse(poss_final$Away==poss_final$Poss_Team, 0, len)
})
result <- cbind(home, away)
col_id <- as.vector(t(result))
rm(result)
rm(away)
rm(home)

row_id_filter <- row_id[which(!is.na(col_id))]
col_id_filter <- col_id[which(!is.na(col_id))]

# Adding HCA
row_id_filter <- c(row_id_filter, which(poss_final$Home == poss_final$Poss_Team))
col_id_filter <- c(col_id_filter, rep(max(col_id_filter)+1, length(which(poss_final$Home == poss_final$Poss_Team))))

sparse_mat <- sparseMatrix(i=row_id_filter, j = col_id_filter)

y <- poss_final$PTS * 100 # Using standard per 100

# Note: used cv.glmnet to determine lambda value
model <- glmnet(sparse_mat, y, lambda = 0.25, alpha = 0, standardize = F)

coefs <- model$beta
c(max(coefs), min(coefs))

player_rapm <- data.frame(
  Player = players_final,
  Name = gsub("_(.*)", "", players_final),
  Team = gsub("(.*)_", "", players_final),
  ORAPM = coefs[1:length(players_final)],
  DRAPM = -coefs[(length(players_final)+1):(length(players_final)*2)]
) %>%
  mutate(
    ORAPM = ORAPM,
    DRAPM = DRAPM,
    RAPM = ORAPM + DRAPM,
    Season = season
  )

dbAppendTable(conn, "raw_rapm", player_rapm)


