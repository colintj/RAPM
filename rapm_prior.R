library(bigballR)
library(tidyverse)
library(Matrix)
library(glmnet)
library(RMariaDB)

conn <- dbConnect(
  RMariaDB::MariaDB(),
  user = Sys.getenv("USER_ID"),
  password = Sys.getenv("PASSWORD"),
  dbname = Sys.getenv("DB_NAME"),
  host = "localhost"
)

season <- "2020-21"
pbp <- readRDS(paste0('/Users/jake/ncaa_pbp_data/pbp', gsub("-","",season),'.Rds'))

pbp_correct <- pbp %>%
  # Throwing out bad pbp
  filter(!isGarbageTime, Sub_Deviate <= 50, Status != "NO_PLAYER", across(matches("Home\\.|Away\\."), ~!is.na(.x))) %>%
  dplyr::rename_with(~gsub("_","\\.",.x), matches("Home_|Away_"))
rm(pbp)

players_thresh <- dbGetQuery(conn, paste0("SELECT Player FROM player_stats WHERE Season = '", season, "'"))$Player
players_pbp <- pbp_correct %>% select(Home.1:Away.5) %>% unlist(use.names = F) %>% unique()
players_final <- players_pbp[players_pbp %in% players_thresh]

poss <- get_possessions(pbp_correct, T)
# Throw away small number of possessions with players not included in stats/roster data above
poss_final <- poss %>%
  filter(across(matches("Home\\.|Away\\."), ~.x%in% players_final))
rm(poss)

# write_csv(poss_final,paste0("/Users/jake/Dropbox/RAPM_data/possessions", gsub("-","",season),'.csv'))

cols <- c(paste0(players_final,"_O"), paste0(players_final, "_D"))

# Store possessions in sparse matrix format
row_id <- rep(1:nrow(poss_final), each=10)
len <- length(players_final)

# Get col ids for home players and away players
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

# Adding HCA indicator
row_id_filter <- c(row_id_filter, which(poss_final$Home == poss_final$Poss_Team))
col_id_filter <- c(col_id_filter, rep(max(col_id_filter)+1, length(which(poss_final$Home == poss_final$Poss_Team))))

sparse_mat <- sparseMatrix(i=row_id_filter, j = col_id_filter)

y <- poss_final$PTS * 100 # Using standard per 100

# Fitting initial RAPM model
rapm_model <- glmnet(sparse_mat, y, lambda = 0.25, alpha = 0, standardize = F)
rapm_coefs <- rapm_model$beta

bpm <- dbGetQuery(conn, paste0("SELECT * FROM raw_bpm WHERE Season = '", season, "'"))

# Get estimate of team level rapm
# This potentially overfits since it uses aspects of the RAPM response variable as a predictor of sRAPM
team_rapm <- data.frame(
  Player = players_final,
  Name = gsub("_(.*)", "", players_final),
  Team = gsub("(.*)_", "", players_final),
  ORAPM = rapm_coefs[1:length(players_final)],
  DRAPM = -rapm_coefs[(length(players_final)+1):(length(players_final)*2)]
  ) %>%
  left_join(bpm) %>%
  select(Player:DRAPM, MINS) %>%
  arrange(MINS) %>%
  group_by(Team) %>%
  slice_head(prop = 0.4) %>% # Get the bottom 40% of the roster in terms of minutes played to estimate "bench" RAPM
  summarise(
    # Shrink the bench estimate with 1,500 minutes of average RAPM
    TeamORAPM = sum(ORAPM*MINS)/(sum(MINS)+1500),
    TeamDRAPM = sum(DRAPM*MINS)/(sum(MINS)+1500),
    .groups = 'drop'
    )

bpm_prior <- bpm %>%
  left_join(team_rapm) %>%
  mutate(
    # Shrink low minutes players to team bench production
    # Using discrete cutoff- may want to revise
    OBPM_Shrink = ifelse(MINS<450,(100*TeamORAPM + MINS*OBPM)/(100+MINS),OBPM),
    DBPM_Shrink = ifelse(MINS<450,(100*TeamDRAPM + MINS*DBPM)/(100+MINS),DBPM),
    OBPM_Shrink = ifelse(is.na(OBPM_Shrink), 0, OBPM_Shrink),
    DBPM_Shrink = ifelse(is.na(DBPM_Shrink), 0, DBPM_Shrink),
    ODif = OBPM_Shrink - OBPM,
    DDif = DBPM_Shrink - DBPM,
    Player = paste0(Name, "_", Team)
    )

# Using BPM estimates to calculate expected points added by home/away team by possession
home_bpm <- ifelse(
  poss_final$Poss_Team == poss_final$Home,
  bpm_prior$OBPM_Shrink[match(poss_final$Home.1, bpm_prior$Player)] +
    bpm_prior$OBPM_Shrink[match(poss_final$Home.2, bpm_prior$Player)] +
    bpm_prior$OBPM_Shrink[match(poss_final$Home.3, bpm_prior$Player)] +
    bpm_prior$OBPM_Shrink[match(poss_final$Home.4, bpm_prior$Player)] +
    bpm_prior$OBPM_Shrink[match(poss_final$Home.5, bpm_prior$Player)],
  bpm_prior$DBPM_Shrink[match(poss_final$Home.1, bpm_prior$Player)] +
    bpm_prior$DBPM_Shrink[match(poss_final$Home.2, bpm_prior$Player)] +
    bpm_prior$DBPM_Shrink[match(poss_final$Home.3, bpm_prior$Player)] +
    bpm_prior$DBPM_Shrink[match(poss_final$Home.4, bpm_prior$Player)] +
    bpm_prior$DBPM_Shrink[match(poss_final$Home.5, bpm_prior$Player)]
)
home_bpm <- ifelse(is.na(home_bpm), 0, home_bpm)

away_bpm <- ifelse(
  poss_final$Poss_Team == poss_final$Away,
  bpm_prior$OBPM_Shrink[match(poss_final$Away.1, bpm_prior$Player)] +
    bpm_prior$OBPM_Shrink[match(poss_final$Away.2, bpm_prior$Player)] +
    bpm_prior$OBPM_Shrink[match(poss_final$Away.3, bpm_prior$Player)] +
    bpm_prior$OBPM_Shrink[match(poss_final$Away.4, bpm_prior$Player)] +
    bpm_prior$OBPM_Shrink[match(poss_final$Away.5, bpm_prior$Player)],
  bpm_prior$DBPM_Shrink[match(poss_final$Away.1, bpm_prior$Player)] +
    bpm_prior$DBPM_Shrink[match(poss_final$Away.2, bpm_prior$Player)] +
    bpm_prior$DBPM_Shrink[match(poss_final$Away.3, bpm_prior$Player)] +
    bpm_prior$DBPM_Shrink[match(poss_final$Away.4, bpm_prior$Player)] +
    bpm_prior$DBPM_Shrink[match(poss_final$Away.5, bpm_prior$Player)]
)
away_bpm <- ifelse(is.na(away_bpm), 0, away_bpm)

# Calculate "expected points" on the possession using BPM/team prior
exp_pts <- ifelse(poss_final$Poss_Team == poss_final$Home | is.na(poss_final$Poss_Team),
                  home_bpm - away_bpm,
                  away_bpm - home_bpm
                  )

# Subtract off the prior from the response points
y_prior <- y-exp_pts

# lambdas= seq(0.05,1, by = 0.05)
# cv_mod <- cv.glmnet(sparse_mat, y_prior, alpha = 0, standardize = F, lambda = lambdas)
# plot(cv_mod)
# cv_mod$lambda.min

model <- glmnet(sparse_mat, y_prior, lambda = 0.45, alpha = 0, standardize = F)

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
    RAPM = ORAPM + DRAPM,
    Season = season
  ) %>%
  left_join(bpm_prior) %>% # Add back prior to new RAPM estimate
  mutate(
    ORAPM_prior = ORAPM+OBPM_Shrink,
    DRAPM_prior = DRAPM+DBPM_Shrink,
    RAPM_prior = ORAPM_prior+DRAPM_prior
  ) %>%
  select(Player:Team, Season, MINS, ORAPM_prior:RAPM_prior)

dbAppendTable(conn, "rapm_prior", player_rapm)
