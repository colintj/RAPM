library(bigballR)
library(tidyverse)
library(Matrix)
library(glmnet)
library(RMariaDB)

conn <- dbConnect(
  RMariaDB::MariaDB(),
  user = Sys.getenv('USER_ID'),
  password = Sys.getenv('PASSWORD'),
  dbname = Sys.getenv("DB_NAME"),
  host = "localhost"
)

stats <- dbGetQuery(conn, "SELECT * FROM player_stats")
players <- dbGetQuery(conn, "SELECT * FROM players")
rapm <- dbGetQuery(conn, "SELECT * FROM raw_rapm")

full_df <- stats %>%
  mutate(
    Name = gsub("_(.*)", "", Player)
  ) %>%
  inner_join(players, by = c("Name"="player", "Team"="team","Season"="season")) %>%
  inner_join(rapm, by = c("Name", "Team", "Season"))

# Impute position based on player height for players missing. If no height, default to forward
ggplot(full_df, aes(x=HtInches, fill = pos)) + geom_bar() + xlim(75, 90)

prepped_df <- full_df %>%
  mutate(
    pos = case_when(
      !is.na(pos) ~ pos,
      HtInches > 78 ~ "F",
      T ~ "G"
    )
  ) %>%
  group_by(pos) %>%
  mutate(
    HtInches = ifelse(is.na(HtInches), mean(round(HtInches)), HtInches)
  ) %>%
  ungroup()

# training data- using 2013-2016
model_df <- prepped_df %>%
  mutate(
    across(c(PTS, ORB, DRB, AST, STL, BLK, TOV, PF, RIMA, FTA, TPA, FGA, TPM), ~.x/oPOSS*100),
    RIMA = ifelse(FGA==0,0,RIMA/FGA),
    TPA = ifelse(FGA==0,0,TPA/FGA),
    FTA = ifelse(FGA==0,0,FTA/FGA),
    PACE = (MINS*60)/oPOSS,
    PCT_ST = GS/GP,
    MinWeight = MINS^2
  )

train_df <- model_df %>%
  filter(
    Season %in% c("2013-14", "2014-15", "2015-16", "2016-17")
    )

obpm_model <- lm(ORAPM ~ PTS + ORB + AST*pos + STL + BLK*pos + TOV*pos + PF*pos + RIMA + FGA  + TPA*pos + FTA*pos + PCT_FGA_trans + PCT_FGM_ast*pos + eFG., 
                 data = train_df,
                 weights = MinWeight)

# summary(obpm_model)

dbpm_model <- lm(DRAPM ~ ORB*pos + DRB + AST*pos + STL + BLK + STL*pos + BLK*pos + TOV + PF*pos + RIMA + TPA + FTA + PCT_FGA_trans + PCT_FGM_ast, 
                 data = train_df,
                 weights = MinWeight)
# summary(dbpm_model)

obpm_pred <- unname(predict(obpm_model, newdata = model_df, type = "response"))
dbpm_pred <- unname(predict(dbpm_model, newdata = model_df, type = "response"))

bpm <- model_df %>%
  select(Name, Team, Season, MINS) %>%
  mutate(
    OBPM = obpm_pred,
    DBPM = dbpm_pred,
    BPM = OBPM+DBPM
  )

dbWriteTable(conn, "raw_bpm", bpm, overwrite = T)

