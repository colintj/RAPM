library(tidyverse)
library(rvest)
library(xml2)
library(mgcv)

# draft_years <- 2014:2020
# 
# draft <- list()
# for(year in draft_years) {
#   url <- paste0("https://www.basketball-reference.com/draft/NBA_",year,".html")
#   html <- read_html(url)
#   table <- html_table(html)[[1]]
#   
#   colnames(table) <- table[1,]
#   table <- table[-1,-9:-18]
#   table <- table[which(!table$Pk %in% c('', 'Pk')),]
#   table$Season <- year
#   draft[[year-2013]] <- table
#   print(year)
# }
# 
# draft_df <- bind_rows(draft)
# write_csv(draft_df, "draft_data.csv")
draft_df <- read_csv("draft_data.csv")



rapm <- read_csv("RAPM_2013-20.csv")

rapm_yr_adj <- lm(RAPM_prior ~ yr, rapm %>% filter(MINS > 500))

combo <- draft_df %>%
  mutate(
    College = case_when(
      College == "Oklahoma State" ~ "Oklahoma St.",
      College == "Michigan State" ~ "Michigan St.",
      College == "Boise State" ~ "Boise St.",
      College == "College of Charleston" ~ "Col. of Charleston",
      College == "Florida State" ~ "Florida St.",
      College == "Georgia State" ~ "Georgia St.",
      College == "Iowa State" ~ "Iowa St.",
      College == "Kansas State" ~ "Kansas St.",
      College == "Mississippi State" ~ "Mississippi St.",
      College == "Murray State" ~ "Murray St.",
      College == "Missouri State" ~ "Missouri St.",
      College == "New Mexico State" ~ "New Mexico St.",
      College == "Ohio State" ~ "Ohio St.",
      College == "Penn State" ~ "Penn St.",
      College == "Pitt" ~ "Pittsburgh",
      College == "San Diego State" ~ "San Diego St.",
      College == "St. John's" ~ "St. John's (NY)",
      College == "UMass" ~ "Massachusetts",
      College == "USC" ~ "Southern California",
      College == "Utah State University" ~ "Utah St.",
      College == "Washington State" ~ "Washington St.",
      College == "Weber State" ~ "Weber St.",
      College == "Wichita State" ~ "Wichita St.",
      College == "UNC" ~ "North Carolina",
      College == "Eastern Washington" ~ "Eastern Wash.",
      College == "Georgia State University" ~ "Georgia St.",
      T ~ College
    ),
    Player = case_when(
      Player == "Wendell Carter Jr." ~ "Wendell Carter",
      Player == "Gary Trent Jr." ~ "Gary Trent",
      Player == "Vernon Carey Jr." ~ "Vernon Carey",
      Player == "OG Anunoby" ~ "O.G. Anunoby",
      Player == "Kelly Oubre Jr." ~ "Kelly Oubre",
      Player == "Frank Mason III" ~ "Frank Mason",
      Player == "Skal Labissière" ~ "Skal Labissiere",
      Player == "P.J. Washington" ~ "PJ Washington",
      Player == "Devyn Marble" ~ "Roy Devyn Marble",
      Player == "Bam Adebayo" ~ "Edrice Adebayo",
      Player == "Johnny O'Bryant" ~ "Johnny O'Bryant III",
      Player == "Lonnie Walker" ~ "Lonnie Walker IV",
      Player == "Xavier Tillman Sr." ~ "Xavier Tillman",
      Player == "Michael Porter Jr." ~ "Michael Porter",
      Player == "Dennis Smith Jr." ~ "Dennis Smith",
      Player == "Kay Felder" ~ "Kahlil Felder",
      Player == "Joe Young" ~ "Joseph Young",
      Player == "Troy Brown Jr." ~ "Troy Brown",
      Player == "Vince Edwards" ~ "Vincent Edwards",
      Player == "DeAndre' Bembry" ~ "Deandre Bembry",
      Player == "Mo Bamba" ~ "Mohamed Bamba",
      Player == "T.J. Leaf" ~ "TJ Leaf",
      Player == "DeAndre Daniels" ~ "Deandre Daniels",
      Player == "Stephen Zimmerman" ~ "Stephen Zimmerman Jr.",
      Player == "Wade Baldwin" ~ "Wade Baldwin IV",
      Player == "Robert Woodard" ~ "Robert Woodard II",
      T ~ Player
    ),
    across(.cols = c("Rk","Pk","Yrs","G","MP","WS","WS/48", "BPM","VORP"), as.numeric)
  ) %>%
  left_join(rapm %>%
              mutate(
                cleanName = case_when(cleanName == "MontÃ© Morris" ~ "Monte Morris", T ~ cleanName),
                yr = case_when(cleanName == "Josh Jackson" ~ "Fr", cleanName == "Cheick Diallo" ~ "Fr",T ~ yr)
                ), 
            by = c("Player"="cleanName", "College"="Team"))

last_year <- combo %>%
  group_by(Player, College) %>%
  slice_max(Season.y) %>%
  ungroup() %>%
  mutate(
    yr = as.factor(yr),
  ) %>%
  rename("WS48" = "WS/48")



