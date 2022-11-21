
library(rvest)
library(tidyverse)
nba <- read_csv(here::here("dataset", "nbaNew.csv"), 
                col_types = cols(PlayerSalary = col_number(), 
                                 `TS%` = col_number(),
                                 `ORB%` = col_number(),
                                 `DRB%` = col_number(),
                                 `TRB%` = col_number(),
                                 `AST%` = col_number(),
                                 `STL%` = col_number(),
                                 `BLK%` = col_number(),
                                 `TOV%` = col_number(),
                                 `USG%` = col_number(),
                                 `FG%` = col_number(),
                                 `3P%` = col_number(),
                                 `2P%` = col_number(),
                                 `eFG%` = col_number(),
                                 `FT%` = col_number()))
nba_clean<- select(nba, -(blanl), -(blank2), -(`3P`), -(`3PA`),-(`3P%`))%>%
  na_if("")%>%
  filter(!row_number() %in% c(24625, 24626, 24627, 24628))

write_csv(nba_clean, 
          file = here::here("dataset", "nba_clean.csv"))
save(nba_clean, file = here::here("dataset/nba_clean.RData"))


nba_clean <- read_csv(here::here("dataset", "nba_clean.csv"))

Injury <- read_csv(here::here("dataset", "injuries_2010-2020 2.csv"), 
                   col_types = cols_only(Date = col_character(), 
                                         Team = col_character(), 
                                         Relinquished = col_character()))
colnames(Injury)[3] <- "PlayerName"
colnames(Injury)[1] <- "SeasonStart"
colnames(Injury)[2] <- "Tm"
Injury <- Injury %>% filter(!is.na(PlayerName))
Injury <- Injury %>% mutate(injuries_data = 1)
Injury$SeasonStart <- substr(Injury$SeasonStart,1,4)

Injury_clean <- Injury %>% filter(SeasonStart %in% c("2014","2015","2016"))

playoffs <- nba_clean%>%
  filter(SeasonStart%in%c("2014","2015","2016"))%>%
  filter(Tm%in%c("IND","MIA", "TOT", "CHA", "WAS", "BRK","CHI", "ATL","SAS",
                 "OKC","LAC","HOU","POR","GSW","MEM","DAL", "NOP", "CLE", 
                 "TOR", "BOS", "MIL","DET"))
playoffs$SeasonStart <- as.character(playoffs$SeasonStart)

Injury_clean<- Injury_clean %>%
  mutate(Tm = recode(Tm, 'Pacers' = 'IND', 'Heat' = 'MIA','Hornets' = 'CHA',
                     'Wizards' = 'WAS','Nets' = 'BRK','Bulls' = 'CHI',
                     'Hawks' = 'ATL','Spurs' = 'SAS','Thunder' = 'OKC', 
                     'Thunder' = 'LAC','Rockets' = 'HOU','Blazers' = 'POR',
                     'Warriors' = 'GSW','Grizzlies' = 'MEM','Mavericks' = 'DAL',
                     'Pelicans' = 'NOP','Cavaliers' = 'CLE','Raptors' = 'TOR',
                     'Celtics' = 'BOS','Bucks' = 'MIL','Pistons' = 'DET'))%>%
  filter(Tm%in%c("IND","MIA", "TOT", "CHA", "WAS", "BRK","CHI", "ATL","SAS",
                 "OKC","LAC","HOU","POR","GSW","MEM","DAL","NOP", "CLE", "TOR",
                 "BOS", "MIL","DET"))

write_csv(Injury_clean, 
          file = here::here("dataset", "Injury_clean.csv"))
save(nba_clean, file = here::here("dataset/Injury_clean.RData"))

write_csv(playoffs, 
          file = here::here("dataset", "playoffs.csv"))
save(playoffs, file = here::here("dataset/playoffs.RData"))

combine <- playoffs %>% left_join(Injury_clean, 
                                  by = c("PlayerName","SeasonStart","Tm"))%>% 
  mutate(injuries_data = coalesce(injuries_data, 0))


write_csv(combine, 
          file = here::here("dataset", "combine.csv"))
save(combine, file = here::here("dataset/combine.RData"))


