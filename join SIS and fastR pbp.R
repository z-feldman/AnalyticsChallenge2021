library(tidyverse)
library(nflfastR)

# pbp from SIS
pbp_df <- read.csv('Data/PlayByPlay.csv', stringsAsFactors = F) %>% tibble

# grab nflfastR pbp and rename columns
# also lookup each team's name
fastR_pbp <- load_pbp(2020) %>% 
  filter(season_type == 'REG' & play_type != 'no_play') %>% 
  left_join(nflfastR::teams_colors_logos %>% select(posteam = team_abbr, OffensiveTeam = team_nick)) %>% 
  left_join(nflfastR::teams_colors_logos %>% select(defteam = team_abbr, DefensiveTeam = team_nick)) %>% 
  rename(Down = down, ToGo = ydstogo, TimeLeft = quarter_seconds_remaining, Quarter = qtr, Week = week, OffensiveYardage = yards_gained)

# first pass at joining with fastR data
fastR_join1 <- pbp_df %>% inner_join(fastR_pbp %>% select(game_id, play_id, Down, ToGo, TimeLeft, Quarter, Week, OffensiveTeam, DefensiveTeam))
# look at only plays that are missing
missing_fastR <- pbp_df %>% anti_join(fastR_pbp %>% select(game_id, play_id, Down, ToGo, TimeLeft, Quarter, Week, OffensiveTeam, DefensiveTeam))

# try to join again, but without TimeLeft, which seems to be the one that is wrong most of the time
# take the closest play that happens within 25 seconds of what fastR has and matches all other criteria
fastR_join2 <- missing_fastR %>%
  inner_join(fastR_pbp %>% select(game_id, play_id, Down, ToGo, wrong_time = TimeLeft, Quarter, Week, OffensiveTeam, DefensiveTeam)) %>% 
  group_by(GameID, EventID) %>% 
  arrange(abs(TimeLeft - wrong_time)) %>% 
  filter(row_number() == 1 & abs(TimeLeft - wrong_time) <= 25) %>% 
  mutate(
    wrong_time = NULL,
    # one double match from a Rams game
    play_id = ifelse(GameID == 2907 & EventID == 340, 1551, play_id)
  ) %>% 
  ungroup

# manually map the final 13 plays that do not match above
pbp_df %>%
  anti_join(bind_rows(fastR_join1, fastR_join2)) %>%
  select(GameID, EventID, PlayDesc)

# look for where these plays probably are in fastR
fastR_pbp %>% 
  anti_join(bind_rows(fastR_join1, fastR_join2) %>% select(game_id, play_id)) %>% 
  filter(!play_type %in% c('punt','field_goal') & season_type == 'REG' & !is.na(Down) & !grepl('Penalty', desc, ignore.case = T) & !grepl('Punt', desc, ignore.case = T)) %>% 
  select(game_id, play_id, TimeLeft, desc)

# now do the mapping for the unmatched plays
fastR_join3 <- pbp_df %>%
  anti_join(bind_rows(fastR_join1, fastR_join2)) %>%
  mutate(
    game_id = NA,
    play_id = case_when(
      GameID == 2794 & EventID == 70 ~ 369,
      GameID == 2797 & EventID == 40 ~ 180,
      GameID == 2797 & EventID == 45 ~ 2026,
      GameID == 2814 & EventID == 905 ~ 4313,
      GameID == 2858 & EventID == 695 ~ 3270,
      GameID == 2891 & EventID == 720 ~ 3296,
      GameID == 2899 & EventID == 880 ~ 4264,
      GameID == 2935 & EventID == 560 ~ 2743,
      GameID == 2935 & EventID == 565 ~ 2767,
      GameID == 2935 & EventID == 570 ~ 2788,
      GameID == 2966 & EventID == 870 ~ 4100,
      GameID == 3046 & EventID == 35 ~ 208,
      GameID == 3046 & EventID == 45 ~ 232
    )
  )

# put it all together and save
# filling in some missing game_ids from fastR_join3 as well (lazy)
bind_rows(fastR_join1, fastR_join2, fastR_join3) %>% 
  group_by(GameID) %>% 
  fill(game_id, .direction = 'downup') %>% 
  ungroup %>% 
  select(GameID, EventID, game_id, play_id) %>% 
  saveRDS('fact_fastRlookup.rds')


