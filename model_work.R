library(tidyverse)

pbp_df <- read.csv('Data/PlayByPlay.csv', stringsAsFactors = F) %>% tibble
skill_df <- read_csv('Data/SkillPositionPlayers.csv') %>% tibble
info_df <- read_csv('Data/GameInfo.csv') %>% tibble
route_mapping <- read_csv('route_mapping.csv')

# take only regular pass plays
pass_plays_df <- pbp_df %>% 
  filter(grepl('pass', EventType) & !DropType %in% c('Spike','Other','RB/WR Pass', 'WR Reverse Pass', 'Double Reverse Pass', 'Flea Flicker') & !(GameID == 2965 & OffensiveTeam == 'Broncos'))

# check on weird roster positions
skill_df %>% 
  filter(!RosterPosition %in% c('QB','FB','RB','WR','TE')) %>% 
  group_by(RosterPosition, OnFieldPosition) %>% 
  summarise(n = n()) %>% 
  view

# get routes and number of potential route runners
play_side_sum_df <- skill_df %>%
  group_by(GameID, EventID) %>%
  mutate(targeted_side = if_else(Target == 1, SideOfCenter, "NULL"),
         targeted_side = na_if(targeted_side, "NULL")) %>%
  fill(targeted_side, .direction = "updown") %>%
  ungroup() %>%
  left_join(route_mapping) %>% 
  filter(SideOfCenter != 'NULL') %>% 
  group_by(GameID, EventID, SideOfCenter) %>% 
  arrange(Order_OutsideToInside) %>% 
  summarise(
    side_players = n(),
    targeted_side = targeted_side,
    route_combo = paste(RouteGroup, collapse = ' | '),
    .groups = 'drop'
  )


model_data <- left_join(play_side_sum_df, pbp_df %>% select(GameID, EventID, EPA, OffensiveTeam, CoverageScheme, EventType, DropType), by = c("GameID", "EventID"))
model_data <- 
  model_data %>% 
  filter(grepl('pass', EventType) & !DropType %in% c('Spike','Other','RB/WR Pass', 'WR Reverse Pass', 'Double Reverse Pass', 'Flea Flicker') & !(GameID == 2965 & OffensiveTeam == 'Broncos'))
model_data <- 
  model_data %>%
  mutate(EPA = na_if(EPA, "NULL"),
         EPA = as.numeric(EPA))
model_data <-
  model_data %>%
  filter(SideOfCenter == targeted_side)


library(lme4)
library(broom.mixed)
library(forcats)
model <- lmer(data = model_data, EPA ~ (1|route_combo) + (1|CoverageScheme) + (1|OffensiveTeam), REML = FALSE)
summary(model)
model_tidy <- broom.mixed::tidy(model, effects = "ran_vals") %>% filter(group == "route_combo")
model_tidy <- model_tidy%>% mutate(num_routes = str_count(level, coll("|") ) + 1)


write_csv(model_tidy, "model_tidy.csv")

model_tidy %>% 
  mutate(level = as_factor(level)) %>%
  filter(num_routes == 2) %>%
  mutate(level = fct_reorder(level, estimate)) %>%
  arrange(desc(estimate)) %>%
  slice(1:10) %>%
  ggplot(aes(x = estimate, y = level)) +
  geom_point() +
  geom_linerange(aes(xmin = estimate - 2*std.error, xmax = estimate + 2*std.error))

