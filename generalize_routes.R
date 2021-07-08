##### 2021 SIS competition EDA ####
library(tidyverse)
pbp <- read_csv("Data/PlayByPlay.csv")
skill <- read_csv("Data/SkillPositionPlayers.csv")

pbp <- pbp %>%
  inner_join(skill)

pass <- pbp %>%
  filter(EventType %in% c("pass", "challenge pass"))

pass %>%
  filter(Route != "NULL") %>%
  group_by(Route) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) -> routes

# 49 unique routes, narrow that down to about 12-14

# First get rid of the extra descriptions
pass <- pass %>%
  mutate(Route = ifelse(str_detect(Route, "-"), str_extract(Route, ".+?(?= -)"), Route))

pass %>%
  filter(Route != "NULL") %>%
  group_by(Route) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) -> routes

# Down to only 35 unique routes now
pass <- pass %>%
  mutate(Route = case_when(Route == "Fade" ~ "Go/Fly",
                           Route == "Sluggo" ~ "Go/Fly",
                           Route == "Chip" ~ "Blocking",
                           Route == "Seam" ~ "Go/Fly",
                           Route == "Fade" ~ "Go/Fly",
                           Route == "Deep Cross" ~ "Slant",
                           Route == "Beneath" ~ "Slant",
                           Route == "Swing" ~ "Wheel",
                           Route == "Drag" ~ "Dig",
                           Route == "Whip" ~ "Out",
                           Route == "Over Ball" ~ "Slant",
                           Route == "Pick" ~ "Blocking",
                           Route == "Hitch & Go" ~ "Go/Fly",
                           Route == "Run Fake" ~ "Blocking",
                           Route == "Quick" ~ "Out",
                           Route == "Post Corner" ~ "Corner",
                           Route == "Stick" ~ "Curl",
                           Route == "Out & Up" ~ "Go/Fly",
                           Route == "Corner Post" ~ "Corner",
                           Route == "Angle" ~ "Slant",
                           Route == "Jerk" ~ "In", # Might have to change this one since there are no other in routes
                           Route == "Check & Release" ~ "Blocking",
                           Route == "Leak" ~ "Wheel",
                           T ~ Route))


pass %>%
  filter(Route != "NULL") %>%
  group_by(Route) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) -> routes

