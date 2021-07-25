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


##### cut down on the number of unique routes ######

pass <- pass %>%
  filter(!(Route %in% c("NULL", "Blocking", "Pick", "Quick", "Chip"))) %>%
  filter(!str_detect(Route, "Screen")) %>%
  mutate(Route = case_when(Route == "Jet Sweep Pass" ~ "Cross",
                           Route == "Post Corner" ~ "Cross",
                           Route == "Beneath" ~ "Flat",
                           Route == "Sluggo" ~ "Go",
                           Route == "Go/Fly" ~ "Go",
                           Route == "Leak" ~ "Go",
                           Route == "Drag" ~ "Cross",
                           Route == "Run Fake" ~ "Flat",
                           Route == "Chip - Drag" ~ "Cross",
                           Route == "Fade - Back Shoulder" ~ "Go",
                           Route == "Wheel" ~ "Go",
                           Route == "Fade" ~ "Go",
                           Route == "Out & Up" ~ "Go",
                           str_detect(Route, "Swing") ~ "Flat",
                           Route == "Hitch & Go" ~ "Go",
                           Route == "Check & Release" ~ "Flat",
                           Route == "Chip - Curl" ~ "Curl",
                           str_detect(Route, "Flat") ~ "Flat",
                           Route == "Screen - Drag" ~ "Cross",
                           Route == "Over Ball" ~ "Curl",
                           Route == "Chip - Seam" ~ "Go",
                           Route == "Jerk" ~ "In",
                           Route == "Deep Cross" ~ "Post",
                           Route == "Corner Post" ~ "Go",
                           Route == "Seam" ~ "Go",
                           Route == "Dig" ~ "In",
                           Route == "Whip" ~ "Out",
                           Route == "Stick - Nod" ~ "Post",
                           Route == "Angle" ~ "Slant",
                           T ~ Route))

pass %>%
  group_by(Route) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) -> routes

## Only 10 routes now
