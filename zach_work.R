library(tidyverse)
library(janitor)

skill_pos_players <- read_csv("Data/SkillPositionPlayers.csv") %>% clean_names()

pbp <- read_csv("Data/PlayByPlay.csv") %>% clean_names()
pbp$event_type %>% unique()
pbp <-
  pbp %>%
  mutate(event_type = if_else(str_detect(event_type, "challenge "), str_remove(event_type, "challenge "), event_type))
#pbp$event_type %>% unique()

game <- read_csv("Data/GameInfo.csv") %>% clean_names()

skill_pos_players <-
  left_join(skill_pos_players, select(pbp, game_id, event_id, event_type), by = c("game_id", "event_id")) %>%
  filter(event_type == "pass")

skill_pos_players <-
  skill_pos_players %>%
  mutate(side_of_center = na_if(side_of_center, "NULL"),
         order_outside_to_inside = na_if(order_outside_to_inside, "NULL") %>% as.numeric(),
         route = na_if(route, "NULL"))

#skill_pos_players$route %>% unique() %>% sort()

skill_pos_players <-
  skill_pos_players %>%
  mutate(route = case_when(
    route == "Chip" ~ "Blocking",
    route == "Check & Release" ~ "Check_Release",
    str_starts(route, "Chip -") ~ str_remove(route, "Chip -"),
    route == "Corner Post" ~ "Corner_Post",
    route == "Deep Cross" ~ "Deep_Cross",
    route == "Fade - Back Shoulder" ~ "Fade",
    str_starts(route, "Flat") ~ "Flat",
    route == "Go/Fly" ~ "Go",
    route == "Hitch & Go" ~ "Hitch_Go",
    route == "Jet Sweep Pass" ~ "Jet_Sweep_Pass",
    route == "Out & Up" ~ "Out_Up",
    route == "Over Ball" ~ "Overball",
    route == "Post Corner" ~ "Post_Corner",
    route == "Run Fake" ~ "Run_Fake",
    str_starts(route, "Screen") ~ str_c(on_field_position, "_Screen"),
    route == "Stick - Nod" ~ "Stick_Nod",
    str_starts(route, "Swing") ~ "Swing",
    TRUE ~ route
  ))

skill_pos_players <-
  skill_pos_players %>%
  mutate(route = str_trim(route),
         route = str_to_lower(route))

skill_pos_players <-
  skill_pos_players %>%
  mutate(route = if_else(is_blocking == 1, "blocking", route))
#skill_pos_players$route %>% unique()


# deal with backfield
skill_pos_players <-
  skill_pos_players %>%
  group_by(game_id, event_id) %>%
  mutate(side_of_center = if_else(on_field_position == "B", "B", side_of_center),
         order_outside_to_inside = if_else(on_field_position == "B", 1, order_outside_to_inside)) %>%
  group_by(game_id, event_id, side_of_center) %>%
  mutate(order_outside_to_inside = if_else(on_field_position == "B", as.double(row_number()), order_outside_to_inside)) %>%
  ungroup()
         
skill_pos_players <-
  skill_pos_players %>%
  group_by(game_id, event_id) %>%
  mutate(target_side = if_else(target == 1, side_of_center, "NULL"),
         target_side = na_if(target_side, "NULL")) %>%
  fill(target_side, .direction = "downup") %>%
  mutate(target_side = if_else(is.na(target_side), "no_pass", target_side)) %>%
  ungroup()

rec_by_side <-
  skill_pos_players %>%
  filter(!is.na(route), !is.na(side_of_center)) %>%
  pivot_wider(id_cols = c(game_id, event_id, side_of_center, target_side), names_from = order_outside_to_inside, values_from = route) %>%
  rowwise() %>%
  mutate(num_rec = sum(!is.na(`1`), !is.na(`2`), !is.na(`3`), !is.na(`4`)))

rec_by_side <-
  rec_by_side %>%
  group_by(game_id, event_id) %>%
  mutate(num_blockers = 10 - sum(num_rec)) %>% 
  mutate(num_blockers = num_blockers + sum(`1` %in% c("check_release", "blocking"), na.rm = TRUE) + sum(`2` %in% c("check_release", "blocking"), na.rm = TRUE) + sum(`3` %in% c("check_release", "blocking"), na.rm = TRUE) + sum(`4` %in% c("check_release", "blocking"), na.rm = TRUE)) %>%
  ungroup()

# rec_by_side$num_rec %>% unique()
rec_by_side <-
  rec_by_side %>%
  mutate(concept = case_when(
    num_rec == 1 ~ `1`,
    num_rec == 2 ~ str_c(`1`, "-", `2`),
    num_rec == 3 ~ str_c(`1`, "-", `2`, "-", `3`),
    num_rec == 4 ~ str_c(`1`, "-", `2`, "-", `3`, "-", `4`)))


    
rec_by_side <- 
  left_join(rec_by_side, select(pbp, game_id, event_id, off = offensive_team, def = defensive_team, epa, hash, coverage_scheme), by = c("game_id", "event_id"))    

rec_by_side <- left_join(rec_by_side, skill_pos_players %>% filter(on_field_position == "QB") %>% select(game_id, event_id, QB = name), by = c("game_id", "event_id"))


rec_by_side$hash %>% unique()
rec_by_side$side_of_center %>% unique()

rec_by_side <-
  rec_by_side %>%
  mutate(concept_field = case_when(
         (hash == 1 & side_of_center == 'L') | (hash == 3 & side_of_center == 'R')  ~ "B",
         (hash == 1 & side_of_center == 'R') | (hash == 3 & side_of_center == 'L')~ "F",
         TRUE ~ "M"))


concept_play <-
  rec_by_side %>% 
  select(game_id, event_id, target_side, num_blockers, epa, hash, coverage_scheme, QB, off, def, side_of_center, concept, num_rec, concept_field) %>%
  pivot_wider(id_cols = c(game_id, event_id, target_side, num_blockers, epa, hash, coverage_scheme, QB, off, def), names_from = "side_of_center", values_from = c(concept, num_rec, concept_field))

  
concept_play <-
  concept_play %>%
  mutate(across(starts_with("concept"), ~if_else(is.na(.x), "none", .x)),
         across(starts_with("num_rec"), ~if_else(is.na(.x), 0L, .x)))
  
target_concept_play <-
  concept_play %>%
  filter(target_side != "no_pass") %>%
  mutate(concept = case_when(
    target_side == "L" ~ concept_L,
    target_side == "R" ~ concept_R,
    target_side == "B" ~ concept_B
  )) %>%
  select(-c(concept_L, concept_R, concept_B))
  
  
library(lme4)
library(broom.mixed)


mixed = lmer(data = concept_play, epa ~ (1|concept_L) + (1|concept_R) + (1|concept_B) + (1|coverage_scheme) + (1|QB) + (1|def))
  
summary(mixed)


target_concept_mixed = lmer(data = target_concept_play, epa ~ (1|concept) + (1|coverage_scheme) + (1|QB) + (1|def))
summary(target_concept_mixed)

tcm_tidy <- 
  broom.mixed::tidy(target_concept_mixed, effects = "ran_vals") %>%
  filter(group == "concept") %>%
  arrange(desc(estimate))

tcm_tidy <-
  tcm_tidy %>%
  mutate(num_rec = str_count(level, "-") + 1)


top_ten <- tcm_tidy %>%
  group_by(num_rec) %>%
  arrange(desc(estimate)) %>%
  slice(1:5) %>%
  mutate(level = fct_reorder(level, estimate)) %>%
  ungroup()


library(forcats)
top_ten %>%
  ggplot(aes(x = estimate, y = level)) +
  geom_point() +
  geom_linerange(aes(xmin = estimate - 2*std.error, xmax = estimate + 2*std.error)) +
  facet_grid()
  
  


target_concept_play$concept %>% unique() %>% length()













  
skill_pos_players$roster_position %>% unique()

personnel <- 
  skill_pos_players %>%
  group_by(game_id, event_id) %>%
  count(roster_position) %>%
  pivot_wider(names_from = roster_position, names_prefix = 'num_', values_from = n) %>%
  mutate(across(starts_with("num_"), ~replace_na(., 0)))

