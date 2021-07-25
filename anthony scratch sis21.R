source('https://github.com/ajreinhard/data-viz/raw/master/ggplot/plot_SB.R')

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
  left_join(route_mapping) %>% 
  filter(SideOfCenter != 'NULL') %>% 
  group_by(GameID, EventID, SideOfCenter) %>% 
  arrange(Order_OutsideToInside) %>% 
  summarise(
    side_players = n(),
    route_combo = paste(RouteGroup, collapse = ' | '),
    .groups = 'drop'
  )

# get motion, RR, personnel extracted from the skill_df and join to play_side_sum_df from above
play_sum_df <- skill_df %>% 
  mutate(
    # A few safties are in on kneel downs
    RosterPosition = ifelse(RosterPosition == 'S', 'RB', RosterPosition),
    RosterPosition = ifelse(RosterPosition %in% c('DT','DE'), 'FB', RosterPosition),
    RosterPosition = ifelse(RosterPosition %in% c('T','G','C'), 'TE', RosterPosition),
    # Make Taysom Hill a TE when he is not aligned at QB
    RosterPosition = ifelse(PlayerId == 4535 & OnFieldPosition != 'QB', 'TE', RosterPosition)
  ) %>% 
  group_by(GameID, EventID) %>% 
  summarise(
    motion = max(FastMotion),
    route_runners = sum(ifelse(!Route %in% c('NULL', 'Run Fake', 'Blocking'), 1, 0)),
    pers_RB = sum(ifelse(RosterPosition %in% c('FB','RB'), 1, 0)),
    pers_TE = sum(ifelse(RosterPosition == 'TE', 1, 0)),
    pers = paste0(pers_RB, pers_TE),
    .groups = 'drop'
  ) %>% 
  left_join(
    play_side_sum_df %>% 
      pivot_wider(names_from = SideOfCenter, values_from = c(side_players, route_combo)) %>% 
      rowwise %>% 
      mutate(rec_align = paste0(max(side_players_R, side_players_L), 'x', min(side_players_R, side_players_L)))
    )

# alignment freq on pass plays
pass_plays_df %>% 
  inner_join(play_sum_df) %>% 
  group_by(rec_align) %>% 
  summarise(n = n())

# most common route combos
pass_plays_df %>% 
  inner_join(play_side_sum_df) %>% 
  group_by(side_players, route_combo) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  group_by(side_players) %>% 
  filter(row_number() <= 10) %>% 
  ungroup %>% 
  arrange(side_players) %>% 
  view



pass_plays_df %>% 
  filter(RPO == 1) %>% 
  group_by(OffensiveTeam) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  view



# adot and count by drop type
pass_plays_df %>% 
  group_by(DropType, sg = ifelse(Shotgun == 1, 'Shotgun', 'UnderCtr')) %>% 
  summarise(
    n = n(),
    adot = mean(as.numeric(ThrowDepth), na.rm = T),
    .groups = 'drop'
  ) %>% 
  pivot_wider(names_from = sg, values_from = c(n,adot))

# route metrics
skill_df %>% 
  group_by(Route) %>% 
  summarise(
    routes = n(),
    targs = sum(Target),
    catch = sum(Reception),
    targ_rt = targs / routes,
    comp_rt = catch / routes
  ) %>% 
  arrange(-targ_rt) %>% 
  filter(routes >= 1000) %>% 
  view


pass_plays_df %>% 
  left_join(play_sum_df) %>% 
  filter(Down < 3 & !(SideOfField == 'Oppo' & StartYard == ToGo)) %>% 
  mutate(
    drop_group = case_when(
      DropType == '0/1 Step' ~ 'QuickGame',
      DropType == '3 Step' | (DropType == '5 Step' & Shotgun == 0) ~ 'MidDrop',
      DropType == '7 Step' | (DropType == '5 Step' & Shotgun == 1) ~ 'DeepDrop',
      RPO == 1 ~ 'RPO',
      grepl('Rollout', DropType) ~ 'Rollout',
      grepl('Basic Screen', DropType) ~ 'Screen',
      T ~ 'Other'
    )
  ) %>% 
  group_by(drop_group) %>% 
  summarise(n = n())


skill_df %>% 
  inner_join(pass_plays_df) %>% 
  filter(OnFieldPosition != 'B' & !Route %in% c('NULL', 'Run Fake', 'Blocking') & !grepl('Screen', DropType) & !grepl('Screen', Route)) %>% 
  group_by(RosterPosition) %>% 
  summarise(
    routes = n(),
    targs = sum(Target),
    catch = sum(Reception),
    adot = mean(ifelse(Target == 1, as.numeric(ThrowDepth), NA), na.rm = T),
    yards = sum(ifelse(Reception == 1, OffensiveYardage, 0)),
    targ_rt = targs / routes,
    comp_rt = catch / routes,
    comp_pct = catch / targs,
    yprr = yards / routes
  )

skill_df %>% 
  inner_join(pass_plays_df) %>% 
  filter(OnFieldPosition != 'B' & RosterPosition == 'RB' & !Route %in% c('NULL', 'Run Fake', 'Blocking') & !grepl('Screen', DropType) & !grepl('Screen', Route)) %>% 
  group_by(Name, OffensiveTeam) %>% 
  summarise(
    games = n_distinct(GameID),
    routes = n(),
    targs = sum(Target),
    catch = sum(Reception),
    adot = mean(ifelse(Target == 1, as.numeric(ThrowDepth), NA), na.rm = T),
    yards = sum(ifelse(Reception == 1 & SackOnPlay == 0, OffensiveYardage, 0)),
    targ_rt = targs / routes,
    comp_rt = catch / routes,
    comp_pct = catch / targs,
    yprr = yards / routes
  ) %>% 
  arrange(-routes)


skill_df %>% 
  inner_join(pass_plays_df) %>% 
  filter(OnFieldPosition != 'B' & RosterPosition == 'RB' & !Route %in% c('NULL', 'Run Fake', 'Blocking') & !grepl('Screen', DropType) & !grepl('Screen', Route) & Name == 'J.D. McKissic') %>%
  view


pbp_df <- read.csv('Data/PlayByPlay.csv', stringsAsFactors = F) %>% tibble
play_id_lookup <- readRDS('fact_fastRlookup.rds')
fastR_pbp <- load_pbp(2020)

pbp_df %>% 
  left_join(play_id_lookup) %>% 
  left_join(fastR_pbp) %>% 
  filter(pass == 1) %>% 
  group_by(CoverageScheme) %>% 
  mutate(n = n()) %>% 
  ungroup %>% 
  arrange(-n) %>% 
  filter(n >= 300) %>% 
  mutate(CoverageScheme = factor(CoverageScheme, rev(unique(CoverageScheme)))) %>% 
  ggplot(aes(x = xpass, y = CoverageScheme)) +
  geom_density_ridges(bandwidth = 0.01, scale = 1.4) +
  scale_x_continuous(limits = c(0,1), expand = expansion(add = 0))



pbp_df %>% 
  left_join(play_id_lookup) %>% 
  left_join(fastR_pbp) %>% 
  filter(pass == 1) %>% 
  group_by(CoverageScheme, down) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  arrange(-n) %>% 
  pivot_wider(names_from = down, values_from = n)



skill_df %>% 
  inner_join(pass_plays_df) %>% 
  group_by(Route) %>% 
  summarise(
    routes = n(),
    targs = sum(Target),
    catch = sum(Reception),
    adot = mean(ifelse(Target == 1, as.numeric(ThrowDepth), NA), na.rm = T),
    yards = sum(ifelse(Reception == 1, OffensiveYardage, 0)),
    targ_rt = targs / routes,
    comp_rt = catch / routes,
    comp_pct = catch / targs,
    yprr = yards / routes
  ) %>% 
  write.csv('route_mapping.csv', row.names = F)



