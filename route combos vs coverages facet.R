source('https://github.com/ajreinhard/data-viz/raw/master/ggplot/plot_SB.R')

pbp_df <- read.csv('Data/PlayByPlay.csv', stringsAsFactors = F) %>% tibble
skill_df <- read_csv('Data/SkillPositionPlayers.csv') %>% tibble
info_df <- read_csv('Data/GameInfo.csv') %>% tibble
route_mapping <- read_csv('route_mapping.csv')

# take only regular pass plays
pass_plays_df <- pbp_df %>% 
  filter(grepl('pass', EventType) & !DropType %in% c('Spike','Other','RB/WR Pass', 'WR Reverse Pass', 'Double Reverse Pass', 'Flea Flicker') & !(GameID == 2965 & OffensiveTeam == 'Broncos'))

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


p <- pass_plays_df %>% 
  inner_join(play_side_sum_df) %>% 
  filter(side_players == 2 & !CoverageScheme %in% c('Cover 0','Screen','Cover 6','Man Cover 2', 'Tampa 2', 'Prevent') & !grepl('None', route_combo)) %>% 
  group_by(route_combo, CoverageScheme) %>% 
  summarise(
    n = n(),
    epa = mean(as.numeric(EPA), na.rm = T),
    .groups = 'drop'
  ) %>% 
  group_by(route_combo) %>%
  filter(sum(n) >= 150 & n >= 10) %>% 
  ungroup %>% 
  ggplot(aes(x = epa, y = CoverageScheme, color = CoverageScheme)) +
  facet_wrap(. ~ route_combo, nrow = 4, scales = 'free') +
  annotation_custom(make_gradient(deg = 270), ymin=4.2, ymax=Inf, xmin=-Inf, xmax=Inf) +
  annotation_custom(make_gradient(deg = 0), ymin=-Inf, ymax=Inf, xmin=0.6, xmax=Inf) +
  annotation_custom(make_gradient(deg = 180), ymin=-Inf, ymax=Inf, xmin=-Inf, xmax=-0.4) +
  geom_shadowtext(aes(label = gsub('Cover', 'Cov', CoverageScheme), x = ifelse(epa < 0, 0.07, -0.07), hjust = ifelse(epa < 0, 0, 1)), family = font_SB, bg.r = 0.2, bg.color = 'white', size = 2, show.legend = F) +
  geom_segment(aes(xend = 0, yend = CoverageScheme), color = 'grey20', size = 1.5, lineend = 'round', show.legend = F) +
  geom_segment(aes(xend = 0, yend = CoverageScheme), size = 1, lineend = 'round', show.legend = F) +
  geom_point(aes(size = n, fill = CoverageScheme), shape = 21, color = 'grey20') +
  scale_x_continuous(limits = c(-0.8,0.8), breaks = seq(-0.9,0.9,0.3), labels = plus_lab_format(accuracy = 0.1)) +
  scale_size(range = c(1,4)) +
  labs(title = 'Two-Man Single-Side Route Combos vs Common Coverages',
       subtitle = 'Includes targeted and non-targeted plays  |  Screens excluded  |  Inside route first  |  min. 150 plays',
       x = 'EPA/Play',
       y = NULL,
       size = 'Plays',
       fill = 'Coverage') +
  theme_SB +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.border = element_rect(size = 0.1, color = 'grey95'),
    axis.line.x = element_line(size = 0.5, color = 'darkblue'),
    legend.position = c(1,-0.08),
    legend.justification = c(1,0),
    legend.key.height = unit(1, 'lines'),
    legend.key.width = unit(0, 'lines'),
    legend.margin = margin(t = 3, r = 3, b = 3, l = 3, unit = "pt"),
    legend.box = 'horizontal',
    legend.spacing.x = unit(0.2, 'lines')
  )

brand_plot(p, 'two man epa.png', asp = 16/9, data_home = 'Data: SIS')
