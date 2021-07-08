route_comb_df <- pass %>%
  filter(Route != "NULL") %>%
  select(GameID, EventID, Route) %>%
  pivot_wider(names_from = Route, values_from = Route, values_fn = length, values_fill = 0,
              names_prefix = "Route_") %>%
  inner_join(pass) %>%
  select(GameID, EventID, Route, starts_with("Route_"), EPA) %>%
  filter(Route != "NULL") %>%
  pivot_longer(starts_with("Route_"), names_to = "TeammateRoute", values_to = "TeammateRouteCount") %>%
  mutate(TeammateRoute = gsub("Route_", "", TeammateRoute),
         TeammateRouteCount = TeammateRouteCount - ifelse(Route == TeammateRoute, 1, 0)) %>%
  group_by(Route, TeammateRoute) %>%
  summarize(freq = sum(ifelse(TeammateRouteCount > 0, 1, 0)))

route_comb_df %>%
  ggplot(aes(x = Route, y = TeammateRoute, fill = freq)) + 
  geom_tile(color = "black", stat = "identity") +
  theme_minimal() +
  scale_fill_gradient(low = "white", high = "orange")
