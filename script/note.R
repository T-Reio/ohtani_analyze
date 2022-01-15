pitch2021 %>%
  filter(type == 'S', strikes == 0, balls == 0) %>%
  view()

pitch2021$bb_type %>% unique()

pitch2021 %>%
  filter(bb_type == "", type == 'X') %>%
  head(30) %>% view()

pitch2021$bb_type %>% unique()
