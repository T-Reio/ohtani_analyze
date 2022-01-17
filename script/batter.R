library(tidyverse)
library(baseballr)
source('library/event_lists.R')

#月別xwOBA


#----- Zone ----------

names <- chadwick %>%
  select(batter_name_last = name_last, batter_name_first = name_first, key_mlbam)

cf %>%
  left_join(., names, by = c('batter' = 'key_mlbam')) -> cf

called_pitch <- cf %>%
  filter(description %in% c("ball", "called_strike"))

called_pitch %>%
  mutate(
    Strike = if_else(type == 'S', 1, 0),
    Zone = if_else(zone %in% 1:9, 1, 0),
    Zone_individual = if_else(
      (plate_x <= 0.85 & plate_x >= -0.85 & 
         plate_z >= sz_bot & plate_z <= sz_top),
      1, 0
    ),
    expected_run = (1 - prob) * if_ball + prob * if_strike,
  ) %>%
  group_by(batter) %>%
  summarise(
    name = paste0(first(batter_name_last), ', ' , first(batter_name_first)),
    total = n(),
    diff = sum(Strike - prob, na.rm = T),
    type1 = sum(Zone_individual - Strike == 1, na.rm = T),
    type2 = sum(Zone_individual - Strike == -1, na.rm = T),
    runs = sum(delta_run_exp_tj - expected_run, na.rm = T),
  ) %>%
  filter(total >= 500) %>%
  mutate(type2_ratio = type2 / total * 100) %>%
  arrange(-runs) %>%
  mutate(rank = row_number()) -> called_ranking

called_ranking

write_excel_csv(called_ranking, 'output/table/pitchcall_runs_ranking_min500.csv')
