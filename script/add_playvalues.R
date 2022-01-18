library(tidyverse)
source('library/event_lists.R')

#-----Add Value----------

count_values <- read_rds('output/counts_run_value_1821.rds') %>%
  select(-strikes, -balls)

pitch2021 %>%
  mutate(
    balls = if_else(balls == 4, 3, balls),
    Count = paste0(balls, '-', strikes)
  ) -> pitch2021

pitch2021 <- pitch2021 %>%
  inner_join(count_values, by = 'Count')

pitch2021 %>%
  mutate(
    swing_indicator = if_else(!(description %in% c("called_strike", "blocked_ball", "ball", "hit_by_pitch", "intent_ball", "pitchout")), 1, 0)
  ) -> pitch2021

play_values <- read_rds('output/play_values_1821.rds') %>%
  bind_rows(., tibble(events = NA_character_, mean_run_value = NA)) %>%
  rename(play_result = events, rv_bb = mean_run_value)

{
  fouls <- c('foul', 'foul_pitchout')
  balls_key <- c('ball', 'ball_blocked', 'blocked_ball', 'pitchout')
  strikes_key <- c('swinging_strike', 'called_strike', 'swinging_strike_blocked', 'foul_tip', 'foul_bunt', 'missed_bunt', 'swinging_pitchout', "bunt_foul_tip")
  
  events_literal <- c('strikeout', 'field_error', 'walk', 'intent_walk',
                      'hit_by_pitch', 'catcher_interf', 'field_error', 'fielders_choice',
                      'single', 'double', 'triple', 'home_run')
  field_outs_key <- c('force_out', 'grounded_into_double_play', 'sac_fly', 'sac_bunt', 'field_out','sac_fly_double_play', 'triple_play', 'double_play', 'sac_bunt_double_play')
  }

pitch2021 <- pitch2021 %>%
  mutate(
    #pitch_result = case_when(
    #  description %in% fouls ~ 'foul',
    #  description %in% balls_key ~ 'ball',
    #  description %in% strikes_key ~ 'strike',
    #  T ~ 'other',
    #),
    play_result = case_when(
      events %in% events_literal ~ events,
      events %in% field_outs_key ~ 'field_out',
      events == 'strikeout_double_play' ~ 'strikeout',
      events == 'fielders_choice_out' ~ 'fielders_choice',
      T ~ NA_character_
    ),
  )

pitch2021 <- pitch2021 %>%
  left_join(., play_values, by = 'play_result')

pitch2021 %>%
  mutate(
    delta_run_exp_tj = case_when(
      !is.na(rv_bb) ~ rv_bb - rv_before,
      (is.na(rv_bb) & description %in% strikes_key) ~ if_strike,
      (is.na(rv_bb) & description %in% balls_key) ~ if_ball,
      (is.na(rv_bb) & description %in% fouls) ~ if_foul,
      T ~ NA_real_
    )
  ) -> pitch2021
