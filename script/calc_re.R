library(tidyverse)

pbp %>%
  dplyr::mutate(
    RUNS = AWAY_SCORE_CT + HOME_SCORE_CT,
    HALF.INNING = paste(GAME_ID, INN_CT, BAT_HOME_ID),
    RUNS.SCORED = 
      (BAT_DEST_ID > 3) + (RUN1_DEST_ID > 3) + (RUN2_DEST_ID > 3) + (RUN3_DEST_ID > 3),
    #0->走者がいない、打者アウト 1, 2, 3, 4 -> 行き先のベース: 4で得点
  ) -> pbp

pbp %>%
  dplyr::group_by(HALF.INNING) %>%
  dplyr::summarise(
    Outs.Inning = sum(EVENT_OUTS_CT),
    Runs.Inning = sum(RUNS.SCORED),
    Runs.Start = dplyr::first(RUNS),
    MAX.RUNS = Runs.Inning + Runs.Start #そのイニング終了時の合計得点数
  ) -> half_innings

pbp %>%
  dplyr::inner_join(half_innings, by = 'HALF.INNING') %>%
  dplyr::mutate(RUNS.ROI = MAX.RUNS - RUNS) -> pbp 
#RUNS: イニング途中含めた得点
#RUNS.ROI: そのイニングの残りで入る得点

pbp %>%
  dplyr::mutate(
    BASES = paste0(
      dplyr::if_else(!is.na(BASE1_RUN_ID), '1', '_'),
      dplyr::if_else(!is.na(BASE2_RUN_ID), '2', '_'),
      dplyr::if_else(!is.na(BASE3_RUN_ID), '3', '_')
    ),
    STATE = paste(BASES, OUTS_CT)
  ) -> pbp

pbp %>%
  dplyr::mutate(
    NRUNNER1 = dplyr::if_else((RUN1_DEST_ID == 1 | BAT_DEST_ID == 1), '1', '_'),
    NRUNNER2 = dplyr::if_else((RUN1_DEST_ID == 2 | RUN2_DEST_ID == 2 | BAT_DEST_ID == 2), '2', '_'),
    NRUNNER3 = dplyr::if_else((RUN1_DEST_ID == 3 | RUN2_DEST_ID == 3 | 
                                 RUN3_DEST_ID == 3 | BAT_DEST_ID == 3), '3', '_'),
    NOUTS = OUTS_CT + EVENT_OUTS_CT, #outs before play + outs on play
    NEW.BASES = paste0(NRUNNER1, NRUNNER2, NRUNNER3),
    NEW.STATE = paste(NEW.BASES, NOUTS)
  ) -> pbp

pbp %>%
  dplyr::filter((STATE != NEW.STATE) | (RUNS.SCORED > 0)) -> pbp_changed

pbp_changed %>%
  dplyr::filter(Outs.Inning == 3) %>%
  filter(INN_CT >= 10) -> pbpC

pbpC %>%
  dplyr::group_by(STATE) %>%
  dplyr::summarise(Mean = round(mean(RUNS.ROI), 2)) %>%
  dplyr::mutate(
    runners_on = stringr::str_sub(STATE, 1, 3),
    Outs = stringr::str_sub(STATE, 5, 5)
  ) %>%
  dplyr::arrange(Outs) -> RUNS

RUNS
RUNS %>%
  dplyr::select(-STATE) %>%
  tidyr::spread(., key = Outs, value = Mean, sep = ': ') %>%
  dplyr::mutate_at(.vars = vars('Outs: 0', 'Outs: 1', 'Outs: 2'), .funs = formatC, digits = 2, format = 'f') -> value_long

value_long

#-----Runs Value---------

pbp %>%
  dplyr::left_join(RUNS %>% dplyr::select(c(STATE, Mean)), by = 'STATE') %>% #打席前の期待得点
  dplyr::rename(Runs.State = Mean) %>%
  dplyr::left_join(RUNS %>% dplyr::select(c(STATE, Mean)), by = c('NEW.STATE' = 'STATE')) %>%
  dplyr::rename(Runs.New.State = Mean) %>%
  tidyr::replace_na(list(Runs.New.State = 0)) %>%
  dplyr::mutate(
    run_value = Runs.New.State - Runs.State + RUNS.SCORED
  ) -> pbp

#setwd('C:/Users/easyu/ballgame_discrimination')
#tracks <- read_rds("input/master_files/master_1618.rds")

pbp %>%
  filter(BAT_EVENT_FL == T) %>%
  filter(EVENT_CD %in% c(2, 3, 13:23)) %>%
  mutate(
    events = case_when(
      EVENT_CD == 2 ~ 'field_out', 
      #force_out grounded_into_double_play, sac_fly sac_bunt, sac_fly_double_play, triple_play sac_bunt_double_play
      EVENT_CD == 3 ~ 'strikeout', #strikeout_double_play
      EVENT_CD == 13 ~ 'field_error',
      EVENT_CD == 14 ~ 'walk',
      EVENT_CD == 15 ~ 'intent_walk',
      EVENT_CD == 16 ~ 'hit_by_pitch',
      EVENT_CD == 17 ~ 'catcher_interf',
      EVENT_CD == 18 ~ 'field_error',
      EVENT_CD == 19 ~ 'fielders_choice', #fielders_choice_out
      EVENT_CD == 20 ~ 'single',
      EVENT_CD == 21 ~ 'double',
      EVENT_CD == 22 ~ 'triple',
      EVENT_CD == 23 ~ 'home_run',
    )
  ) -> pbpb

pbpb %>%
  group_by(events) %>%
  summarise(
    freq = n(),
    mean_run_value = mean(run_value),
    max_run_value = max(run_value),
    min_run_value = min(run_value),
    sd_run_value = sd(run_value),
  )

play_run_value <- pbpb %>%
  group_by(events) %>%
  summarise(
    mean_run_value = mean(run_value),
  )

#setwd('C:/Users/easyu/ballgame_discrimination')
#write_rds(play_run_value, 'input/master_files/plays_runvalue_1519.rds')
#write_rds(value_long, 'input/master_files/RE24_1519.rds')

#------Count---------

pitchcount <- pbpb %>%
  #filter()
  dplyr::mutate(pseq = stringr::str_remove_all(PITCH_SEQ_TX, pattern = '[.<>123N+*]'))

b <- '[BIPV]'
s <- '[CFKLMOQRST]'

pitchcount <- pitchcount %>%
  dplyr::mutate(
    c00 = T,
    c10 = stringr::str_detect(pseq, paste0('^', b)),
    c01 = stringr::str_detect(pseq, paste0('^', s)),
    c20 = stringr::str_detect(pseq, paste0('^', b, '{2}')), #ボールを2回繰り返す
    c02 = stringr::str_detect(pseq, paste0('^', s, '{2}')),
    c30 = stringr::str_detect(pseq, paste0('^', b, '{3}')),
    c11 = stringr::str_detect(pseq, paste0('^', s, b, #先頭一致、ストライク→ボールかその逆
                                           '|', b, s)),
    c21 = stringr::str_detect(pseq, paste0('^', s, b, b,
                                           '|', b, s, b,
                                           '|', b, b, s)),
    c31 = stringr::str_detect(pseq, paste0('^', s, b, b, b,
                                           '|', b, s, b, b,
                                           '|', b, b, s, b, 
                                           '|', b, b, b, s)),
    c12 = stringr::str_detect(pseq, paste0('^', b, s, s,
                                           '|', s, b, s, #3球で1-2になった場合
                                           '|', s, s, '[FR]*', b #2-0からファウル挟んでボール
    )),
    c22 = stringr::str_detect(pseq, paste0('^', b, b, s, s,
                                           '|', b, s, b, s,
                                           '|', s, b, b, s,
                                           '|', s, b, s, '[FR]*', b,
                                           '|', b, s, s, '[FR]*', b,
                                           '|', s, s, '[FR]*', b, '[FR]*', b)),
    c32 = stringr::str_detect(pseq, paste0('^', s, '*', b, s, '*', b, s, '*', b)) |
      #ボールとストライクが交互に続くパターン
      stringr::str_detect(pseq, paste0('^', b, '*', s, b, '*', 's')), #先に3ボールになるパターン
  )

pitchcount %>%
  dplyr::select(GAME_ID, EVENT_ID, run_value, dplyr::starts_with('c')) %>%
  head()

pbp_counts <- pitchcount %>%
  dplyr::select(starts_with('c'), RUNS.VALUE = run_value)
pbp_counts

pbp_counts_tidy <- pbp_counts %>%
  tidyr::gather(key = 'count', value = 'passed_thru', -RUNS.VALUE)

pbp_counts_tidy

run_value_by_count <- pbp_counts_tidy %>%
  dplyr::filter(passed_thru == 1) %>%
  dplyr::group_by(count) %>%
  dplyr::summarise(
    N = n(),
    value = mean(RUNS.VALUE)
  ) %>%
  dplyr::mutate(balls = as.numeric(stringr::str_sub(count, 2, 2)),
                strikes = as.numeric(stringr::str_sub(count, 3, 3)),) %>%
  select(-1)

run_value_by_count

counts <- expand.grid(balls = as.numeric(0:3), strikes = as.numeric(0:2), 
                      call = c('if_strike', 'if_ball', 'if_foul')) %>%
  mutate(
    balls_after = if_else(condition = call == 'if_ball', true = 1, false = 0) + balls,
    strikes_after = case_when(
      call == 'if_strike' ~ strikes + 1,
      (call == 'if_foul' & strikes <= 1) ~ strikes + 1,
      T ~ strikes
    )
  )
counts

counts %>%
  left_join(., run_value_by_count, by = c('balls', 'strikes')) %>%
  rename(rv_before = value) %>%
  left_join(., run_value_by_count, by = c('balls_after' = 'balls', 'strikes_after' = 'strikes')) %>%
  select(1, 2, 3, 7, rv_after = 9) %>%
  mutate(
    rv_after = if_else(is.na(rv_after), if_else(call == 'if_strike', -.266, .310), rv_after),
    runs_value = rv_after - rv_before
  ) -> call_values

call_values

call_values %>%
  select(-c(4, 5)) %>%
  pivot_wider(., names_from = call, values_from = runs_value) %>%
  mutate(Count = paste0(balls, '-', strikes)) -> call_values_saved