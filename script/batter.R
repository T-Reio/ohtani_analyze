library(tidyverse)
library(baseballr)
library(lubridate)
library(REdaS)
source('library/event_lists.R')
source('library/colour_palette.R')

fg_stats <- baseballr::fg_batter_leaders(2021, 2021)

fg_stats %>%
  select(playerid, Team, IBB) -> fg_selected

#月別xwOBA

names <- chadwick %>%
  select(batter_name_last = name_last, batter_name_first = name_first, key_mlbam, key_fangraphs)

pitch2021 %>%
  left_join(., names, by = c('batter' = 'key_mlbam')) -> pitch2021

pitch2021$pitch_type %>% unique()

pitch2021 %>%
  mutate(
    pitch_type_forBatter = case_when(
      pitch_type %in% c('FF', 'SI', 'FC') ~ 'Fastball',
      pitch_type %in% c('SL', 'KC', 'CU', 'CS') ~ 'Breaking',
      pitch_type %in% c('CH', 'FS', 'SC') ~ 'Offspeed',
      T ~ 'Others'
    ),
    base_hits = factor(
      dplyr::case_when(
        events == 'single' ~ 'Single',
        events == 'double' ~ 'Double',
        events == 'triple' ~ 'Triple',
        events == 'home_run' ~ 'HR',
        events == 'field_error' ~ 'Error',
        TRUE ~ 'Out'
      ), 
      levels = c('HR', 'Triple', 'Double', 'Single', 'Error', 'Out')
    ),
    Barrel = if_else(launch_speed_angle == 6, 1, 0),
    launch_speed_km = launch_speed * 1.61,
    PA_denom =if_else(events %in% PAresult, 1, 0),
    BABIP_denom =if_else(events %in% BABIP_den, 1, 0),
    Season_Half = if_else(game_date <= '2021-07-01', '1st', '2nd'),
    spray_angle = rad2deg(atan((hc_x - 125.42)/(198.27 - hc_y))),
    mirror_spray = if_else(stand == 'R', -spray_angle, spray_angle),
    bb_dim_type = case_when(
      mirror_spray >= 15 ~ "Pull",
      mirror_spray >= -15 & mirror_spray <= 15 ~ "Cent",
      mirror_spray <= -15 ~ "Oppo",
      TRUE ~ NA_character_
    )
  ) -> pitch2021

pitch2021 %>%
  mutate(
    pfx_x_mirror = if_else(p_throws == 'L', -pfx_x, pfx_x),
    xwoba_value = if_else(is.na(estimated_woba_using_speedangle), woba_value, estimated_woba_using_speedangle),
  )%>%
  arrange(game_pk,-at_bat_number,-pitch_number) -> pitch2021

#----- League ----------

pitch2021 %>%
  group_by(batter) %>%
  summarise(
    name = paste0(first(batter_name_last), ', ' , first(batter_name_first)),
    key_fangraphs = as.character(first(key_fangraphs)),
    pitches = n(),
    Fastball_pct = round(mean(pitch_type_forBatter == 'Fastball', na.rm = T) * 100, digits = 2),
    Zone = round(sum(zone %in% 1:9)/n() * 100, digits = 1),
    CStr = round(sum(description %in% calledstr) / n() * 100, digits = 1),
    Swing = round(sum(description %in% swing) / n() * 100, digits = 1),
    Contact = round(sum(description %in% contact, na.rm = T) / 
                        sum(description %in% swing, na.rm = T) * 100, digits = 1),
    foul_pct = round(sum(description %in% c('foul', 'foul_pitchout', 'foul_bunt')) / 
                         sum(description %in% contact, na.rm = T) * 100, digits = 1),
    exit_velo = round(mean(launch_speed[type == 'X'], na.rm = T), digits = 1),
    launch_angle = round(mean(launch_angle[type == 'X'], na.rm = T), digits = 1),
    Pull = round(sum(bb_dim_type[type == 'X'] == "Pull", na.rm = T) /
                   sum(type == 'X', na.rm = T) * 100, digits = 1),
    Hard = round(sum(launch_speed[type == 'X'] >= 95, na.rm = T) /
                     sum(type == 'X', na.rm = T) * 100, digits = 1),
    BABIP = round(sum(babip_value, na.rm = T) / sum(BABIP_denom, na.rm = T), digits = 3),
    wOBA = round(sum(woba_value, na.rm = T) / sum(woba_denom, na.rm = T), digits = 3),
    xwOBA = round(sum(xwoba_value, na.rm = T) / sum(woba_denom, na.rm = T), digits = 3),
    flare = round(sum(launch_speed_angle == 4, na.rm = T) /
                      sum(!is.na(launch_speed_angle), na.rm = T) * 100, digits = 1),
    barrel = round(sum(Barrel == 1, na.rm = T) /
                       sum(!is.na(Barrel), na.rm = T) * 100, digits = 1),
    barrelperPA = round(sum(Barrel == 1, na.rm = T) /
                            sum(PA_denom, na.rm = T) * 100, digits = 1),
    shifted = round(sum(if_fielding_alignment != 'Standard' & type == 'X', na.rm = T) / sum(type == 'X', na.rm = T) * 100, digits = 1),
  ) %>%
  ungroup() -> league_individual

league_individual %>%
  right_join(., fg_selected, by = c('key_fangraphs' = 'playerid')) %>%
  arrange(-wOBA) %>%
  select(-c(batter, key_fangraphs, pitches)) %>%
  select(name, Team, everything()) -> league

#write_excel_csv(league, 'output/table/bat_individual_qual.csv')

#----- Ohtani --------

ohtani <- pitch2021 %>%
  filter(batter == 660271) %>%
  mutate(game_month = month(game_date))

#----- Season Total -------

ohtani %>%
  #group_by(Season_Half) %>%
  summarise(
    pitches = n(),
    Fastball_pct = round(mean(pitch_type_forBatter == 'Fastball', na.rm = T) * 100, digits = 2),
    Zone = round(sum(zone %in% 1:9)/n() * 100, digits = 1),
    CStr = round(sum(description %in% calledstr) / n() * 100, digits = 1),
    Swing = round(sum(description %in% swing) / n() * 100, digits = 1),
    Contact = round(sum(description %in% contact, na.rm = T) / 
                        sum(description %in% swing, na.rm = T) * 100, digits = 1),
    foul_pct = round(sum(description %in% c('foul', 'foul_pitchout', 'foul_bunt')) / 
                         sum(description %in% contact, na.rm = T) * 100, digits = 1),
    exit_velo = round(mean(launch_speed[type == 'X'], na.rm = T), digits = 1),
    launch_angle = round(mean(launch_angle[type == 'X'], na.rm = T), digits = 1),
    Hard = round(sum(launch_speed >= 95, na.rm = T) /
                     sum(type == 'X', na.rm = T) * 100, digits = 1),
    BABIP = round(sum(babip_value, na.rm = T) / sum(BABIP_denom, na.rm = T), digits = 3),
    wOBA = round(sum(woba_value, na.rm = T) / sum(woba_denom, na.rm = T), digits = 3),
    xwOBA = round(sum(xwoba_value, na.rm = T) / sum(woba_denom, na.rm = T), digits = 3),
    flare = round(sum(launch_speed_angle == 4, na.rm = T) /
                       sum(!is.na(launch_speed_angle), na.rm = T) * 100, digits = 1),
    barrel = round(sum(Barrel == 1, na.rm = T) /
                       sum(!is.na(Barrel), na.rm = T) * 100, digits = 1),
    barrelperPA = round(sum(Barrel == 1, na.rm = T) /
                       sum(PA_denom, na.rm = T) * 100, digits = 1),
    shifted = round(sum(if_fielding_alignment != 'Standard' & type == 'X', na.rm = T) / sum(type == 'X', na.rm = T) * 100, digits = 1),
  ) -> ohtani_total

#write_excel_csv(ohtani_total, 'output/table/Ohtani_bat_total.csv')

#----- Season Half -----------

ohtani %>%
  group_by(Season_Half) %>%
  summarise(
    pitches = n(),
    Fastball_pct = round(mean(pitch_type_forBatter == 'Fastball', na.rm = T) * 100, digits = 2),
    Zone = round(sum(zone %in% 1:9)/n() * 100, digits = 1),
    CStr = round(sum(description %in% calledstr) / n() * 100, digits = 1),
    Swing = round(sum(description %in% swing) / n() * 100, digits = 1),
    Contact = round(sum(description %in% contact, na.rm = T) / 
                        sum(description %in% swing, na.rm = T) * 100, digits = 1),
    foul_pct = round(sum(description %in% c('foul', 'foul_pitchout', 'foul_bunt')) / 
                         sum(description %in% contact, na.rm = T) * 100, digits = 1),
    exit_velo = round(mean(launch_speed[type == 'X'], na.rm = T), digits = 1),
    launch_angle = round(mean(launch_angle[type == 'X'], na.rm = T), digits = 1),
    Hard = round(sum(launch_speed >= 95, na.rm = T) /
                     sum(type == 'X', na.rm = T) * 100, digits = 1),
    BABIP = round(sum(babip_value, na.rm = T) / sum(BABIP_denom, na.rm = T), digits = 3),
    wOBA = round(sum(woba_value, na.rm = T) / sum(woba_denom, na.rm = T), digits = 3),
    xwOBA = round(sum(xwoba_value, na.rm = T) / sum(woba_denom, na.rm = T), digits = 3),
    flare = round(sum(launch_speed_angle == 4, na.rm = T) /
                      sum(!is.na(launch_speed_angle), na.rm = T) * 100, digits = 1),
    barrel = round(sum(Barrel == 1, na.rm = T) /
                       sum(!is.na(Barrel), na.rm = T) * 100, digits = 1),
    barrelperPA = round(sum(Barrel == 1, na.rm = T) /
                            sum(PA_denom, na.rm = T) * 100, digits = 1),
    shifted = round(sum(if_fielding_alignment != 'Standard' & type == 'X', na.rm = T) / sum(type == 'X', na.rm = T) * 100, digits = 1),
  ) -> season_half

#write_excel_csv(season_half, 'output/table/Ohtani_bat_half_season.csv')

#------ by Month -----------

ohtani %>%
  group_by(game_month) %>%
  summarise(
    pitches = n(),
    Fastball_pct = round(mean(pitch_type_forBatter == 'Fastball', na.rm = T) * 100, digits = 2),
    Zone = round(sum(zone %in% 1:9)/n() * 100, digits = 1),
    CStr = round(sum(description %in% calledstr) / n() * 100, digits = 1),
    Swing = round(sum(description %in% swing) / n() * 100, digits = 1),
    Contact = round(sum(description %in% contact, na.rm = T) / 
                      sum(description %in% swing, na.rm = T) * 100, digits = 1),
    foul_pct = round(sum(description %in% c('foul', 'foul_pitchout', 'foul_bunt')) / 
                       sum(description %in% contact, na.rm = T) * 100, digits = 1),
    exit_velo = round(mean(launch_speed[type == 'X'], na.rm = T), digits = 1),
    launch_angle = round(mean(launch_angle[type == 'X'], na.rm = T), digits = 1),
    Hard = round(sum(launch_speed >= 95, na.rm = T) /
                   sum(type == 'X', na.rm = T) * 100, digits = 1),
    BABIP = round(sum(babip_value, na.rm = T) / sum(BABIP_denom, na.rm = T), digits = 3),
    wOBA = round(sum(woba_value, na.rm = T) / sum(woba_denom, na.rm = T), digits = 3),
    xwOBA = round(sum(xwoba_value, na.rm = T) / sum(woba_denom, na.rm = T), digits = 3),
    flare = round(sum(launch_speed_angle == 4, na.rm = T) /
                    sum(!is.na(launch_speed_angle), na.rm = T) * 100, digits = 1),
    barrel = round(sum(Barrel == 1, na.rm = T) /
                     sum(!is.na(Barrel), na.rm = T) * 100, digits = 1),
    barrelperPA = round(sum(Barrel == 1, na.rm = T) /
                          sum(PA_denom, na.rm = T) * 100, digits = 1),
    shifted = round(sum(if_fielding_alignment != 'Standard' & type == 'X', na.rm = T) / sum(type == 'X', na.rm = T) * 100, digits = 1),
  ) -> by_month

#write_excel_csv(by_month, 'output/table/Ohtani_bat_byMonth.csv')

#----- Pitch_Type -----------

ohtani %>%
  group_by(p_throws, pitch_type_forBatter) %>%
  summarise(
    pitches = n(),
    #Fastball_pct = round(mean(pitch_type_forBatter == 'Fastball', na.rm = T) * 100, digits = 2),
    Zone = round(sum(zone %in% 1:9)/n() * 100, digits = 1),
    CStr = round(sum(description %in% calledstr) / n() * 100, digits = 1),
    Swing = round(sum(description %in% swing) / n() * 100, digits = 1),
    Contact = round(sum(description %in% contact, na.rm = T) / 
                        sum(description %in% swing, na.rm = T) * 100, digits = 1),
    foul_pct = round(sum(description %in% c('foul', 'foul_pitchout', 'foul_bunt')) / 
                         sum(description %in% contact, na.rm = T) * 100, digits = 1),
    exit_velo = round(mean(launch_speed[type == 'X'], na.rm = T), digits = 1),
    launch_angle = round(mean(launch_angle[type == 'X'], na.rm = T), digits = 1),
    Hard = round(sum(launch_speed >= 95, na.rm = T) /
                     sum(type == 'X', na.rm = T) * 100, digits = 1),
    BABIP = round(sum(babip_value, na.rm = T) / sum(BABIP_denom, na.rm = T), digits = 3),
    wOBA = round(sum(woba_value, na.rm = T) / sum(woba_denom, na.rm = T), digits = 3),
    xwOBA = round(sum(xwoba_value, na.rm = T) / sum(woba_denom, na.rm = T), digits = 3),
    flare = round(sum(launch_speed_angle == 4, na.rm = T) /
                      sum(!is.na(launch_speed_angle), na.rm = T) * 100, digits = 1),
    barrel = round(sum(Barrel == 1, na.rm = T) /
                       sum(!is.na(Barrel), na.rm = T) * 100, digits = 1),
    barrelperPA = round(sum(Barrel == 1, na.rm = T) /
                            sum(PA_denom, na.rm = T) * 100, digits = 1),
    pv = round(sum(delta_run_exp, na.rm = T), digits = 1),
    pvC = round(mean(delta_run_exp, na.rm = T) * 100, digits = 2),
    pv_tj = round(sum(delta_run_exp_tj, na.rm = T), digits = 1),
    pvC_tj = round(mean(delta_run_exp_tj, na.rm = T) * 100, digits = 2),
  ) %>%
  arrange(p_throws, -pitches) -> pitch_types

#write_excel_csv(pitch_types, 'output/table/pitchtype_platoon.csv')

#------ Pitch Type Summed -----------

ohtani %>%
  group_by(pitch_name) %>%
  summarise(
    pitches = n(),
    #Fastball_pct = round(mean(pitch_type_forBatter == 'Fastball', na.rm = T) * 100, digits = 2),
    Zone = round(sum(zone %in% 1:9)/n() * 100, digits = 1),
    CStr = round(sum(description %in% calledstr) / n() * 100, digits = 1),
    Swing = round(sum(description %in% swing) / n() * 100, digits = 1),
    Contact = round(sum(description %in% contact, na.rm = T) / 
                      sum(description %in% swing, na.rm = T) * 100, digits = 1),
    foul_pct = round(sum(description %in% c('foul', 'foul_pitchout', 'foul_bunt')) / 
                       sum(description %in% contact, na.rm = T) * 100, digits = 1),
    exit_velo = round(mean(launch_speed[type == 'X'], na.rm = T), digits = 1),
    launch_angle = round(mean(launch_angle[type == 'X'], na.rm = T), digits = 1),
    Hard = round(sum(launch_speed >= 95, na.rm = T) /
                   sum(type == 'X', na.rm = T) * 100, digits = 1),
    BABIP = round(sum(babip_value, na.rm = T) / sum(BABIP_denom, na.rm = T), digits = 3),
    wOBA = round(sum(woba_value, na.rm = T) / sum(woba_denom, na.rm = T), digits = 3),
    xwOBA = round(sum(xwoba_value, na.rm = T) / sum(woba_denom, na.rm = T), digits = 3),
    flare = round(sum(launch_speed_angle == 4, na.rm = T) /
                    sum(!is.na(launch_speed_angle), na.rm = T) * 100, digits = 1),
    barrel = round(sum(Barrel == 1, na.rm = T) /
                     sum(!is.na(Barrel), na.rm = T) * 100, digits = 1),
    barrelperPA = round(sum(Barrel == 1, na.rm = T) /
                          sum(PA_denom, na.rm = T) * 100, digits = 1),
    pv = round(sum(delta_run_exp, na.rm = T), digits = 1),
    pvC = round(mean(delta_run_exp, na.rm = T) * 100, digits = 2),
    pv_tj = round(sum(delta_run_exp_tj, na.rm = T), digits = 1),
    pvC_tj = round(mean(delta_run_exp_tj, na.rm = T) * 100, digits = 2),
    shifted = round(sum(if_fielding_alignment != 'Standard', na.rm = T) / n() * 100, digits = 1),
  ) %>%
  arrange(-pitches) -> pitch_type_specified

#write_excel_csv(pitch_type_specified, 'output/table/Ohtani_bat_pitch_type_specified.csv')

#----- Exit velo / Launch Angle plot -------

ohtani <- ohtani %>%
  mutate(
    base_hits = factor(
      dplyr::case_when(
        events == 'single' ~ 'Single',
        events == 'double' ~ 'Double',
        events == 'triple' ~ 'Triple',
        events == 'home_run' ~ 'HR',
        events == 'field_error' ~ 'Error',
        TRUE ~ 'Out'
      ), 
      levels = c('HR', 'Triple', 'Double', 'Single', 'Error', 'Out')
    ),
    Barrel = if_else(launch_speed_angle == 6, 1, 0),
    launch_speed_km = launch_speed * 1.61,
  )

velo_min <- 50
velo_max <- 220
angle_min <- -60
angle_max <- 80

p1 <- ggplot2::ggplot(
  data = ohtani %>% filter(type == 'X'), aes(x = launch_speed_km, y = launch_angle, colour = base_hits)
) +
  ggplot2::geom_point(size = 1, alpha = .7) + ggplot2::theme_bw() +
  ggplot2::ggtitle(paste0("Shohei Ohtani: Batted Ball 2021")) +
  ggplot2::xlab("Exit Velocity (km/h)") + ggplot2::ylab("Launch Angle (°)") +
  ggplot2::geom_hline(yintercept = 0, linetype = 2) + 
  ggplot2::geom_vline(xintercept = 152.95, linetype = 2) +
  ggplot2::scale_colour_manual(values = hit_colours, name = 'Result') +
  ggplot2::xlim(velo_min, velo_max) + ggplot2::ylim(angle_min, angle_max)
p1

#ggsave('output/figure/Ohtani_angle_speed.png', width = 4, height = 3)

p1 +
  facet_wrap(~ Season_Half) -> p2

#ggsave('output/figure/Ohtani_angle_speed_half.png', width = 5, height = 3)

#----- Spray Chart -------

library(ggthemes)
source('library/geom_spraytable.R')

#Total

geom_spraytable(data = ohtani %>% filter(type == 'X'),
                aes(x = hc_x, y = -hc_y, 
                    colour = base_hits)) +
  geom_point(alpha = .7) +
  ggplot2::scale_colour_manual(values = hit_colours, name = 'Result') +
  theme_few() +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank()) +
  labs(title = 'Shohei Ohtani Spray Chart')

#ggsave('output/figure/Ohtani_spray_total.png', width = 5, height = 3)

#---- Season Half-----

geom_spraytable(data = ohtani %>% filter(type == 'X'),
                aes(x = hc_x, y = -hc_y, 
                    colour = base_hits)) +
  geom_point(alpha = .7) +
  ggplot2::scale_colour_manual(values = hit_colours, name = 'Result') +
  theme_few() +
  facet_wrap(~Season_Half) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank()) +
  labs(title = 'Shohei Ohtani Spray Chart', caption = '1stは4-6月、2ndは7月以降の打球をプロット')

#ggsave('output/figure/Ohtani_spray_half.png', width = 6, height = 3)

#---- Month -----

geom_spraytable(data = ohtani %>% filter(type == 'X', game_month <= 9),
                aes(x = hc_x, y = -hc_y, 
                    colour = base_hits)) +
  geom_point(alpha = .6) +
  ggplot2::scale_colour_manual(values = hit_colours, name = 'Result') +
  theme_few() +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank()) +
  facet_wrap(~game_month) +
  labs(title = 'Shohei Ohtani Spray Chart: monthly', caption = '10月はサンプルサイズが小さいため省略')

#ggsave('output/figure/Ohtani_spray_month.png', width = 6, height = 4.5)

#---- Shift -----

geom_spraytable(data = ohtani %>% filter(type == 'X'),
                aes(x = hc_x, y = -hc_y, 
                    colour = base_hits)) +
  geom_point(alpha = .6) +
  ggplot2::scale_colour_manual(values = hit_colours, name = 'Result') +
  theme_void() +
  facet_wrap(~ if_fielding_alignment) +
  labs(title = 'Shohei Ohtani Spray Chart')

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
