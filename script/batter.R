library(tidyverse)
library(baseballr)
source('library/event_lists.R')
source('library/colour_palette.R')

#月別xwOBA

names <- chadwick %>%
  select(batter_name_last = name_last, batter_name_first = name_first, key_mlbam)

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
    )
  ) -> pitch2021

pitch2021 %>%
  group_by(batter) %>%
  summarise(
    tot = n(),
    ratio = formatC(n() / nrow(pitch2021) * 100, digits = 1, format = 'f'),
    velo = formatC(mean(release_speed, na.rm = T) * 1.61, digits = 1, format = 'f'),
    pv = formatC(-sum(delta_run_exp, na.rm = T), digits = 1, format = 'f'),
    pvC = formatC(-mean(delta_run_exp, na.rm = T) * 100, digits = 2, format = 'f'),
    pv_tj = formatC(sum(-delta_run_exp_tj, na.rm = T), digits = 1, format = 'f'),
    pvC_tj = formatC(mean(-delta_run_exp_tj, na.rm = T) * 100, digits = 2, format = 'f'),
    CStr = formatC(sum(description %in% calledstr) / n() * 100, digits = 1, format = 'f'),
    Swing = formatC(sum(description %in% swing) / n() * 100, digits = 1, format = 'f'),
    Contact = formatC(sum(description %in% contact) / 
                        sum(description %in% swing, na.rm = T) * 100, digits = 1, format = 'f'),
    GB = formatC(sum(bb_type == "ground_ball", na.rm = T) / 
                   sum(bb_type %in% c('ground_ball', 'line_drive', 'fly_ball', 'popup'), na.rm = T) * 100, digits = 1, format = 'f'),
    LD = formatC(sum(bb_type == "line_drive", na.rm = T) / 
                   sum(bb_type %in% c('ground_ball', 'line_drive', 'fly_ball', 'popup'), na.rm = T) * 100, digits = 1, format = 'f'),
    FB = formatC(sum(bb_type == "fly_ball", na.rm = T) / 
                   sum(bb_type %in% c('ground_ball', 'line_drive', 'fly_ball', 'popup'), na.rm = T) * 100, digits = 1, format = 'f'),
    PU = formatC(sum(bb_type == "popup", na.rm = T) / 
                   sum(bb_type %in% c('ground_ball', 'line_drive', 'fly_ball', 'popup'), na.rm = T) * 100, digits = 1, format = 'f'),
    Hard = formatC(sum(launch_speed >= 95, na.rm = T) /
                     sum(!is.na(launch_speed), na.rm = T) * 100, digits = 1, format = 'f'),
    wOBA = formatC(sum(woba_value, na.rm = T) / sum(woba_denom, na.rm = T), digits = 3, format = 'f'),
    xwOBA = formatC(sum(xwoba_value, na.rm = T) / sum(woba_denom, na.rm = T), digits = 3, format = 'f'),
    #Barrel = 
  ) %>%
  arrange(-tot)


#----- Ohtani --------

ohtani <- pitch2021 %>%
  filter(batter == 660271)

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
    Season_Half = if_else(game_date >= '2021-07-01', '1st', '2nd'),
  )

velo_min <- 50
velo_max <- 220
angle_min <- -60
angle_max <- 80

p1 <- ggplot2::ggplot(
  data = ohtani, aes(x = launch_speed_km, y = launch_angle, colour = base_hits)
) +
  ggplot2::geom_point(size = 1, alpha = .7) + ggplot2::theme_bw() +
  ggplot2::ggtitle(paste0("Shohei Ohtani: Batted Ball 2021")) +
  ggplot2::xlab("Exit Velocity (km/h)") + ggplot2::ylab("Launch Angle (°)") +
  ggplot2::geom_hline(yintercept = 0, linetype = 2) + 
  ggplot2::geom_vline(xintercept = 152.95, linetype = 2) +
  ggplot2::scale_colour_manual(values = hit_colours, name = 'Result') +
  ggplot2::xlim(velo_min, velo_max) + ggplot2::ylim(angle_min, angle_max)
p1

p1 +
  facet_wrap(~ Season_Half) -> p2

ggsave('output/figure/Ohtani_angle_speed.png', width = 5, height = 3)

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
