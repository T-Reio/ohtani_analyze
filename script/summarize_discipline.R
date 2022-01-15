library(tidyverse)
source('library/event_lists.R')

names <- chadwick %>%
  select(name_last, name_first, key_mlbam)

pitch2021 %>%
  left_join(., names, by = c('pitcher' = 'key_mlbam')) -> pitch2021

pitch2021 %>% colnames()

pitch2021 %>%
  mutate(
    pfx_x_mirror = if_else(p_throws == 'L', -pfx_x, pfx_x),
    xwoba_value = if_else(is.na(estimated_woba_using_speedangle), woba_value, estimated_woba_using_speedangle),
  )%>%
  arrange(game_pk,at_bat_number,pitch_number) -> pitch2021


#pitch2021 %>%
#  pull(pitch_type) %>%
#  unique()

#pitch2021 %>%
#  filter(pitch_type == '') %>% view()

league_average <- pitch2021 %>%
  group_by(pitch_name) %>%
  summarise(
    tot = n(),
    ratio = formatC(n() / nrow(pitch2021) * 100, digits = 1, format = 'f'),
    velo = formatC(mean(release_speed, na.rm = T) * 1.61, digits = 1, format = 'f'),
    pfx_x = formatC(mean(pfx_x_mirror, na.rm = T) * 30.48, digits = 1, format = 'f'),
    pfx_z = formatC(mean(pfx_z, na.rm = T) * 30.48, digits = 1, format = 'f'),
    pv = formatC(-sum(delta_run_exp, na.rm = T), digits = 1, format = 'f'),
    pvC = formatC(-mean(delta_run_exp, na.rm = T) * 100, digits = 2, format = 'f'),
    spinrate = formatC(mean(release_spin_rate, na.rm = T), digits = 1, format = 'f'),
    Zone = formatC(sum(zone %in% 1:9)/n() * 100, digits = 1, format = 'f'),
    SwStr = formatC(sum(description %in% swst) / n() * 100, digits = 1, format = 'f'),
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
  ) %>%
  filter(!(pitch_name %in% c('', 'Fastball', 'Screwball', 'Eephus'))) %>%
  arrange(-tot)

league_average %>% view()

ohtani <- pitch2021 %>% filter(pitcher == 660271)

ohtani %>%
  group_by(pitch_name) %>%
  summarise(
    #name = paste0(first(name_last), ', ', first(name_first)),
    pitches = n(),
    ratio = formatC(n() / nrow(ohtani) * 100, digits = 1, format = 'f'),
    velo = formatC(mean(release_speed, na.rm = T) * 1.61, digits = 1, format = 'f'),
    pfx_x = formatC(mean(pfx_x, na.rm = T) * 30.48, digits = 1, format = 'f'),
    pfx_z = formatC(mean(pfx_z, na.rm = T) * 30.48, digits = 1, format = 'f'),
    pv = formatC(-sum(delta_run_exp, na.rm = T), digits = 1, format = 'f'),
    pvC = formatC(-mean(delta_run_exp, na.rm = T) * 100, digits = 2, format = 'f'),
    spinrate = formatC(mean(release_spin_rate, na.rm = T), digits = 1, format = 'f'),
    Zone = formatC(sum(zone %in% 1:9)/n() * 100, digits = 1, format = 'f'),
    SwStr = formatC(sum(description %in% swst) / n() * 100, digits = 1, format = 'f'),
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
  ) %>%
  #filter(name == 'Ohtani, Shohei') %>%
  arrange(-pitches) -> summary
  #select(pitch_name, pitches, velo, pfx_x, pfx_z, pv, pvC)
summary %>% view()

write_excel_csv(summary, 'output/table/ohtani2021_summary.csv')
write_excel_csv(league_average, 'output/table/league2021_summary.csv')
