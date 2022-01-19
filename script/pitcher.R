library(tidyverse)

source('library/event_lists.R')
source('library/colour_palette.R')
source('script/add_playvalues.R')

#------ stats ---------------

fg_stats <- fg_pitch_leaders(2021, 2021, qual = 100)

fg_stats %>%
  select(Name, ERA, W, L, SV, G, GS, IP, K_pct, SO, BB, K_BB, WHIP, FIP, tERA, WAR) %>%
  arrange(-tERA) %>%
  mutate(no = row_number()) %>% 
  filter(Name == 'Shohei Ohtani')

#write_excel_csv(stats_saved, 'output/table/stas_fg_min100.csv')

#------ Add playername -------

names <- chadwick %>%
  select(name_last, name_first, key_mlbam)

pitch2021 %>%
  left_join(., names, by = c('pitcher' = 'key_mlbam')) -> pitch2021

#pitch2021 %>% colnames()

#------League Average ---------

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
    pv_tj = formatC(sum(-delta_run_exp_tj, na.rm = T), digits = 1, format = 'f'),
    pvC_tj = formatC(mean(-delta_run_exp_tj, na.rm = T) * 100, digits = 2, format = 'f'),
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

#------- Ohtani Stats ---------

ohtani <- pitch2021 %>% 
  filter(pitcher == 660271) %>%
  mutate(game_month = month(game_date))

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
    pv_tj = formatC(sum(-delta_run_exp_tj, na.rm = T), digits = 1, format = 'f'),
    pvC_tj = formatC(mean(-delta_run_exp_tj, na.rm = T) * 100, digits = 2, format = 'f'),
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

#------- by Month -----------------

ohtani %>%
  group_by(game_month) %>%
  summarise(total = n()) -> pitch_bymonth

ohtani %>%
  left_join(., pitch_bymonth) %>%
  group_by(pitch_name, game_month) %>%
  summarise(
    pitches = n(),
    ratio = round(n() / first(total) * 100, digits = 1),
    velo = round(mean(release_speed, na.rm = T) * 1.61, digits = 1),
    pfx_x = round(mean(pfx_x, na.rm = T) * 30.48, digits = 1),
    pfx_z = round(mean(pfx_z, na.rm = T) * 30.48, digits = 1),
    pv = round(-sum(delta_run_exp, na.rm = T), digits = 1),
    pvC = round(-mean(delta_run_exp, na.rm = T) * 100, digits = 2),
    pv_tj = round(sum(-delta_run_exp_tj, na.rm = T), digits = 1),
    pvC_tj = round(mean(-delta_run_exp_tj, na.rm = T) * 100, digits = 2),
    spinrate = round(mean(release_spin_rate, na.rm = T), digits = 1),
    Zone = round(sum(zone %in% 1:9, na.rm = T)/n() * 100, digits = 1),
    SwStr = round(sum(description %in% swst, na.rm = T) / n() * 100, digits = 1),
    CStr = round(sum(description %in% calledstr, na.rm = T) / n() * 100, digits = 1),
    Swing = round(sum(description %in% swing, na.rm = T) / n() * 100, digits = 1),
    Contact = round(sum(description %in% contact) / 
                        sum(description %in% swing, na.rm = T) * 100, digits = 1),
    GB = round(sum(bb_type == "ground_ball", na.rm = T) / 
                   sum(bb_type %in% c('ground_ball', 'line_drive', 'fly_ball', 'popup'), na.rm = T) * 100, digits = 1),
    LD = round(sum(bb_type == "line_drive", na.rm = T) / 
                   sum(bb_type %in% c('ground_ball', 'line_drive', 'fly_ball', 'popup'), na.rm = T) * 100, digits = 1),
    FB = round(sum(bb_type == "fly_ball", na.rm = T) / 
                   sum(bb_type %in% c('ground_ball', 'line_drive', 'fly_ball', 'popup'), na.rm = T) * 100, digits = 1),
    PU = round(sum(bb_type == "popup", na.rm = T) / 
                   sum(bb_type %in% c('ground_ball', 'line_drive', 'fly_ball', 'popup'), na.rm = T) * 100, digits = 1),
    Hard = round(sum(launch_speed >= 95, na.rm = T) /
                     sum(!is.na(launch_speed), na.rm = T) * 100, digits = 1),
    wOBA = round(sum(woba_value, na.rm = T) / sum(woba_denom, na.rm = T), digits = 3),
    xwOBA = round(sum(xwoba_value, na.rm = T) / sum(woba_denom, na.rm = T), digits = 3),
  ) %>%
  arrange(desc(pitch_name), game_month) -> summary_bymonth

ohtani %>%
  #left_join(., pitch_bymonth) %>%
  group_by(game_month) %>%
  summarise(
    pitches = n(),
    pv = round(-sum(delta_run_exp, na.rm = T), digits = 1),
    pvC = round(-mean(delta_run_exp, na.rm = T) * 100, digits = 2),
  ) %>%
  arrange(game_month) -> pv_byMonth

names(pitch_colour) %>% intersect(unique(ohtani$pitch_name)) -> pitches

pitch_colour[names(pitch_colour) %in% pitches] -> pitch_colours_temp

ggplot() +
  geom_bar(data = summary_bymonth, 
           aes(x = game_month, y = pv, fill = pitch_name), 
           stat = 'identity', 
           position = 'stack') +
  scale_fill_manual(values = pitch_colours_temp) + 
  theme_bw() +
  geom_point(data = pv_byMonth, aes(x = game_month, y = pv), colour = 'blue', size = 3) +
  geom_line(data = pv_byMonth, aes(x = game_month, y = pv), colour = 'blue', size = 1) +
  geom_hline(yintercept = 0, size = 1) +
  labs(title = '月別 PitchValueの推移', x = "月", y = "PitchValue", fill = '球種')

ggsave('output/figure/monthly_pv.png', width = 4, height = 4)

#write_excel_csv(summary_bymonth, 'output/table/ohtani_monthly.csv')

#-------pitch value rankings--------

pitch2021 %>%
  group_by(pitcher, pitch_name) %>%
  summarise(
    name = paste0(first(name_last), ', ' , first(name_first)),
    rank = -mean(delta_run_exp, na.rm = T) * 100,
    rank_tj = -mean(delta_run_exp_tj, na.rm = T) * 100,
    tot = n(),
    velo = round(mean(release_speed, na.rm = T) * 1.61, digits = 1),
    pfx_x = round(mean(pfx_x_mirror, na.rm = T) * 30.48, digits = 1),
    pfx_z = round(mean(pfx_z, na.rm = T) * 30.48, digits = 1),
    pv = round(-sum(delta_run_exp, na.rm = T), digits = 1),
    pvC = round(-mean(delta_run_exp, na.rm = T) * 100, digits = 2),
    pv_tj = round(sum(-delta_run_exp_tj, na.rm = T), digits = 1),
    pvC_tj = round(mean(-delta_run_exp_tj, na.rm = T) * 100, digits = 2),
  ) %>%
  filter(tot >= 200) %>%
  arrange(-rank) %>%
  ungroup() %>%
  mutate(rank = row_number()) %>%
  select(rank, name, pitch_name, tot, velo, pfx_x, pfx_z, pv, pvC, pv_tj, pvC_tj) -> ranking

write_excel_csv(ranking, 'output/table/pitchValue_rankings_min200.csv')


#------- strike zone ---------

library(mgcv)

df_split <- pitch2021 %>%
  split(list(pull(., stand), pull(., Count)))

called <- df_split %>%
  map(~filter(., description %in% c("ball", "called_strike")) %>%
        mutate(
          Strike = if_else(description == 'called_strike', 1, 0)
        ))

call_fit <- called %>%
  map(~ gam(Strike ~ s(plate_x, plate_z),
            family = 'binomial',
            data=.))

fit_strike <- function(i, call_fit, df_split){
  df_split[[i]] %>%
    mutate(
      lp = predict(call_fit[[i]], .) %>% c(),
      prob = exp(lp) / (1 + exp(lp)),
    )
}

df_with_cp <- map(1:length(call_fit),
                  .f = fit_strike, call_fit = call_fit, df_split = df_split)

cf <- bind_rows(df_with_cp)



ohtani_split <- ohtani %>%
  split(list(pull(., stand), pull(., pitch_name)))

#----- Ohtani above ave -------

ohtani_with_call <- cf %>% 
  filter(pitcher == 660271) %>%
  filter(description %in% c("ball", "called_strike"))

ohtani_with_call %>%
  mutate(
    Strike = if_else(type == 'S', 1, 0),
    Zone = if_else(zone %in% 1:9, 1, 0),
  ) %>%
  summarise(
    diff = sum(Strike - prob, na.rm = T),
    type1 = sum(Zone - Strike == 1, na.rm = T),
    type2 = sum(Zone - Strike == -1, na.rm = T),
  )


called_pitch %>%
  mutate(
    Strike = if_else(type == 'S', 1, 0),
    Zone = if_else(zone %in% 1:9, 1, 0),
    Zone_individual = if_else(
      (plate_x <= 0.85 & plate_x >= -0.85 & 
         plate_z >= sz_bot & plate_z <= sz_top),
      1, 0
    ),
  ) %>%
  group_by(pitcher) %>%
  summarise(
    name = paste0(first(name_last), ', ' , first(name_first)),
    total = n(),
    diff = sum(Strike - prob, na.rm = T),
    type1 = sum(Zone_individual - Strike == 1, na.rm = T),
    type2 = sum(Zone_individual - Strike == -1, na.rm = T),
    runs = sum(delta_run_exp_tj - expected_run, na.rm = T),
  ) %>%
  filter(total >= 500) %>%
  mutate(type1_ratio = type1 / total * 100) %>%
  arrange(-type1_ratio) %>%
  mutate(rank = row_number()) -> called_ranking_pitcher

write_excel_csv(called_ranking_pitcher, 'output/table/pitcher_pitchcall_runs_ranking_min500.csv')
