library(tidyverse)

LN <- 'Ohtani'
FN <- 'Shohei'

#baseballr::playerid_lookup(LN, FN) %>%
#  select(first_name, last_name, birth_year, mlbam_id)
id <- 660271
yr <- 2021

source('library/make_pitch_movement.R', encoding = 'utf-8')

player <- make_pitch_movement(FN = FN, LN = LN, playerid = id, year = yr, lims = c(75, -55, 60, -50))

#player <- make_pitch_movement(FN = FN, LN = LN, playerid = id, year = yr, lims = c(x_max, x_min, z_max, z_min))

okuyuki(player, FN, LN, playerid, yr, lims = c(170, 100, 60, -50))
