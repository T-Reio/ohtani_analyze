#subfuncs
source("library/func_cm_km.R")
source("library/event_lists.R")
source('library/colour_palette.R')

make_pitch_movement <- function(FN, LN, playerid, year = NULL, lims = NULL) {
  out <- list()
  
  player <- baseballr::scrape_statcast_savant(
    start_date = paste0(year, '-', 3, '-', '1'),
    end_date = paste0(year, '-', 11, '-', '30'),
    player_type = "pitcher", playerid = playerid
  ) %>%
    select(game_date, game_type, pitch_type, pfx_x, pfx_z, plate_x, plate_z, type,
           description, release_speed, stand, release_spin_rate, pitch_name, zone,
           events, description, launch_speed, launch_angle, bb_type, woba_value, woba_denom) %>%
    filter(!(pitch_type %in% c("PO", "IN", "null")), !is.na(pitch_type)) %>%
    dplyr::filter(game_type != 'S') %>%
    mutate(
      pfx_x_cm = cm(pfx_x),
      pfx_z_cm = cm(pfx_z),
      release_speed_km = km(release_speed),
      #woba_value = parse_number(woba_value),
      #woba_denom = parse_number(woba_denom),
    )
  
  if (is.null(lims)) {
    pl <- ggplot2::ggplot(player,
                          aes(x=pfx_x_cm, y=pfx_z_cm))
    pl2 <- pl + ggplot2::geom_point(aes(colour=pitch_name), alpha = .7) + ggplot2::theme_bw() +
      ggplot2::ggtitle(paste0(FN, ' ', LN, " : Pitch Movement ", year)) +
      ggplot2::xlab("Horizontal Movement (cm)") + ggplot2::ylab("Vertical Movement (cm)") +
      ggplot2::geom_hline(yintercept = 0) + ggplot2::geom_vline(xintercept = 0) +
      ggplot2::scale_colour_manual(values = pitch_colour) +
      ggplot2::labs(colour = "Pitch Type")
  } else {
    pfx_max <- lims[1]
    pfx_min <- lims[2]
    pfz_max <- lims[3]
    pfz_min <- lims[4]
    pl <- ggplot2::ggplot(player,
                          aes(x=pfx_x_cm, y=pfx_z_cm))
    pl2 <- pl + ggplot2::geom_point(aes(colour=pitch_name), alpha = .7) + ggplot2::theme_bw() +
      ggplot2::ggtitle(paste0(FN, ' ', LN, " : Pitch Movement ", year)) +
      ggplot2::xlab("Horizontal Movement (cm)") + ggplot2::ylab("Vertical Movement (cm)") +
      ggplot2::geom_hline(yintercept = 0) + ggplot2::geom_vline(xintercept = 0) +
      ggplot2::xlim(pfx_min, pfx_max) + ggplot2::ylim(pfz_min, pfz_max) +
      ggplot2::scale_colour_manual(values = pitch_colour) +
      ggplot2::labs(colour = "Pitch Type")
  }
  print(pl2)
  
  ggplot2::ggsave(filename = paste0("output/figure/", LN, "_", FN, year, ".png"), plot = pl2, width = 6, height = 4)
  
  pitchsum <- player %>%
    dplyr::group_by(pitch_name) %>%
    dplyr::summarise(number = n(), usage = round(100 * n() / nrow(player), 1),
                     speed = round(mean(release_speed_km), 1),
                     srate = round(mean(release_spin_rate, na.rm = T), 1),
                     hmov = round(mean(pfx_x_cm, na.rm = T), 1),
                     vmov = round(mean(pfx_z_cm, na.rm = T), 1),
                     Zone = round(sum(zone %in% 1:9)/n() * 100, 1),
                     SwStr = round(sum(description %in% swst) / n() * 100, 1),
                     Called = round(sum(description %in% calledstr) / n() * 100, 1),
                     Swing = round(sum(description %in% swing) / n() * 100, 1),
                     Contact = round(sum(description %in% contact) / 
                                       sum(description %in% swing, na.rm = T) * 100, 1),
                     GB = round(sum(bb_type == "ground_ball", na.rm = T) / 
                                  sum(!is.na(bb_type), na.rm = T) * 100, 1),
                     Hard = round(sum(launch_speed >= 95, na.rm = T) /
                                    sum(!is.na(launch_speed), na.rm = T) * 100, 1),
                     wOBA = round(sum(woba_value, na.rm = T) / sum(woba_denom, na.rm = T), 3),
    ) %>%
    dplyr::arrange(desc(number))
  
  colnames(pitchsum) <- c("Type", "#", "Usage(%)", "Velocity(km/h)", "Spin Rate(rpm)",
                          'hmov(cm)', 'vmov(cm)', 'Zone%', 'SwStr%',
                          "CalledStr%", "Swing%", "Contact%", "GB%", "Hard%", "wOBA")
  print(pitchsum)
  
  readr::write_excel_csv(pitchsum, paste0("output/table/", LN, "_", FN, year, ".csv"))
  player
}

okuyuki <- function(df, FN, LN, playerid, year = NULL, lims = NULL) {
  if (is.null(lims)) {
    pl <- ggplot2::ggplot(player,
                          aes(x = release_speed_km, y=pfx_z_cm))
    pl2 <- pl + ggplot2::geom_point(aes(colour=pitch_name), alpha = .7) + ggplot2::theme_bw() +
      ggplot2::ggtitle(paste0(FN, ' ', LN, "\n Velocity-Vertical Break Chart ", year)) +
      ggplot2::xlab("Pitch Velocity (km/h)") + ggplot2::ylab("Vertical Movement (cm)") +
      ggplot2::geom_hline(yintercept = 0) + #ggplot2::geom_vline(xintercept = 0) +
      ggplot2::scale_colour_manual(values = pitch_colour) +
      ggplot2::labs(colour = "Pitch Type")
  } else {
    pfx_max <- lims[1]
    pfx_min <- lims[2]
    pfz_max <- lims[3]
    pfz_min <- lims[4]
    pl <- ggplot2::ggplot(player,
                          aes(x = release_speed_km, y = pfx_z_cm))
    pl2 <- pl + ggplot2::geom_point(aes(colour=pitch_name), alpha = .7) + ggplot2::theme_bw() +
      ggplot2::ggtitle(paste0(FN, ' ', LN, "\n Velocity-Vertical Break Chart ", year)) +
      ggplot2::xlab("Pitch Velocity (km/h)") + ggplot2::ylab("Vertical Movement (cm)") +
      ggplot2::geom_hline(yintercept = 0) + #ggplot2::geom_vline(xintercept = 0) +
      ggplot2::xlim(pfx_min, pfx_max) + ggplot2::ylim(pfz_min, pfz_max) +
      ggplot2::scale_colour_manual(values = pitch_colour) +
      ggplot2::labs(colour = "Pitch Type")
  }
  print(pl2)
  
  ggplot2::ggsave(
    filename = paste0("output/figure/", LN, "_", FN, year, "_okuyuki.png"), plot = pl2,
    width = 6, height = 4
  )
}
