geom_spraytable <- function(...){
  ggplot(...) +
    geom_curve(x = 33, xend = 223, y = -100, yend = -100, curvature = -.65, colour = 'black') +
    geom_segment(x = 128, xend = 33, y = -208, yend = -100, colour = 'black') +
    geom_segment(x = 128, xend = 223, y = -208, yend = -100, colour = 'black') +
    geom_curve(x = 83, xend = 173, y = -155, yend = -156, curvature = -.65, linetype = 'dotted', colour = 'black') +
    coord_fixed() +
    scale_x_continuous(NULL, limits = c(25, 225)) +
    scale_y_continuous(NULL, limits = c(-225, -25))
}
