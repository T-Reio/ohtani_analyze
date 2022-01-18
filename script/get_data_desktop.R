library(tidyverse)
library(vroom)

pitch2021 <- read_rds('C:/Users/easyu/Dropbox/ballgame_economics/traking2021.rds')
chadwick <- vroom("https://raw.githubusercontent.com/chadwickbureau/register/master/data/people.csv", delim = ',')
