library(tidyverse)
library(RMariaDB)
library(broom)

conn <- DBI::dbConnect(MariaDB(), host = 'localhost', dbname = 'abdwr', port = 3306,
                       user = 'root', password = 'ENPP7vW52Svt')

#class(conn)

#------ playByPlay -----

query <- "
SELECT * FROM retro_pbp
WHERE (SUBSTRING(GAME_ID, 4, 4) IN (2018, 2019, 2020, 2021))
"
pbp <- DBI::dbGetQuery(conn, query)

#------ Gamelogs ------

query <- "
SELECT * FROM gamelogs
WHERE (DATE > 20180000)
"
gamelogs <- DBI::dbGetQuery(conn, query)

#------ PitchByPitch ------

query <- "
SELECT * FROM savant_tracking
WHERE (SUBSTRING(game_date, 1, 4) IN (2021))
"

pitch2021 <- DBI::dbGetQuery(conn, query)

#write_rds(pitch2021, 'C:/Users/easyu/Dropbox/ballgame_economics/traking2021.rds')

DBI::dbDisconnect(conn)

#------ chadwick ---------------

chadwick <- vroom::vroom("https://raw.githubusercontent.com/chadwickbureau/register/master/data/people.csv", delim = ',')

