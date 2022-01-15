library(tidyverse)
library(RMariaDB)
library(broom)

conn <- DBI::dbConnect(MariaDB(), host = 'localhost', dbname = 'abdwr', port = 3306,
                       user = 'root', password = 'ENPP7vW52Svt')

class(conn)

query <- "
SELECT * FROM retro_pbp
WHERE (SUBSTRING(GAME_ID, 4, 4) IN (2018, 2019, 2020, 2021))
"
pbp <- DBI::dbGetQuery(conn, query)

query <- "
SELECT * FROM 
WHERE (SUBSTRING(GAME_ID, 4, 4) IN (2018, 2019, 2020, 2021))
"
gamelogs <- DBI::dbGetQuery(conn, query)

query <- "
SELECT * FROM savant_tracking
WHERE (SUBSTRING(game_date, 1, 4) IN (2021))
"

pitch2021 <- DBI::dbGetQuery(conn, query)

DBI::dbDisconnect(conn)

chadwick <- vroom::vroom("https://raw.githubusercontent.com/chadwickbureau/register/master/data/people.csv", delim = ',')

