library(RJDBC)
library(ggplot2)

# Connect to GridDB
driver <- JDBC(
  driverClass = "com.toshiba.mwcloud.gs.sql.Driver",
  classPath = "/usr/share/java/gridstore-jdbc-5.1.0.jar"
)
conn <- dbConnect(driver, "jdbc:gs://127.0.0.1:20001/myCluster/public", "admin", "admin")


QUERY_1 <- "SELECT * FROM chess_table_depth_20 WHERE Black == 'D1stknightmaster'"
game_1 <- dbGetQuery(conn, QUERY_1)
game_1_white <- eval(parse(text = game_1$white_stats))

QUERY_2 <- "SELECT * FROM chess_table_depth_20 WHERE Black == 'Nakamura, Hikaru'"
game_2 <- dbGetQuery(conn, QUERY_2)
game_2_white <- eval(parse(text = game_2$white_stats))

QUERY_3 <- "SELECT * FROM chess_table_depth_20 WHERE Black == 'lichess AI level 8'"
game_3 <- dbGetQuery(conn, QUERY_3)
game_3_black <- eval(parse(text = game_3$black_stats))

QUERY_4 <- "SELECT * FROM chess_table_depth_20 WHERE Black == 'Niemann, Hans Moke'"
game_4 <- dbGetQuery(conn, QUERY_4)
game_4_black <- eval(parse(text = game_4$black_stats))

dataset <- data.frame(
  player = c(rep("Me", 3), rep("World Champion", 3), rep("Chess Engine", 3), rep("Niemann", 3)),
  moves = c(names(game_1_white), names(game_2_white), names(game_3_black), names(game_4_black)),
  values = c(game_1_white, game_2_white, game_3_black, game_4_black)
)

ggplot(dataset, aes(fill = moves, y = values, x = player)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_manual(
    "legend",
    values = c("Average Move" = "#bcc438", "Precise Move" = "#53afde", "Bad Move" = "#d8664d")
  )
