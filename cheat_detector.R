options(warn = -1)

library(dplyr)
library(magrittr)
library(stringr)
library(bigchess)
library(RJDBC)


# Connect to GridDB
driver <- JDBC(
  driverClass = "com.toshiba.mwcloud.gs.sql.Driver",
  classPath = "/usr/share/java/gridstore-jdbc-5.1.0.jar"
)
conn <- dbConnect(driver, "jdbc:gs://127.0.0.1:20001/myCluster/public", "admin", "admin")

calculateEngineCentipawnLoss <- function(
    lan,
    engine_path,
    limit_ranges = c(0, 25, 75, Inf),
    limit_names = c("Precise Move", "Average Move", "Bad Move"),
    engine_depth = 20
  ) {
  all_moves <- str_split(lan, " ")[[1]]

  notation <- ""
  white_scores <- c()
  black_scores <- c()

  for (i in seq_along(all_moves)) {
    notation <- trimws(paste(notation, all_moves[i]), which = "left")
    score <- uci_engine(engine_path) %>%
      uci_position(moves = notation) %>%
      uci_go(depth = engine_depth) %>%
      uci_quit() %>%
      uci_parse(filter = "score")

    score <- score * (-1)
    if (i %% 2 == 0) {
      # black move -> Evaluation of position according to white
      black_scores <- c(black_scores, score)
    } else {
      # white move -> Evaluation of position according to black
      white_scores <- c(white_scores, score)
    }
    print(paste("Move:", i, "Score:", score, "Notation:", notation))
    print('=================')
  }

  if (length(white_scores) != length(black_scores)) {
    # Black lost before making the last move.
    black_scores <- c(black_scores, NA)
    lan <- paste(lan, 'NA')
  }

  paired_moves <- trimws(strsplit(lan, "(?<=.{10})", perl = TRUE)[[1]], which = "right")

  game_results <- data.frame(
    notation = paired_moves,
    white_scores = white_scores,
    black_scores = black_scores
  )
  
  game_results <- game_results %>%
    mutate(
      black_difference = white_scores + black_scores,
      white_difference = white_scores + dplyr::lag(black_scores),
      
      # Although very rarely, we can still get positive values due to engine not taking
      # all possiblities into account. We will convert values larger than zero, meaning the
      # values where the person outperformed the engine, as zero for simplificaiton.
      white_difference = ifelse(white_difference > 0 | is.na(white_difference), 0, white_difference),
      black_difference = ifelse(black_difference > 0 | is.na(black_difference), 0, black_difference),
      
      # Get the ranges of moves the user played. 
      white_cuts = cut(white_difference * -1, breaks = limit_ranges, include.lowest = T, labels = limit_names),
      black_cuts = cut(black_difference * -1, breaks = limit_ranges, include.lowest = T, labels = limit_names),
    )
    
  white_percentages <- prop.table(table(game_results$white_cuts))
  black_percentages <- prop.table(table(game_results$black_cuts))
  
  white_average_cp <- abs(mean(game_results$white_difference))
  black_average_cp <- abs(mean(game_results$black_difference))
  
  return(
    list(
      all_data = game_results,
      white_results = white_percentages,
      black_results = black_percentages,
      white_average_cp = white_average_cp,
      black_average_cp = black_average_cp
    )
  )
}

engine_path <- "/usr/games/stockfish"

list_of_games_to_analyze <- list.files("Data")

dbListTables(conn)
for (i in seq_along(list_of_games_to_analyze)) {
  game <- read.pgn(con = file(paste0("Data/", list_of_games_to_analyze[i])), stat.moves = F, extract.moves = 0)
  for (j in seq_len(nrow(game))) {
    sub_game <- game[j, ]
    game_lan <- san2lan(sub_game$Movetext[1])
    game_result <- calculateEngineCentipawnLoss(lan = game_lan, engine_path = engine_path, engine_depth = 20)
    sub_game$white_stats <- list(game_result$white_results)
    sub_game$black_stats <- list(game_result$black_results)
    sub_game$white_average_cp <- game_result$white_average_cp
    sub_game$black_average_cp <- game_result$black_average_cp
    dbWriteTable(conn, "chess_table_depth_20", sub_game, append = T)
  }
}
