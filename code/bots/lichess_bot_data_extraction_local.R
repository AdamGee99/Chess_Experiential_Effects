#' March 19, 2025
#' Extracting bot data from lichess locally in parallel - cluster is down

#running in parallel locally
library(foreach)
library(doParallel)

#the usernames to extract
bot_usernames = c("honzovy-sachy", 
                  #"eubos", 
                  "likeawizard-bot") 
                  #"Lynx_BOT", 
                  #"InanisBot", 
                  #"Euwe-chess-engine")

#number of cores, max out at 15
n_cores = min(8, length(bot_usernames))
cluster = parallel::makeCluster(
  n_cores, 
  type = "PSOCK"
)
doParallel::registerDoParallel(cl = cluster)

#for loop in parallel
foreach(user = bot_usernames, .packages = c("tidyverse", "chessR", "here"), .errorhandling = "pass") %dopar% {
  bot_games = chessR::get_raw_lichess(user)
  
  #remove moves column, we don't need it and it takes up so much space
  bot_games = bot_games %>% select(-Moves)
  
  write.csv(bot_games, file = here("data", "lichessBots", paste0(user, ".csv")), row.names = FALSE)
}

