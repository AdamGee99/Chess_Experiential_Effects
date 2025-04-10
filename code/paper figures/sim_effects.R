#' April 8, 2025
#' 
#' Script for simulating data with varying experiential effect sizes and number of games per player
#' to test how well our model can actually capture this effects if they exist

library(tidyverse)
library(here)
library(rstanarm)


#' data simulation
#' assume that the probability player j wins game i is p_ij = inv_logit(alpha_j + beta_j*x_ij + gamma_1*colour + gamma_2*rating_diff)
#' will need to also give values for the other parameters
#' do this for 1700-1900s and GMs
#' assume there are 10 players in each cohort
#' also assume that we can't get any draws under our model... (is this a big assumption?)


#' experiential effects size of logit(0.51) represents 1% increase win percentage when coming from a win... logit(0.53) represents 3% increase...
experiential_effect_size = c(logit(0.51), logit(0.53), logit(0.6)) #add some noise so everyone has a slightly different winner effect
num_games_per_player = c(100, 5000, 20000)

#' values for other parameters
#' make player effect come from the same prior as our model has for each cohort - say N(-0.05, 0.05) for 1700-1900s, N(0, 0.05) for GMs
#' fixed effects can be same as the cohort the player is in 

#' use model results for setting default values for other parameters (is this an issue? the prior for it has correlation to betas..)
model = readRDS(file = here("results", "lichessHumans", "1700-1900", "fit2_test", "full_fit.RDS"))
users = readRDS(file = here("results", "lichessHumans", "1700-1900", "fit2_test", "users_bullet.RDS"))

num_players = 10

#population effects values
gamma1 = model$summary("gamma1") %>% pull(mean) #colour effect
gamma2 = model$summary("gamma2") %>% pull(mean) #rating effect

#varying player effect values
set.seed(2025)
alphas = rnorm(n = num_players, mean = -0.075, sd = 0.05) #rough dist of the alphas from 1700-1900 fit in paper - should be close enough


#' now simluate the games 
#' need to do a for loop for each player for each game
#' at each game, flip a coin for white or black, draw from some distribution for estimated rating difference - N(0, 100) seems good

#for storage - list of each simulated dataset
sim_datasets = data.frame("player_id" = as.numeric(), "result" = as.numeric(), "colour" = as.numeric(), 
           "rating_diff" = as.numeric(), "last_result" = as.numeric(), "beta" = as.numeric(), 
           "num_games_per_player" = as.numeric(), "alpha" = as.numeric(), "gamma1" = as.numeric(), "gamma2" = as.numeric()) %>% 
  list() %>% rep(length(experiential_effect_size)*length(num_games_per_player))

curr_dataset_number = 1 #the current dataset number - for indexing later
for (beta in experiential_effect_size) { #loop through different effect sizes
  for (num_games in num_games_per_player) { #loop through different number of games per player
    print(paste0("dataset_number: ", curr_dataset_number)) #for analysing for loop speed
    curr_row = 1
    
    for (j in 1:num_players) { #loop through each player for this particular data set (effect size and sample size combination)
      curr_player_beta = beta + rnorm(n = 1, mean = 0, sd = 0.05) #add some noise around the mu_beta so not every player is the same
      print(paste0("player_id: ", j)) #for analysing for loop speed
      
        for (i in 1:num_games) { #loop through the number of games for that player
        
        #sim current covariate info
        curr_rating_diff = rnorm(n = 1, mean = 0, sd = 60) #sd found from the sd of the original 1700-1900 rating diffs
        curr_colour = sample(c(0, 1), prob = c(0.5, 0.5), size = 1) #1 is white, 0 is black
        last_result = ifelse(i == 1, 0.5, sim_datasets[[curr_dataset_number]][(i - 1), 2]) #4 is the index of the lst result
        
        #sim results
        lin_comb = alphas[j] + curr_player_beta*last_result + gamma1*curr_colour + gamma2*curr_rating_diff #the predictors
        curr_win_prob = invlogit(lin_comb) #simulate probability
        curr_result = sample(c(1, 0), prob = c(curr_win_prob, 1 - curr_win_prob), size = 1)
        
        #store everything - player number, result, colour, rating diff, last result, parameter values
        sim_datasets[[curr_dataset_number]][curr_row,] = c(j, curr_result, curr_colour, curr_rating_diff, last_result, 
                                                    curr_player_beta, num_games, alphas[j], gamma1, gamma2) #the row for this data set
        curr_row = curr_row + 1 #next row
      }
    }
    #next dataset number
    curr_dataset_number = curr_dataset_number + 1
  }
}
#save sim datasets
# for(i in 1:9) {
#   write.csv(sim_datasets[[i]], file = paste0(here("results", "sim_datasets", "1700-1900", paste0("sim_dataset_", i, ".csv"))))
# }


#probabilities given last result
prob_given_last_result = sim_datasets[[6]] %>%
  group_by(player_id, last_result) %>%
  summarise(win_prob = signif(mean(result), 2),
            beta = unique(beta)) %>%
  ungroup() %>%
  filter(last_result != 0.5) %>%
  mutate(true_win_prob = ifelse(last_result == 1, invlogit(beta), 1 - invlogit(beta)))
prob_given_last_result

prob_given_last_result_plot = ggplot(data = prob_given_last_result, mapping = aes(x = last_result, y = win_prob)) +
  geom_line() +
  geom_line(mapping = aes(x = last_result, y = true_win_prob), col = "red") +
  facet_wrap(~player_id) +
  ylim(c(0,1))
prob_given_last_result_plot

#red is the true expected win probability given last result
#black is observed

#just look at the slopes, they should be the same, the black line can be shifted up or down because of the player effect (intercept)


# #running in parallel
# library(foreach)
# library(doParallel)
# 
# n_cores = 10
# cluster = parallel::makeCluster(
#   n_cores, 
#   type = "PSOCK"
# )
# doParallel::registerDoParallel(cl = cluster)
# 
# 
# test = foreach(beta = experiential_effect_size, .combine = "cbind", .packages = c("tidyverse", "doParallel", "rstanarm")) %dopar% {
#   foreach(num_games = num_games_per_player, .combine = "cbind") %do% {
#     foreach(j = 1:num_players, .combine = "c") %do% {
#       foreach(i = 1:num_games, .combine = "c") %do% {
#         
#         #sim current covariate info
#         curr_rating_diff = rnorm(n = 1, mean = 0, sd = 100)
#         curr_colour = sample(c(0, 1), prob = c(0.5, 0.5), size = 1) #1 is white, 0 is black
#         last_result = 0.5
#           #ifelse(i == 1, 0.5, sim_datasets[[curr_dataset_number]][(i - 1), 5]) #4 is the index of the lst result
#         
#         #sim results
#         lin_comb = alphas[j] + beta*last_result + gamma1*curr_colour + gamma2*curr_rating_diff #the predictors
#         curr_win_prob = invlogit(lin_comb) #simulate probability
#         curr_result = sample(c(1, 0), prob = c(curr_win_prob, 1 - curr_win_prob), size = 1)
#         
#         c(j, curr_result, curr_colour, curr_rating_diff, last_result, 
#           beta, num_games, alphas[j], gamma1, gamma2)
#       }
#     }
#   }
# }




