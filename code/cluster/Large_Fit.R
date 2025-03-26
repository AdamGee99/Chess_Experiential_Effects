##### March 5th 2024 ######
###########
## Fit the current model to a large selection of 
## games on a cluster
## stan model is somewhat optimized for speed to fit to faster, using 
## matrix multiplication

library(tidyverse)
library(RcppRoll)
library(cmdstanr)
library(posterior)
library(bayesplot)
library(loo)
library(here)

options(mc.cores = parallel::detectCores())

## source helper functions for reading and transforming data
## along with defaults for plots, etc
source("/home/adamgee/scratch/code/helper functions/helper.R")
source("/home/adamgee/scratch/code/helper functions/plot_templates.R")

#path_id <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))

#not sure if this is necessary but I'll do it just to be safe
set_cmdstan_path("/home/adamgee/R/x86_64-pc-linux-gnu-library/4.4/cmdstan")

### load in the data to use
data_path = "/home/adamgee/scratch/data/lichess1700-1900_with_bots"
save_path = "/home/adamgee/scratch/results/human_bot_results/1700-1900/"

files = list.files(data_path)
lichess_data = files %>%
  map_dfr(~read_player(data_path, .x))


## restrict to rated rapid and shorter here
small_data = lichess_data %>%
  mutate(Event = tolower(Event)) |>
  filter(Event == "rated bullet game", Variant == "Standard")

users = small_data$Username %>% unique() %>% sort()

#remove duplicated games - only an issue with grandmasters...
small_data = small_data[!duplicated(small_data),]

saveRDS(users, file = paste0(save_path, "users_bullet.RDS"))
# saveRDS(users, file = paste0(save_path, "users_blitz.RDS"))

tidy_games = map_dfr(users, get_hist, small_data, prev_n = 1) %>% 
  as_tibble()

#final tidying
init_data = tidy_games %>%
  mutate(WhiteElo = as.numeric(WhiteElo),
         BlackElo = as.numeric(BlackElo)) %>%
  filter(!is.na(WhiteElo) & !is.na(BlackElo)) %>%
  mutate(focal_user = ifelse(focal_white == 1, White, Black)) %>%
  mutate(elo_diff = ifelse(focal_white == 1,
                           WhiteElo - BlackElo, BlackElo - WhiteElo)) %>%
  mutate(focal_id = match(focal_user, users)) %>%
  mutate(opponent = ifelse(focal_white == 1, Black, White)) %>%
  mutate(opponent_id = match(opponent, users)) %>%
  dplyr::select(focal_user, focal_id, opponent_id, focal_white, White, Black, WhiteElo, BlackElo,
                focal_win_prop, elo_diff, focal_result) %>%
  group_by(focal_id) %>%
  mutate(ave_prop = lag(focal_win_prop, default = 0) - mean(focal_result)) %>%
  mutate(prev_result = lag(focal_win_prop, default = 0)) %>%
  filter(focal_result != 0.5) %>%
  ungroup() %>%
  ###filtering out rematches and big elo differences
  mutate(opp = ifelse(focal_white == 1, Black, White)) %>%
  filter(lag(opp) != opp) %>% #filter out rematches
  #filter(sign(elo_diff) != sign(lag(elo_diff))) %>%
  filter(abs(elo_diff) <= 200) #trying to mimic player games - they hardly ever play people rated 200 away from them 


#probabilities given last result
prob_given_last_result = init_data %>%
    mutate(result = focal_result) %>%
    group_by(focal_user, prev_result) %>%
    summarise(win_prob = signif(mean(result), 2),
              n = n()) %>%
    ungroup()
prob_given_last_result
  
prob_given_last_result_plot = ggplot(data = prob_given_last_result, mapping = aes(x = prev_result, y = win_prob)) +
    geom_line() +
    facet_wrap(~focal_user)
prob_given_last_result_plot

#save prob_given_last plot
ggsave(plot = prob_given_last_result_plot, 
       filename = paste0(save_path, "prob_given_last.png"), dpi = 1000,
       width = 7, height = 6)


### then fit the models
stan_data_ave = list(N = nrow(init_data),
                      J = length(users),
                      y = init_data$focal_result,
                      id = init_data$focal_id,
                      colour = init_data$focal_white,
                      elo = init_data$elo_diff,
                      win_prop = init_data$ave_prop)


#saveRDS(object = stan_data_ave, file = "C:/Users/adamg/Desktop/Thesis Fall 2024/cluster code and data/gm_middle_1000_stan_data.RDS")


#stan_file <- here("owen", "cluster_scripts", "final_model_scale_priors.stan")
stan_file = "/home/adamgee/scratch/code/final_model_scale_priors_NEW.stan"

mod = cmdstan_model(stan_file)

fit3_ave = mod$sample(data = stan_data_ave,
                       seed = 123,
                       chains = 4,
                       parallel_chains = 4,
                       refresh = 100)


## save the stan fit as not actually that large here,
## when no generated quantities

fit3_ave$save_object(file = paste0(save_path, "full_fit.RDS"))






