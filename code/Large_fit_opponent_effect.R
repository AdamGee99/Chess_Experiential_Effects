##### March 5th 2024 ######
###########
## Fit the current model to a large selection of 
## games on a cluster
## stan model is somewhat optimized for speed to fit to faster, using 
## matrix multiplication
##

library(tidyverse)
library(data.table)
library(here)
library(cmdstanr)
library(bayesplot)
library(posterior)
library(ggridges)
library(scales)
library(ggdist)
library(rstanarm)
library(bigchess) #package for reading in PGNs


### FUNCTIONS ###

source(here("code", "helper functions", "plot_templates.R"))
#function to read in separate files and merge
read_player = function(path, file){
  dat <- read_csv(file = paste0(data_path, "/", file),
                  col_types = cols(UTCDate = col_date("%Y.%m.%d"),
                                   WhiteTitle = col_character(),
                                   BlackTitle = col_character(),
                                   WhiteElo = col_character(),
                                   BlackElo = col_character(),
                                   FEN = col_character())) %>%
    dplyr::select(Username, Event, White, Black, Result, UTCDate, UTCTime,
                  WhiteElo, BlackElo, Variant, TimeControl, Termination) %>%
    mutate(WhiteElo = parse_number(if_else(WhiteElo == "?", NA, WhiteElo)),
           BlackElo = parse_number(if_else(BlackElo == "?", NA, BlackElo)))
  dat
}
get_hist <- function(user, games, prev_n) {
  if(prev_n == 1){
    hist_games <- games %>% 
      #filter(White == user | Black == user) %>% 
      filter(Username == user) |> #need to filter for username, since if we do white or black can add extra games since username can be the focal player or opponent
      arrange(UTCDate, UTCTime) %>% 
      mutate(focal_white = ifelse(Username == White, 1, 0)) %>% 
      dplyr::select(White:BlackElo, focal_white) %>% 
      mutate(focal_result = case_when(
        (focal_white == 1 & Result == "1-0") ~ 1,
        (focal_white == 0 & Result == "0-1") ~ 1,
        (Result == "1/2-1/2") ~ 0.5,
        .default = 0
      )) %>% 
      mutate(focal_win_prop = focal_result)
    
  }
  else{
    hist_games <- games %>% 
      #filter(White == user | Black == user) %>% 
      filter(Username == user) |>
      arrange(UTCDate, UTCTime) %>% 
      mutate(focal_white = ifelse(Username == White, 1, 0)) %>% 
      dplyr::select(White:BlackElo, focal_white) %>% 
      mutate(focal_result = case_when(
        (focal_white == 1 & Result == "1-0") ~ 1,
        (focal_white == 0 & Result == "0-1") ~ 1,
        (Result == "1/2-1/2") ~ 0.5,
        .default = 0
      )) %>% 
      mutate(focal_win_prop = c(cumsum(focal_result[1:(prev_n - 1)])/(1:(prev_n -1)), 
                                roll_mean(focal_result, n = prev_n)))
  }
  
  hist_games
}

### load in the data to use

users = readRDS(file = here("results", "model fits", "users", "users_bullet_gm.RDS"))
data_path = here("data", "lichessGrandmasters")
files = list.files(data_path)
lichess_data = files %>%
  map_dfr(~read_player(data_path, .x))



## restrict to rated rapid and shorter here
## this also removes the NAs, which makes sense

small_data <- lichess_data %>%
  mutate(Event = tolower(Event)) |>
  # filter(Event == "Rated Bullet game") %>%
  filter(TimeControl == "60+0") %>%
  filter(Variant == "Standard") %>%
  filter(grepl("rated bullet game", Event))

#remove duplicated games
small_data = small_data[!duplicated(small_data),]

# ## what time length should be
# small_data <- lichess_data %>%
#   mutate(Event = tolower(Event)) |>
#   filter(TimeControl == "180+0") %>%
#   filter(Variant == "Standard") %>%
#   filter(grepl("rated blitz game", Event))


## when players play less than 10 games
## otherwise not needed
users <- small_data %>%
  group_by(Username) %>%
  tally() %>%
  filter(n >= 10) %>%
  pull(Username)

# saveRDS(users, file = paste0(save_path, "users_bullet.RDS"))
# saveRDS(users, file = paste0(save_path, "users_blitz.RDS"))


tidy_games <- map_dfr(users, get_hist, small_data, prev_n = 1) %>% 
  as_tibble()

init_data <- tidy_games %>%
  mutate(WhiteElo = as.numeric(WhiteElo),
         BlackElo = as.numeric(BlackElo)) %>%
  mutate(focal_user = ifelse(focal_white == 1, White, Black)) %>%
  filter(White %in% users & Black %in% users) %>%
  mutate(elo_diff = ifelse(focal_white == 1,
                           WhiteElo - BlackElo, BlackElo - WhiteElo)) %>%
  mutate(focal_id = match(focal_user, users)) %>%
  mutate(opponent = ifelse(focal_white == 1, Black, White)) %>%
  mutate(opponent_id = match(opponent, users)) %>%
  dplyr::select(focal_user, focal_id, opponent_id, focal_white,
                focal_win_prop, elo_diff, focal_result) %>%
  group_by(focal_id) %>%
  mutate(ave_prop = lag(focal_win_prop, default = 0) - mean(focal_result)) %>%
  filter(focal_result != 0.5)


### then fit the models

stan_data_ave <- list(N = nrow(init_data),
                      J = length(users),
                      y = init_data$focal_result,
                      id = init_data$focal_id,
                      colour = init_data$focal_white,
                      win_prop = init_data$ave_prop,
                      id_focal = init_data$focal_id, #for opponent effect
                      id_opp = init_data$opp_id) #for opponent effect



#saveRDS(object = stan_data_ave, file = "C:/Users/adamg/Desktop/Thesis Fall 2024/cluster code and data/gm_middle_1000_stan_data.RDS")


stan_file <- here("code", "stan", "opponent_effect.stan")
fit = "fit_opp_effect"

mod <- cmdstan_model(stan_file)

mod_fit <- mod$sample(data = stan_data_ave,
                      seed = 123,
                      chains = 4,
                      parallel_chains = 4,
                      refresh = 100)


## save the stan fit as not actually that large here,
## when no generated quantities

mod_fit$save_object(file = here("results", "model fits", "opponent_effect", paste0(fit, "_gm.RDS")))




### PLOTTING ###

facet_labels <- as_labeller(c(
  mu_beta = "mu[beta]",
  gamma = "gamma",
  sigma_a = "sigma[a]",
  sigma_b = "sigma[b]",
  sigma_g = "sigma[g]"
), label_parsed)



vars = mod_fit$summary() %>% pull(variable)
vars = vars[!startsWith(vars, "beta")]
vars = vars[!startsWith(vars, "alpha")]
vars = vars[!startsWith(vars, "lp")]
vars = vars[!startsWith(vars, "L_Omega")]
vars = vars[!startsWith(vars, "nu")]
vars

global_gm_bullet_prev <- mod_fit$draws(vars) |> 
  as_draws_df() |> 
  pivot_longer(cols = -c(.chain, .iteration, .draw)) |> 
  ggplot(aes(value)) +
  geom_histogram() +
  facet_wrap(~name, ncol = 5, scales = "free",
             labeller = labeller(name = facet_labels)) +
  labs(x = element_blank(), y = element_blank()) +
  theme_single_y() +
  scale_x_continuous(breaks = breaks_pretty(n = 2)) 

global_gm_bullet_prev

# ggsave(plot = global_gm_bullet_prev,
#        filename = here("results", "bot figures", fit, "global_fixed.png"), dpi = 1000,
#        height = 4, width = 8)


winner_pars = mod_fit$draws() |> 
  as_draws_df() |>
  dplyr::select(starts_with("beta[")) |>
  pivot_longer(cols = everything()) |>
  mutate(id = sub(".*\\[(\\d+)\\]", "\\1", name)) |>
  mutate(id = as.numeric(id))

winner_pars$player_name = users[winner_pars$id]

winner_pars_plot <- winner_pars |> 
  ggplot(aes(y = reorder(player_name, value, FUN = median), x = value)) +
  stat_histinterval() +
  labs(x = "Estimated Experiential Effect", y = element_blank(), 
       title = "GMs") +
  geom_vline(aes(xintercept = 0), col = "red", linetype = "dashed") +
  theme_single_y()
winner_pars_plot

ggsave(plot = winner_plot_bot,
       filename = here("results", "temp model figures", fit, "winner.png"), dpi = 1000,
       height = 6, width = 7)






player_pars = mod_fit$draws() |> 
  as_draws_df() |>
  dplyr::select(starts_with("alpha[")) |>
  pivot_longer(cols = everything()) |>
  mutate(id = sub(".*\\[(\\d+)\\]", "\\1", name)) |>
  mutate(id = as.numeric(id))

player_pars$player_name = users[player_pars$id]

player_pars_plot <- player_pars |> 
  ggplot(aes(y = reorder(player_name, value, FUN = median), x = value)) +
  stat_histinterval() +
  labs(x = "Estimated Player Effect", y = element_blank(), 
       title = "GMs") +
  geom_vline(aes(xintercept = 0), col = "red", linetype = "dashed") +
  theme_single_y()
player_pars_plot

ggsave(plot = player_pars_plot,
       filename = here("results", "temp model figures", fit, "player.png"), dpi = 1000,
       height = 6, width = 7)


