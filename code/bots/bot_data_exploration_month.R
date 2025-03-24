#### MARCH 3, 2025 ####

#Bot data exploration
#Exploring bot data from CCRL for Dec, 2024 - not single engine here, its the cumulative games from every engine played that month

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

source(here("code", "helper functions", "helper.R"))
source(here("code", "helper functions", "plot_templates.R"))


#testing for months
data_path = here("data", "bot data", "TCEC Div P")
files = list.files(data_path)
#just do january and february - these won't be many games
engine_blitz_raw =  files %>% map_dfr(~read.pgn(paste0(data_path, "/", .x), add.tags = c("WhiteElo", "BlackElo", "Date", "Time")))

#' these games are the same format as the players
# engine_blitz_raw = read.csv(here("data", "lichessBots", "lichess_bot_games.csv"))
# users = engine_blitz_raw$Username %>% unique() %>% sort()

#can do multiple tournaments - but then theres the argument that they're different engines... between tournaments, they can get updated, etc...
#but isn't this like humans in a sense...

#for now, just consider all different versions of an engine as the same engine
engine_blitz_raw = engine_blitz_raw %>%
  mutate(White = sub("^(\\S+).*", "\\1", White)) %>%
  mutate(Black = sub("^(\\S+).*", "\\1", Black))

users = engine_blitz_raw$White %>% unique() %>% sort()



# #filter for engines that have over 100 games played for both black and white
# white_users = engine_blitz_raw %>% group_by(White) %>% summarise(n = n()) %>% filter(n >= 100) %>% pull(White)
# black_users = engine_blitz_raw %>% group_by(Black) %>% summarise(n = n()) %>% filter(n >= 100) %>% pull(Black)
# users = intersect(white_users, black_users) 



#cleaning the openings and adding win rate
engine_blitz_white = engine_blitz_raw %>% 
  filter(White %in% users) %>% #filter and add for focal usernames
  mutate(Username = White)

engine_blitz_black = engine_blitz_raw %>%
  filter(Black %in% users) %>%
  mutate(Username = Black)

engine_blitz = bind_rows(engine_blitz_white, engine_blitz_black) %>%
  mutate(across(Round, as.numeric)) %>%
  mutate(across(Date, ymd)) %>%
  group_by(Username) %>%
  arrange(Event, Date, Round, .by_group = TRUE) %>%
  ungroup()


tidy_games <- map_dfr(users, get_hist, engine_blitz, prev_n = 1) %>% 
  as_tibble()


init_data <- tidy_games %>%
  mutate(WhiteElo = as.numeric(WhiteElo),
         BlackElo = as.numeric(BlackElo)) %>%
  mutate(focal_user = ifelse(focal_white == 1, White, Black)) %>%
  mutate(rating_diff = ifelse(focal_white == 1,
                              WhiteElo - BlackElo, BlackElo - WhiteElo)) %>%
  mutate(focal_id = match(focal_user, users)) %>%
  dplyr::select(focal_user, Round, focal_id, focal_white,
                focal_win_prop, rating_diff, focal_result, White, Black, WhiteElo, BlackElo) %>%
  group_by(focal_id) %>%
  mutate(ave_prop = lag(focal_win_prop, default = 0) - mean(focal_result)) %>%
  mutate(opp_id = ifelse(focal_white == 1, match(Black, users), match(White, users))) %>%
  filter(focal_result != 0.5) %>%
  ungroup()



init_data %>% filter(focal_user == users[1]) %>%
  mutate(prev_result  = as.numeric((round(ave_prop*2) + 1) /2)) %>%
  mutate(result = focal_result) %>%
  select(result, prev_result) %>%
  table()

#I think it's because its a closed system. If you get a win, you've likely just played someone bad, then next game you're more likely to play someone good
#I'm pretty sure the tournament is designed so people don't play eachother in random order. There is a fixed order
  #Eg, Berserk will play stockfish every 6 games...
#the order repeats...
tidy_games %>% filter(Username=="AllieStein") %>% mutate(opp = ifelse(focal_white == 1, Black, White)) %>% pull(opp)

#the model isn't picking up winner effects, it's picking up when the model will win from the order from the games

#need to find data set where the order/selection of opponents is random...

#this is probably whats going on with the other bot data - might be a feature to using tournament data?
  #thought for the others it was positive winner effects... os maybe something different going on
#is there a dataset for bots that randomly selects opponent out of a pool of players? like our other player data
#can we do this for bot v player data?


test = init_data %>% 
  mutate(prev_result  = as.numeric((round(ave_prop*2) + 1) /2)) %>%
  filter(prev_result != 0.5)


stan_data_ave <- list(N = nrow(init_data),
                      J = length(users),
                      y = init_data$focal_result,
                      id = init_data$focal_id,
                      colour = init_data$focal_white,
                      elo = init_data$rating_diff,
                      win_prop = init_data$ave_prop,
                      id_focal = init_data$focal_id, #for opponent effect
                      id_opp = init_data$opp_id) #for opponent effect



stan_file <- here("code", "stan", "final_model_scale_priors_NEW.stan")
#stan_file <- here("code", "stan", "opponent_effect.stan")
fit = "fit_divP"

mod <- cmdstan_model(stan_file)

bot_fit <- mod$sample(data = stan_data_ave,
                      seed = 123,
                      chains = 4,
                      parallel_chains = 4,
                      refresh = 100)

# bot_fit$save_object(file = here("results", "model fits", "bot fits", paste0(fit, ".RDS")))
# 
# bot_fit = readRDS(file = here("results", "model fits", "bot fits", paste0(fit, ".RDS")))

facet_labels <- as_labeller(c(
  mu_beta = "mu[beta]",
  gamma1 = "gamma[1]",
  gamma2 = "gamma[2]",
  gamma3 = "gamma[3]",
  sigma_1 = "sigma[1]",
  `tau[1]` = "tau[1]",
  `tau[2]` = "tau[2]",
  sigma_g1 = "sigma[g[1]]",
  sigma_g2 = "sigma[g[2]]",
  sigma_g3 = "sigma[g[3]]"
), label_parsed)

# facet_labels <- as_labeller(c(
#   mu_beta = "mu[beta]",
#   gamma = "gamma",
#   sigma_a = "sigma[a]",
#   sigma_b = "sigma[b]",
#   sigma_g = "sigma[g]"
# ), label_parsed)



vars = bot_fit$summary() %>% pull(variable)
vars = vars[!startsWith(vars, "beta")]
vars = vars[!startsWith(vars, "lp")]
vars = vars[!startsWith(vars, "L_Omega")]
vars = vars[!startsWith(vars, "nu")]
vars = vars[!startsWith(vars, "alpha")]
vars

global_gm_bullet_prev <- bot_fit$draws(vars) |> 
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



#winner
bot_pars = bot_fit$draws() |> 
  as_draws_df() |>
  dplyr::select(starts_with("beta[")) |>
  pivot_longer(cols = everything()) |>
  mutate(param = stringr::str_extract(name, pattern = "\\d"),
         id = stringr::str_extract(name, pattern = "\\d+]"),
         id = stringr::str_replace(id, "\\]", ""),
         player_id = paste0("beta[", id, "]")) |> 
  mutate(id = as.numeric(id),
         param = as.numeric(param))
bot_pars$player_name = users[bot_pars$id]



winner_plot_bot <- bot_pars |> 
  filter(param == 2) |> 
  ggplot(aes(y = reorder(player_name, value, FUN = median), x = value)) +
  stat_histinterval() +
  labs(x = "Estimated Experiential Effect", y = element_blank(), 
       title = "Bots") +
  geom_vline(aes(xintercept = 0), col = "red", linetype = "dashed") +
  theme_single_y()
winner_plot_bot

# ggsave(plot = winner_plot_bot,
#        filename = here("results", "bot figures", fit, "winner.png"), dpi = 1000,
#        height = 6, width = 7)


#player
int_plot_bot <- bot_pars |> 
  filter(param == 1) |> 
  ggplot(aes(y = reorder(player_name, value, FUN = median), x = value)) +
  stat_histinterval() +
  labs(x = "Estimated Player Effect", y = element_blank(), 
       title = "Bots") +
  geom_vline(aes(xintercept = 0), col = "red", linetype = "dashed") +
  theme_single_y()
int_plot_bot

# ggsave(plot = int_plot_bot,
#        filename = here("results", "bot figures", fit,  "int.png"), dpi = 1000,
#        height = 6, width = 7)
