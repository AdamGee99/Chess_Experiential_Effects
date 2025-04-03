library(tidyverse)

#helper functions
source(here("code", "helper functions", "helper.R"))
source(here("code", "helper functions", "plot_templates.R"))


#import data from 1700-1900 and gm cohorts
data_path_gm = here("data", "lichessGrandmasters")
data_path_17_19 = here("data", "lichess1700-1900")
files_gm = list.files(data_path_gm, pattern = ".csv")
files_17_19 = list.files(data_path_17_19, pattern = ".csv")

lichess_data_gm = files_gm %>%
  map_dfr(~read_player(data_path_gm, .x))
lichess_data_17_19 = files_17_19 %>%
  map_dfr(~read_player(data_path_17_19, .x))

#combine
lichess_data = rbind(lichess_data_17_19, lichess_data_gm)

#selecting 3 users from each cohort that have around 10,000 games
users = c("larrywheels", "SeanBambic", "dewang_007", #1700-1900s
          "DrNykterstein", "alireza2003", "BlueGreensun") #gms

#data for plot
plot_data = lichess_data %>%
  mutate(Event = tolower(Event),
         cohort = ifelse(Username %in% users[1:3], "1700-1900", "GM")) %>%
  filter(Event == "rated bullet game", Variant == "Standard", Username %in% users) %>%
  mutate(focal_rating = ifelse(Username == White, WhiteElo, BlackElo)) %>%
  group_by(Username) %>%
  arrange(UTCDate, UTCTime, .by_group = TRUE) %>%
  mutate(game = row_number(),
         Username = case_when( #rename magnus and alireza usernames
           Username == "DrNykterstein" ~ "Magnus Carlsen",
           Username == "alireza2003" ~ "Alireza Firouzja",
           .default = Username
         ),
         Username = factor(Username, levels = c("Magnus Carlsen", "Alireza Firouzja", "BlueGreensun", #for order in legend
                                                "SeanBambic", "dewang_007", "larrywheels"))) %>% 
  ungroup() %>%
  select(Username, cohort, game, focal_rating)
  
#plotting
ggplot(data = plot_data, mapping = aes(x = game, y = focal_rating, colour = Username)) +
  geom_line() +
  labs(x = "Game Number", y = "Rating") + 
  theme_single_y_legend()



