library(rvest)
library(janitor)
library(tidyverse)

theme_set(theme_bw())

position_graph <- function(pos, string) {
  
  team_url <- read_html("https://www.msn.com/en-us/Sports/nfl/team-stats")
  player_url <- read_html("https://www.pro-football-reference.com/years/2020/fantasy.htm")
  team_names <- read.csv("https://gist.githubusercontent.com/cnizzardini/13d0a072adb35a0d5817/raw/dbda01dcd8c86101e68cbc9fbe05e0aa6ca0305b/nfl_teams.csv")
  
  team <- team_url %>%
    html_table(fill = T) %>%
    purrr::pluck(1) %>%
    clean_names() %>%
    select(team_3, pts_gm, pass_gm, rush_gm) %>%
    rename(team = team_3,
           team_ppg = pts_gm
    ) %>%
    mutate(team = gsub("KC", "KAN",team),
           team = gsub("GB", "GNB", team),
           team = gsub("TB", "TAM", team), 
           team = gsub("OAK", "LVR", team), 
           team = gsub("NO", "NOR", team), 
           team = gsub("SF", "SFO", team),
           team = gsub("NE", "NWE", team),
    ) 
  
  player <- player_url %>%
    html_table(fill = T) %>%
    purrr::pluck(1) %>%
    clean_names() %>%
    filter(x_4 == pos) %>%
    filter(as.numeric(games)>5) %>%
    mutate(
      ppg = as.numeric(fantasy_2)/as.numeric(games),
      player = gsub("[[:punct:]]", "", x_2)
    ) %>%
    select(player, x_3, ppg) %>%
    rename(team = x_3,
           player_ppg = ppg
    )
  
  points_per_game <- left_join(team, player, by = c("team"="team")) %>%
    group_by(team) %>%
    top_n(n=string)  %>%
    slice(which.min(player_ppg))  %>%
    mutate(team_ppg = as.numeric(team_ppg),
           player_ppg = as.numeric(player_ppg)
    )
 
  correlation <- cor(points_per_game$team_ppg,points_per_game$player_ppg)
  
  ggplot(points_per_game, aes( x=team_ppg, y=player_ppg)) + 
    geom_point(size=5, color="blue") + 
    geom_text(aes(label=player), size=3, pos = position_nudge(y = -0.5), color="blue") + 
    geom_smooth() +
    ylab("PPR Fantasy PPG") +
    xlab("Team PPG") +
    ggtitle(
      paste(
        pos,string," Points vs Team Scoring (Correlation: ", format(round(correlation, 2), nsmall = 2) , ")",
        sep = ""
      )
    ) 
  
}

position_graph("QB",1)
position_graph("RB",1)
position_graph("RB",2)
position_graph("WR",1)
position_graph("WR",2)
position_graph("WR",3)
position_graph("TE",1)

