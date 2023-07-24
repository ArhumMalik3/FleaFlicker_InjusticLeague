library(ffscrapr)
library(dplyr)
library(purrr)
library(glue)
library(tidyverse)
library(tidyr)


sport <- "NFL"
league_id <- 312113
season <- 2022
week <- 1

response_scoreboard <- fleaflicker_getendpoint("FetchLeagueScoreboard",
                                               sport = sport, 
                                               league_id = league_id, 
                                               season = season, 
                                               scoring_period = week)


df_scoreboard <- response_scoreboard %>% 
  purrr::pluck("content","games") %>% 
  tibble::tibble() %>% 
  tidyr::unnest_wider(1) %>% 
  dplyr::mutate_at(c("away","home"),purrr::map_chr,purrr::pluck,"name") %>% 
  dplyr::mutate_at(c("homeScore","awayScore"),purrr::map_dbl,purrr::pluck,"score","value")

onegame_lineups <- fleaflicker_getendpoint(
  "FetchLeagueBoxscore",
  sport = sport, 
  league_id = league_id, 
  # example for one call, but you can call this in a map or loop! 
  fantasy_game_id = df_scoreboard$id[[1]], 
  scoring_period = week) %>% 
  purrr::pluck('content','lineups') %>% 
  tibble::tibble() %>% 
  tidyr::unnest_wider(1) %>% 
  tidyr::unnest_longer('slots') %>% 
  tidyr::unnest_wider('slots') %>% 
  tidyr::pivot_longer(c("home","away"),names_to = "franchise",values_to = "player") %>% 
  tidyr::unnest_wider('player')

head(df_scoreboard)