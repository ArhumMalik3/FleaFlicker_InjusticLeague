library(ffscrapr)
library(dplyr)
library(purrr)
library(glue)
library(tidyverse)
library(tidyr)


sport <- "NFL"
league_id <- 312113
season <- 2020
week <- 1

standings <- fleaflicker_getendpoint("FetchLeagueStandings",
                                     league_id = league_id,
                                     season = season,
                                     sport = "NFL")

df_standings <- standings %>%
  purrr::pluck("content", "divisions") %>%
  tibble::tibble() %>%
  tidyr::hoist(1, "division_id" = "id", "division_name" = "name", "teams") %>%
  tidyr::unnest_longer("teams") %>%
  tidyr::hoist(
    "teams",
    "franchise_id" = "id",
    "franchise_name" = "name",
    "recordOverall",
    "points_for" = "pointsFor",
    "points_against" = "pointsAgainst"
  ) %>%
  dplyr::mutate(dplyr::across(c("points_for", "points_against"), purrr::map_dbl, purrr::pluck, "value")) %>%
  tidyr::hoist("recordOverall", "h2h_wins" = "wins", "h2h_losses" = "losses", "h2h_ties" = "ties") %>%
  dplyr::mutate(
    dplyr::across(dplyr::starts_with("h2h"), tidyr::replace_na, 0),
    h2h_winpct = (.data$h2h_wins / (.data$h2h_wins + .data$h2h_losses + .data$h2h_ties)) %>% round(3)
  ) %>%
  dplyr::select(
    dplyr::starts_with("division"),
    dplyr::starts_with("franchise"),
    dplyr::starts_with("h2h"),
    dplyr::starts_with("points")
  ) %>%
  dplyr::arrange(dplyr::desc(.data$h2h_winpct))

df_standings$season <- season

saveRDS(df_standings, file = "df_standings2020.rds")