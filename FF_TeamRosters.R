library(ffscrapr)
library(dplyr)
library(purrr)
library(glue)
library(tidyverse)
library(tidyr)


sport <- "NFL"
league_id <- 312113
season <- 2023
week <- 1






df_rosters <- fleaflicker_getendpoint("FetchLeagueRosters",
                                      sport = "NFL",
                                      external_id_type = "SPORTRADAR",
                                      league_id = league_id
) %>%
  purrr::pluck("content", "rosters") %>%
  tibble::tibble() %>%
  tidyr::unnest_wider(1) %>%
  tidyr::hoist("team", "franchise_id" = "id", "franchise_name" = "name") %>%
  dplyr::select(-"team") %>%
  tidyr::unnest_longer("players") %>%
  tidyr::hoist("players", "proPlayer") %>%
  tidyr::hoist("proPlayer",
               "player_id" = "id",
               "player_name" = "nameFull",
               "pos" = "position",
               "team" = "proTeamAbbreviation",
               "externalIds"
  ) %>%
  dplyr::mutate(sportradar_id = purrr::map_chr(.data$externalIds, purrr::pluck, 1, "id", .default = NA)) %>%
  dplyr::select(dplyr::any_of(c(
    "franchise_id",
    "franchise_name",
    "player_id",
    "player_name",
    "pos",
    "team",
    "sportradar_id"
  )))

