library(tidyverse)
library(ffscrapr)
library(dplyr)
library(purrr)
library(glue)

sport <- "NFL"
league_id <- 312113
season <- 2023

#Fetches API
response_draftboard <- fleaflicker_getendpoint("FetchLeagueDraftBoard",
                                               sport = sport, 
                                               league_id = league_id,
                                               season = season)

#Grabs team, player, position, team, and pick data from API
df_draftboard <- response_draftboard %>%
  purrr::pluck("content", "orderedSelections")%>%
  tidyr::tibble()%>%
  unnest_wider(1)%>%
  dplyr::mutate_at("team", map_chr, pluck, "id")%>%
  tidyr::hoist("player", "proPlayer")%>%
  tidyr::hoist("proPlayer", "Name" = "nameFull", "Team" = "proTeamAbbreviation", "Position" = "position")%>%
  tidyr::hoist("slot", "Round" = "round", "Overall" = "overall")


#Gets out columns of lists from df_draftboard
i <- 1

while (i <= ncol(df_draftboard)) {
  if(class(df_draftboard[[i]]) != "integer" & class(df_draftboard[[i]]) != "character"){
    df_draftboard[,i] <- NULL
    i <- i - 1
  }
  i <- i + 1
}

#Takes out final column of colors
df_draftboard <- df_draftboard[,-ncol(df_draftboard)]
df_draftboard$Year <- season

write.csv(df_draftboard, "df_draftboard2023.csv")

              
              
