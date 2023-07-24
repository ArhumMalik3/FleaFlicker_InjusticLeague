library(tidyverse)
library(ggplot2)
library(shiny)
library(shinythemes)
library(data.table)
library(tidyr)
library(httr)
library(jsonlite)
library(plumber)
library(dplyr)
library(DT)
library(rsconnect)

#Pull out team owner data later, but for now this works as a translation
teamIdVec <- c(1573611, 1573637, 1573726, 1573740, 1574357, 1574358, 1574361, 1574450, 1574493, 1574908)
names(teamIdVec) <- c("Matt", "Arhum", "Jack", "Alex", "Anzal", "Trn", "Blake", "Luke", "Pranav", "Hassan")

df <- read.csv("df_draftboardAll_July_19_2023.csv")

df <- df[,-1]

#Adding column notation
df$Owner <- vector("character", length = nrow(df))

for (i in 1:nrow(df)) {
  index <- which(teamIdVec == df$team[i])
  df$Owner[i] <- names(teamIdVec)[index]
}

df <- df[,-1]
df <- df[,-1]

colnames(df)[7] <- "KTC_Value"

df <- df[,-8]
df <- df[,-8]
df <- df[,-8]

df <- df[order(-df$KTC_Value),]
# UI ----
ui <- fluidPage(
  
  tags$head(
    tags$style(HTML(".inputFormat {border:2px solid black; padding: 10px; overflow-x: scroll;
                      overall-y: hidden; height: 50vh;}")),
    tags$style(HTML(".centerTitle {text-align: center"))
  ),
  # Title ----
  fluidRow(
    wellPanel(
      h3(class = "centerTitle", "KTC Value Graph")
    ),
    column(3, offset = 1.5,
           h3("Selection Inputs"),
           div(class = "inputFormat",
               checkboxGroupInput("owner", label = "Owner",
                                  choices = list("Trn" = "Trn",
                                                 "Matt" = "Matt",
                                                 "Jack" = "Jack",
                                                 "Arhum" = "Arhum",
                                                 "Pranav" = "Pranav",
                                                 "Blake" = "Blake",
                                                 "Alex" = "Alex",
                                                 "Luke" = "Luke",
                                                 "Hassan" = "Hassan",
                                                 "Anzal" = "Anzal"),
                                  selected = c("Trn", "Matt", "Jack",
                                               "Arhum", "Pranav", "Blake",
                                               "Alex","Luke","Hassan","Anzal")),
               checkboxGroupInput("position", label = "Position",
                                  choices = list("QB" = "QB",
                                                 "RB" = "RB",
                                                 "WR" = "WR",
                                                 "TE" = "TE"),
                                  selected = c("QB", "RB", "WR", "TE")),
               checkboxGroupInput("year", label = "Year Drafted",
                                  choices = list(2020,2021,2022,2023),
                                  selected = 2023)
           )
    ),
    column(6,
           plotOutput(outputId = "draftPlot")
    ),
    column(3, offset = 1.5,
           h3("Slider Inputs"),
           div(class = "inputFormat",
               
               sliderInput("ktc_value", label = "KTC Value", min = 0, max = 9999,
                           value = c(0,9999)),
               sliderInput("round", label = "Round Range", min = 1, max = 7,
                           value = c(1,7)),
               sliderInput("overall", label = "Pick Range", min = 1,
                           max = 70, value = c(1,70))
           )
    )
  ),
  fluidRow(
    wellPanel(
      h3(class = "centerTitle", "Averages")
    ),
    column(12,
           div(align = "center",
               tableOutput(outputId = "avgTable"))
    )
  ),
  # Main panel for displaying outputs ----
  fluidRow(
    wellPanel(
      h3(class = "centerTitle", "Data Table")
    ),
    column(12,
           div(align = "center", tags$style(h3("Sample Table"))),
           DTOutput(outputId = "dfTable")
    )  
  )
)




# Define server logic required to create a table ----
server <- function(input, output, session) {
  
  output$draftPlot <- renderPlot({
    
    #Filtering data frame based on inputs
    df <- filter(df, Owner %in% input$owner)
    df <- filter(df, Position %in% input$position)
    df <- filter(df, Year %in% input$year)
    df <- filter(df, KTC_Value >= input$ktc_value[1] & KTC_Value <= input$ktc_value[2])
    df <- filter(df, Round >= input$round[1] & Round <= input$round[2])
    df <- filter(df, Overall >= input$overall[1] & Overall <= input$overall[2])
    
    plot <- ggplot(df, aes(x= Overall, y = KTC_Value, col = Position)) +
      geom_point() +
      geom_smooth(method = "loess", se = F, aes(group = Position))
    labs(x = "Pick Number", y = "KTC Value", caption = "Based on data from KeepTradeCut",
         title = "Overall Pick x KTC")+
      theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5))
    
    plot
    
  })
  
  output$avgTable <- renderTable({
    
    #Filtering data frame based on inputs
    df <- filter(df, Owner %in% input$owner)
    df <- filter(df, Position %in% input$position)
    df <- filter(df, Year %in% input$year)
    df <- filter(df, KTC_Value >= input$ktc_value[1] & KTC_Value <= input$ktc_value[2])
    df <- filter(df, Round >= input$round[1] & Round <= input$round[2])
    df <- filter(df, Overall >= input$overall[1] & Overall <= input$overall[2])
    
    params <- c("Round", "Overall", "KTC_Value")
    
    avgs <- colMeans(df[params])
    avgs <- t(avgs)
    names(avgs) <- c("Average Round", "Average Pick", "Average KTC Value")
    
    avgs
  })
  
  output$dfTable <- renderDT({
    
    #Filtering data frame based on inputs
    df <- filter(df, Owner %in% input$owner)
    df <- filter(df, Position %in% input$position)
    df <- filter(df, Year %in% input$year)
    df <- filter(df, KTC_Value >= input$ktc_value[1] & KTC_Value <= input$ktc_value[2])
    df <- filter(df, Round >= input$round[1] & Round <= input$round[2])
    df <- filter(df, Overall >= input$overall[1] & Overall <= input$overall[2])
    
    datatable(df)
    
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)


# selectInput("team", label = "NFL Team",
#               choices = list("ARI" = "ari", "ATL" = "atl",
#               "BAL" = "bal", "BUF" = "buf", "CAR" = "car",
#               "CHI" = "chi", "CIN" = "cin", "CLE" = "cle",
#               "DAL" = "dal", "DEN" = "den", "DET" = "det",
#               "FA" = "fa", "GB" = "gb", "HOU" = "hou",
#               "IND" = "ind", "JAC" = "jac", "KC" = "kc",
#               "LAC" = "lac", "LAR" = "lar", "LV" = "lv",
#               "MIA" = "mia", "MIN" = "min", "NE" = "ne",
#               "NO" = "no", "NYG" = "nyg", "NYJ" = "nyj",
#               "PHI" = "phi", "PIT" = "pit", "SEA" = "sea",
#               "SF" = "sf", "TB" = "tb", "TEN" = "ten", "WAS" ="was"),
#               selected = c("ari", "atl", "bal", "buf", "car",
#               "chi", "cin", "cle", "dal", "den", "det", "fa",
#               "gb", "hou", "ind", "jac", "kc", "lac", "lar",
#               "lv", "mia", "min", "ne", "no", "nyg", "nyj",
#               "phi", "pit", "sea", "sf", "tb", "ten", "was"))

