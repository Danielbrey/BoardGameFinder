library(shiny)
library(tidyverse)
library(rvest)

#devtools::install_github("9thcirclegames/bgg-analysis")

boardgames.data <- read_csv("BoardGames.csv")

bg.data.update <- boardgames.data %>% 
    filter(attributes.boardgamedesigner != "(Uncredited)")

ui <- fluidPage(
    numericInput(inputId = "choose.num.players",
                 label = "Number of players",
                 value = 2,
                 min = 1,
                 max = 50,
                 step = 1),
    sliderInput(inputId = "choose.min.playtime",
                 label = "Minimum playtime",
                 value = 30,
                 min = 1,
                 max = 420),
    sliderInput(inputId = "choose.max.playtime",
                label = "Maximum playtime",
                value = 60,
                min = 1,
                max = 420),
    selectInput(inputId = "choose.category",
                label = "Board game category",
                choices = c("Political",
                            "Dice",
                            "Bluffing",
                            "Wargame",
                            "Card Game",
                            "Party",
                            "Economic",
                            "Abstract Strategy")),
    tableOutput(outputId = "table1"),
    plotOutput(outputId = "plot1"),
    plotOutput(outputId = "plot2")
    
    
)

server <- function(input, output) {

    output$table1 <- renderTable({
        bg.custom <- boardgames.data %>% 
            filter(game.type == "boardgame",
                   attributes.boardgamedesigner != "(Uncredited)", #or get rid of 0's altogether?
                   details.maxplayers >= input$choose.num.players,
                   details.minplayers <= input$choose.num.players,
                   details.maxplaytime <= input$choose.max.playtime,
                   details.minplaytime >= input$choose.min.playtime,
                   str_detect(boardgames.data$attributes.boardgamecategory,
                              input$choose.category) == TRUE) %>% 
            arrange(-stats.owned) %>% 
            select(details.name, details.description,
                   stats.average, stats.averageweight) %>% 
            head(5)
    })
    
    output$plot1 <- renderPlot({
        boardgames.data %>% 
            filter(game.type == "boardgame",
                   attributes.boardgamedesigner != "(Uncredited)",
                   str_detect(boardgames.data$attributes.boardgamecategory,
                              input$choose.category) == TRUE) %>% 
            ggplot(aes(x = stats.average))+
            geom_density()+
            labs(title = "Rating Distribution for class")
    })
    
    output$plot2 <- renderPlot({
        boardgames.data %>% 
            filter(game.type == "boardgame",
                   attributes.boardgamedesigner != "(Uncredited)",
                   str_detect(boardgames.data$attributes.boardgamecategory,
                              input$choose.category) == TRUE) %>% 
            ggplot(aes(x = stats.averageweight))+
            geom_density()+
            labs(title = "Weight Distribution for class")
    })
}


shinyApp(ui = ui, server = server)
