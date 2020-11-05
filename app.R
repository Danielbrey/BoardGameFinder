library(shiny)
library(tidyverse)
library(rvest)

boardgames.data <- read_csv("BoardGames.csv")

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
                 max = 450),
    sliderInput(inputId = "choose.max.playtime",
                label = "Maximum playtime",
                value = 60,
                min = 1,
                max = 450),
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
        boardgames.data %>% 
            filter(details.maxplayers >= input$choose.num.players,
                   details.minplayers <= input$choose.num.players,
                   details.playingtime <= input$choose.max.playtime,
                   details.playingtime >= input$choose.min.playtime,
                   str_detect(boardgames.data$attributes.boardgamecategory,
                              input$choose.category) == TRUE) %>% 
            arrange(-stats.average) %>% 
            select(details.name, details.description,
                   stats.average, stats.averageweight) %>% 
            head(5)
    })
    
    output$plot1 <- renderPlot({
        boardgames.data %>% 
            filter(str_detect(boardgames.data$attributes.boardgamecategory,
                              input$choose.category) == TRUE) %>% 
            ggplot(aes(x = stats.average))+
            geom_density()+
            labs(title = "Rating Distribution for class")
    })
    
    output$plot2 <- renderPlot({
        boardgames.data %>% 
            filter(str_detect(boardgames.data$attributes.boardgamecategory,
                              input$choose.category) == TRUE) %>% 
            ggplot(aes(x = stats.averageweight))+
            geom_density()+
            labs(title = "Weight Distribution for class")
    })
}


shinyApp(ui = ui, server = server)
