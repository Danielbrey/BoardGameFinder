library(shiny)
library(tidyverse)
library(rvest)
library(shinydashboard)
library(DT)
library(shinythemes)
#install.packages("shinythemes")
#install.packages("DT")
boardgames.data <- read_csv("boardgames copy.csv")

boardgames.data$details.description <- str_replace_all(boardgames.data$details.description,
                                                       "&#[:digit:]+;",
                                                       " ")
boardgames.data$details.description <- str_replace_all(boardgames.data$details.description,
                                                       "&quot",
                                                       "")
boardgames.data$details.description <- str_replace_all(boardgames.data$details.description,
                                                       "&rsquo;",
                                                       "'")
boardgames.data$details.description <- str_replace_all(boardgames.data$details.description,
                                                       "&mdash;",
                                                       "-")

ui <- dashboardPage(
    dashboardHeader(title = "Board Game Finder"),
    
    #These inputs go into the sidebar
    dashboardSidebar(width = 230,
                     sidebarMenu(id = "tabs",style = "position:fixed;width:230px;",
                                 menuItem(style = "position:fixed;width: inherit;",
                                          numericInput(inputId="choose.num.players",
                                                       label = "Number of players",
                                                       value = 2,
                                                       min = 1,
                                                       max = 50)),
                                 menuItem(style = "position:fixed;width: inherit;",
                                          sliderInput(inputId="choose.min.playtime",
                                                      label="Minimum playtime",
                                                      value = 30,
                                                      min = 1,
                                                      max = 450)),
                                 menuItem(style = "position:fixed;width: inherit;",
                                          sliderInput(inputId = "choose.max.playtime",
                                                      label = "Maximum playtime",
                                                      value = 60,
                                                      min = 1,
                                                      max = 450)),
                                 menuItem(style = "position:fixed;width: inherit;",
                                          selectInput(inputId = "choose.category",
                                                      label = "Board game category",
                                                      choices = c("Political",
                                                                  "Dice",
                                                                  "Bluffing",
                                                                  "Wargame",
                                                                  "Card Game",
                                                                  "Party",
                                                                  "Economic",
                                                                  "Abstract Strategy")))
                     )),
    dashboardBody(
        tags$head(tags$style(HTML("
       
       #This code dictates the colors & font for the dashboard
       
        @import url('//fonts.googleapis.com/css2?family=Josefin+Sans&display=swap');
 
                                /* logo */
                                .skin-blue .main-header .logo {
                                background-color: #015a60;
                                }
                                
                                .main-header .logo {
                                font-family: 'Josefin Sans', sans-serif;
                                font-weight: italic;
                                font-size: 24px;
                                }

                                /* logo when hovered */
                                .skin-blue .main-header .logo:hover {
                                background-color: ##015a60;
                                }

                                /* navbar (rest of the header) */
                                .skin-blue .main-header .navbar {
                                background-color: #015a60;
                                }

                                /* main sidebar */
                                .skin-blue .main-sidebar {
                                background-color: #09666d;
                                }

                                /* active selected tab in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                background-color: #09666d;
                                }

                                /* other links in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                                background-color: #09666d;
                                color: #ffffff;
                                }

                            
                                /* body */
                                .content-wrapper, .right-side {
                                background-color: #ffffff;
                                }

                                "))),
        tabsetPanel(
            tabPanel("Chart", 
                     DT::dataTableOutput(outputId = "table1")
            ),
            tabPanel("Graphs", fluid = TRUE,
                             plotOutput(outputId = "plot1"),
                             plotOutput(outputId = "plot2")
            )
            
            
            
        )
    )
    
)

server <- function(input, output) {
    
    output$table1 <- DT::renderDataTable({
        modified.games<- boardgames.data %>% 
            filter(details.maxplayers >= input$choose.num.players,
                   details.minplayers <= input$choose.num.players,
                   details.playingtime <= input$choose.max.playtime,
                   details.playingtime >= input$choose.min.playtime,
                   str_detect(boardgames.data$attributes.boardgamecategory,
                              input$choose.category) == TRUE) %>% 
            arrange(-stats.average) %>% 
            head(5)
        
        game.id.list <- modified.games$game.id
        game.names.list <- str_replace_all(modified.games$details.name,
                                           "[:blank:]",
                                           "_")
        links <- NULL
        for(i in 1:5){
            url1 <- paste0("https://www.boardgamegeek.com/boardgame/", game.id.list[i], "/", game.names.list[i])
            hyperlink <- paste0("<a href='",url1,"'>",modified.games$details.name[i],"</a>")
            links[[i]] <- hyperlink
        }
        modified.games$details.name <- links
        modified.games$details.description <- paste0(str_sub(modified.games$details.description, 1, 250), "...")
        mod.games.2 <- modified.games %>% 
            select(details.name, details.description, stats.average, stats.averageweight)
        colnames(mod.games.2)<- c("Name", "Description", "AverageRating", "AverageWeight")
        mod.games.2
    },
    options = list(
        dom = 't',
        ordering = FALSE,
        searching = FALSE,
        paging = FALSE),
    escape = FALSE
    )
    
    #Plot 1: Density vs. Rating
    output$plot1 <- renderPlot({
        rating.data <- boardgames.data %>% 
            filter(! is.na(boardgames.data$attributes.boardgamecategory)) 
        
        rating.data %>% 
            ggplot(aes(x = stats.average))+
            geom_density(color = "white",
                         aes(fill = str_detect(rating.data$attributes.boardgamecategory,
                                               input$choose.category)),
                         alpha = 0.5)+
            scale_fill_manual(values = c("turquoise4", "lightskyblue"),
                              labels = c(paste0("Not ", input$choose.category, " Games"), paste0(input$choose.category, " Games"))) +
            labs(title = "Rating distribution based on game category",
                 subtitle='How does the rating for your game stack up to others in its category?',
                 y = "DENSITY",
                 x = "RATING",
                 fill = "Type of game")+
            theme(plot.title = element_text(family = 'Georgia'))
    })
    
    #Plot 2: Density vs. Complexity (Weight)
    output$plot2 <- renderPlot({
        weight.data<- boardgames.data %>% 
            filter(! is.na(boardgames.data$attributes.boardgamecategory))
        
        weight.data %>% 
            ggplot(aes(x = stats.averageweight))+
            geom_density(color = "white",
                         aes(fill = str_detect(weight.data$attributes.boardgamecategory,
                                               input$choose.category)),
                         alpha = 0.5) +
            scale_fill_manual(values = c("goldenrod", "coral"),
                              labels = c(paste0("Not ", input$choose.category, " Games"), paste0(input$choose.category, " Games"))) +
            labs(title = "Complexity distribution based on game category",
                 subtitle='How does the complexity of your game stack up to others in its category?',
                 y = "DENSITY",
                 x = "COMPLEXITY",
                 fill = "Type of game")+
            theme(plot.title = element_text(family = 'Georgia'))
    })
}


shinyApp(ui = ui, server = server)
