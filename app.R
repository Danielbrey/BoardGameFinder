library(shiny)
library(tidyverse)
library(rvest)
library(shinydashboard)
library(DT)
library(shinythemes)


boardgames.data <- read_csv("boardgamesswipedata.csv")
boardgames.data$checked <- FALSE
boardgames.data$like <- NULL
boardgames.data$dislike <- NULL

ui <- fluidPage(
    
    tabsetPanel(
        tabPanel("Intro", fluid = TRUE,
                 sidebarPanel(),
                 mainPanel(
                     htmlOutput(outputId= "intro")
                 )
                 ),
        tabPanel("Search", fluid = TRUE,
                 sidebarPanel(
                     numericInput(inputId="choose.num.players",
                                  label = "Number of players",
                                  value = 2,
                                  min = 1,
                                  max = 50),
                    sliderInput(inputId="choose.min.playtime",
                             label="Minimum playtime",
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
                                            "Abstract Strategy"))
                    ),
                 mainPanel(
                     DT::dataTableOutput(outputId = "table1")
                 )),
        tabPanel("Random", fluid = TRUE,
                 sidebarPanel(
                     actionButton(inputId = "like",
                                  label = "Like"),
                     actionButton(inputId = "dislike",
                                  label = "Dislike")
                 ),
                 mainPanel(
                     tableOutput(outputId = "gamename"),
                     tableOutput(outputId = "gamedetails"),
                     htmlOutput(outputId = "description")
                 )),
    tabPanel("Liked Games", fluid = TRUE,
             sidebarPanel(),
             mainPanel(
                 DT::dataTableOutput(outputId = "games")
             )),
    tabPanel("Thetas", fluid = TRUE,
             sidebarPanel(),
             mainPanel(
                 tableOutput(outputId = "thetas"),
                 tableOutput(outputId = "test")
             ))
    )
    
)


server <- function(input, output) {
    
    rand <- reactiveValues(randval = 1,
                           d = boardgames.data,
                           numcheck = 0,
                           switch = boardgames.data[1,],
                           used = NULL)
    
    rand$thetas <- data.frame(Base = 0,
                              Weight = 0,
                              PlayingTime = 0,
                              Players = 0
                              #Cost = 0
                              )
    
    rand$liked.games <- data.frame(details.name = NULL,
                                   details.playingtime = NULL,
                                   stats.averageweight = NULL)
    
    rand$checked.games <- data.frame(details.name = NULL,
                                     stats.averageweight = NULL,
                                     details.playingtime = NULL,
                                     details.maxplayers = NULL,
                                     details.chosen = NULL)
    
    hypothesis.for.game <- function(theta.set, game.weight, game.playtime, game.maxplayers){
        hypo <- theta.set[1]*game.weight + theta.set[2] * game.playtime + theta.set[3] * game.maxplayers
        return(hypo)
    }
    
    descend <- function(theta.set.descend, games.list, B){
        newthetas <- c(0,0,0,0)
        sums <- c(0,0,0,0)
        value<- 0
        sums[1] <- for(i in 1:nrow(games.list)){
            chosen.game <- games.list[i]
            add.on <- hypothesis.for.game(theta.set.descend, chosen.game$stats.averageweight, chosen.game$details.playingtime, chosen.game$details.maxplayers) - chosen.game$chosen
            value + add.on
        }
        for(j in 2:4){
            value<- 0
            for(i in 1:nrow(games.list)){
                chosen.game <- games.list[i]
                add.on <- hypothesis.for.game(theta.set.descend, chosen.game$stats.averageweight, chosen.game$details.playingtime, chosen.game$details.maxplayers) - chosen.game$chosen
                value <- value + (add.on * chosen.game[j])
            }
            sums[j] <- value
        }
        
        for(k in 1:4){
            newthetas[k] <- newthetas[k] - (B / nrow(games.list) * sums[k])
        }
        return(c(0,1,1,1))
    }
    
    observeEvent(input$like, {
        
        #Add new game to the liked games chart
        row.add <- rand$d %>% select(details.name, details.playingtime, stats.averageweight)
        rand$liked.games <- rbind(rand$liked.games, row.add[rand$randval + rand$numcheck, ])
        
        #Add new game to checked games
        row.add2 <- rand$d %>% select(details.name, stats.averageweight, details.playingtime, details.maxplayers)
        row.add2.2 <- row.add2[rand$randval + rand$numcheck, ]
        row.add2.2$chosen <- 1
        rand$checked.games <- rbind(rand$checked.games, row.add2.2)
        
        #Modify thetas
        #rand$thetas <- descend(rand$thetas, rand$checked.games, 0.01)
        
        #Modify values, showing this game has been liked/checked
        rand$d$checked[rand$randval+rand$numcheck] <- TRUE
        rand$d$like[rand$randval+rand$numcheck] <- 1
        rand$d$dislike[rand$randval+rand$numcheck] <- 0
        
        #Making name a link in liked list
        game.id.link <- rand$d$game.id[rand$randval + rand$numcheck]
        game.name.link <- str_replace_all(rand$d$details.name[rand$randval + rand$numcheck],
                                          "[:blank:]",
                                          "_")
        url1 <- paste0("https://www.boardgamegeek.com/boardgame/", game.id.link, "/", game.name.link)
        hyperlink <- paste0("<a href='",url1,"'>", rand$d$details.name[rand$randval + rand$numcheck],"</a>")
        rand$liked.games$details.name[nrow(rand$liked.games)] <- hyperlink
        
        #Swap this checked game and put it at the top of the list (so it won't be checked again)
        rand$switch <- rand$d[rand$numcheck+1, ]
        rand$d[rand$numcheck+1, ] <- rand$d[rand$randval + rand$numcheck, ]
        rand$d[rand$randval + rand$numcheck, ] <- rand$switch
        
        #set up for next game
        rand$randval <- sample(1:10, 1)
        rand$numcheck <- rand$numcheck+1
        
    })
    
    observeEvent(input$dislike, {
        #Add new game to checked games
        row.add2 <- rand$d %>% select(details.name, stats.averageweight, details.playingtime, details.maxplayers)
        row.add2.2 <- row.add2[rand$randval + rand$numcheck, ]
        row.add2.2$chosen <- 0
        rand$checked.games <- rbind(rand$checked.games, row.add2.2)
        
        #Modify thetas
        #rand$thetas <- descend(rand$thetas, rand$checked.games, 0.01)
        
        #Modify values, showing this game has been disliked/checked
        rand$d$checked[rand$randval+rand$numcheck] <- TRUE
        rand$d$dislike[rand$randval+rand$numcheck] <- 1
        rand$d$like[rand$randval+rand$numcheck] <- 0
        
        rand$switch <- rand$d[rand$numcheck+1, ]
        rand$d[rand$numcheck+1, ] <- rand$d[rand$randval + rand$numcheck, ]
        rand$d[rand$randval + rand$numcheck, ] <- rand$switch
        
        rand$randval <- sample(1:10, 1)
        
        rand$numcheck <- rand$numcheck+1
    })
    
    
    output$intro <- renderUI({
        str1<- "This app takes data from BoardGameGeek's website including each game's playing time, complexity, rating, and more."
        str2 <- "The 'Search' tab gives users the opportunity to search for games based on their game preferences, whether that be the number of players or the game's category."
        str3 <- "The 'Random' tab will give the user a game, and they can choose if they like or dislike the game. All choices will be saved. After the user makes a choice, the app will show another game, and so on."
        str4 <- "The 'Liked Games' tab will save all games that have been liked, and will have links to their BoardGameGeek webpages."
        str5 <- "The goal of this app will be to find games you are more interested in based on your previous likes and dislikes. While it is not working yet, the 'Thetas' tab will show the theta values for different variables that may affect a users decision to like/dislike a game. This tab also has a list of all checked games and whether or not they are liked/disliked."
        HTML(paste(str1, str2, str3, str4, str5, sep = "<br/><br/>"))
    })
    
    output$table1 <- DT::renderDataTable({
        tryCatch({
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
        
        warning=function(w) { 
            modified.games
        },
        error=function(e) {
            modified.games
        })
    },
    options = list(
        dom = 't',
        ordering = FALSE,
        searching = FALSE,
        paging = FALSE),
    escape = FALSE)
    
    
    
    output$gamename <- renderUI({
        name <- rand$d %>% 
            filter(checked == FALSE)
        name2 <- name[rand$randval, ]
        h2(HTML(name2$details.name), align = "center")
    })
    
    output$gamedetails <- renderTable({
        
        gametitles <- rand$d %>%
            filter(checked == FALSE) %>% 
            select(details.playingtime, details.minplayers, details.maxplayers, stats.average, stats.averageweight)
        colnames(gametitles) <- c("Playing Time", "Min Players", "Max Players", "Average Rating", "Average Weight")
        gametitles[rand$randval, ]
        
        
        #Goal: post a picture here
        #game.id.link <- rand$d$game.id[rand$randval]
        #game.name.link <- str_replace_all(rand$d$details.name[rand$randval],
        #                                  "[:blank:]",
        #                                  "_")
        #url1 <- paste0("https://www.boardgamegeek.com/boardgame/", game.id.link, "/", game.name.link)
        #picture <- url1 %>% 
        #    read_html() %>% 
        #    html_nodes(xpath = '//*[@id="mainbody"]/div/div[1]/div[2]/div[2]/ng-include/div/ng-include/div/div[2]/div[1]/ng-include/div/a[1]/img') %>%
        #    html_attrs()
        
        })
    
    
    output$description <- renderUI({
        desc <- rand$d %>% 
            filter(checked == FALSE)
        desc2 <- desc[rand$randval, ]
        HTML(desc2$details.description)
    })
    
    output$img1 <- renderImage({
        url3 <- "https://boardgaming.com/games/card-games/dominion"
        sess <- html_session(url3)
        picture3 <- sess %>% 
            read_html() %>% 
            html_nodes(xpath = '//*[@id="feature-image"]/img') %>% 
            html_attr('src')
        
        list(src = picture3, height = 400, width = 400, alt = "This is alternate text", deleteFile = FALSE)
    })
    

    
    
    
    
    output$games <- DT::renderDataTable({
        games <- rand$liked.games
    },
    escape = FALSE
    )
    
    
    
    output$test <- renderTable({
        rand$d %>% 
            select(details.name, checked, like, dislike) %>% 
            head(20)
    }) 
    
    
    output$thetas <- renderTable({
        rand$thetas
    })
    
}


shinyApp(ui = ui, server = server)
