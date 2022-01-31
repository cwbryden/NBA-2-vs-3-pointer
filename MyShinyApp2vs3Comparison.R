library(shiny)
library(dplyr)
library(ggplot2)
library(tidyverse)

playerData <- read_csv("D:/MyShinyApp/nba2or3_2021_player_stats.csv")

server <- function(input, output, session) {
    
    
    
    data <- reactive({
        req(input$select_playerName)
        
        df1 <- playerData %>%
            filter(playerName %in% input$select_playerName) %>%
            summarize(
                player2PtMake = input$select_playerName[,2],
                player2PtMiss = input$select_playerName[,3],
                player3PtMake = input$select_playerName[,4],
                player3PtMiss = input$select_playerName[,5])
    
        })
    
    # Update SelectInput Dynamically
    observe({
        updateSelectInput(session, "select_playerName", choices = playerData$playerName)
    })
    observe({
        updateSliderInput(session, "select_shotAttempts")
    })
    
    
    #Plot
    output$lineplot <- renderPlot({
        
        shot2 <- c(2, 0)
        shot3 <- c(3, 0)
        
        player2_prob <- c(data()$player2PtMake, data()$player2PtMiss)
        player3_prob <- c(data()$player3PtMake, data()$player3PtMiss)
        
        number_of_simulated_shot_attempts <- select_shotAttempts
        
        player2samp <- sample(shot2,
                              size = number_of_simulated_shot_attempts, 
                              replace = TRUE, 
                              prob = player2_prob)
        
        player3samp <- sample(shot3,
                              size = number_of_simulated_shot_attempts,
                              replace = TRUE,
                              prob = player3_prob)
        
        player2cumulative <- cumsum(player2samp)
        player3cumulative <- cumsum(player3samp)
        
        simulated_shot_attempts <- seq(1, number_of_simulated_shot_attempts, by = 1)
        
        player_df <- data.frame(simulated_shot_attempts, player2cumulative, player3cumulative)
        player_dff
        
        player_plot <- ggplot()+
            geom_line(data = player_df, aes(x = simulated_shot_attempts, 
                                            y = player2cumulative,
                                            col = "2 Point Shot Attempt")) +
            geom_line(data = player_df, aes(x = simulated_shot_attempts,
                                            y = player3cumulative,
                                            col = "3 Point Shot Attempt")) +
            labs(title = paste("2 v. 3 Comparison For", input$select_playerName),
                 subtitle = "By 2020-21 Shooting Percentages",
                 y = "Simulated Points Scored",
                 x = "Simulated Shot Attempts",
                 color = "Shot Type") +
            theme_minimal()
        player_plot
    })
}



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("NBA 2 Point vs 3 Point Player Comparison"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "select_playerName",
                        label = "What player would you like to simulate?",
                        choices = playerData$playerName),
            
            sliderInput(inputId = "select_shotAttempts",
                        "Number of Shot Attempts:",
                        min = 1,
                        max = 10000,
                        value = 20),
            
           plotOutput("plot")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("plot")
        )
    )
)




# Run the application 
shinyApp(ui = ui, server = server)
