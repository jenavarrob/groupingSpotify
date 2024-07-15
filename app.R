#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Spotify Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectInput("selection", "Choose an Option:",
                      choices = list("Popularity" = "option1popularity",
                                     "Danceability" = "option2danceability",
                                     "Beats per Minute" = "option3beatspm")),
        ),

        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("plot_xy"),
          plotOutput("plot_mean"),
          plotlyOutput("plot_broad_genre"),
          # show the table of the data
          tableOutput("table")
        )
    )
)

# Define server logic required to show table and plot
server <- function(input, output) {
    # read the data
    data <- read.csv("top50MusicFrom2010-2019.csv")

    # show the table of the data
    output$table <- renderTable({
      data
    })
  
    ## Using Generic X-Y Plot: plot all data values
    output$plot_xy <- renderPlot({
      # plot x,y for two columns of the data
      
      x <- data$year
      if(input$selection == "option1popularity") 
        y <- data$Popularity
      else if(input$selection == "option2danceability")
        y <- data$Danceability
      else
        y <- data$Beats.Per.Minute
      
      title(main="All Values")
      xlabel <- "Year"
      ylabel <- input$selection
      plot(x, y)
    })
    
    # Using GGPLOT: plot the sum of the data
    output$plot_mean <- renderPlot({
      if(input$selection == "option1popularity") 
        sum_data <- data.frame(JAHR_START = data$year, SUM = data$Popularity)
      else if(input$selection == "option2danceability")
        sum_data <- data.frame(JAHR_START = data$year, SUM = data$Danceability)
      else
        sum_data <- data.frame(JAHR_START = data$year, SUM = data$Beats.Per.Minute)
      
      aggregated_data <- sum_data %>%
      group_by(JAHR_START) %>%
      summarise(SUM = mean(SUM))

      ggplot(aggregated_data, aes(x=JAHR_START,y=SUM))+
        geom_line()+
        geom_point()+
        labs(title="Average",
             x="years",
             y=input$selection)+
        theme_minimal()
    })
    
    ## Using PLOTLY: plot different lines for each broad_genre
    output$plot_broad_genre <- renderPlotly({

      # Define a function to find the closest genre
      find_closest_genre <- function(genre, genre_list) {
        # Use agrep to find the closest match, allowing for some dissimilarity
        matches <- sapply(genre_list, function(g) agrep(g, genre, value = TRUE, max.distance = 0.1))
        # Filter out empty matches
        matches <- matches[sapply(matches, length) > 0]
        # Return the first match found or a default value
        if (length(matches) > 0) {
          names(matches)[1]
        } else {
          "Other"
        }
      }

      # Define the broad genres
      broad_genres = c("pop", "rock", "hip hop", "rap", "country", "r&b", "latin", "edm", "metal", "jazz", "classical", "reggae", "blues", "folk", "indie",
                       "punk", "soul", "funk", "disco", "electronic", "house", "techno",
                       "bossa nova", "bolero", "band", "dance", "ska")
      broadGenre <- sapply(data$the.genre.of.the.track, find_closest_genre, genre_list = broad_genres)
      data$BroadGenre <- as.factor(broadGenre)

      #show the table of the data on rstudio environment
      View(data)

      if(input$selection == "option1popularity")
        sum_data <- data.frame(JAHR_START = data$year, SUM = data$Popularity, BroadGenre = data$BroadGenre)
      else if(input$selection == "option2danceability")
        sum_data <- data.frame(JAHR_START = data$year, SUM = data$Danceability, BroadGenre = data$BroadGenre)
      else
        sum_data <- data.frame(JAHR_START = data$year, SUM = data$Beats.Per.Minute, BroadGenre = data$BroadGenre)

      
      # Add plotly code here for the broad genre
      # sum up values for each broad genre for each year for the selected option, danceability, popularity, or beats per minute
      # sum_data <- data %>%
      #   group_by(year, BroadGenre) %>%
      #   summarise(SUM = mean(SUM))
    #   
      aggregated_data <- sum_data %>%
        group_by(JAHR_START, BroadGenre) %>%
        summarise(SUM = mean(SUM,na.rm = TRUE)) %>%
        ungroup()
    #   
      plot_ly(aggregated_data, x=~JAHR_START, y=~SUM, color =~BroadGenre, type='scatter', mode ='lines+markers',
              hoverinfo = 'text',
              text = ~paste('Year:', JAHR_START, '<br>Popularity:', SUM, '<br>Genre:', BroadGenre)) %>%
        layout(title= "by Broad Genre",
               xaxis = list(title ="Year"),
               yaxis = list(title="Average"))
    #   
    })
    
    ###
    # genre_counts <- data %>%
    #   +     group_by(`the.genre.of.the.track`) %>%
    #   +     summarise(Count = n()) %>%
    #   +     arrange(desc(Count))
    # > View(genre_counts)

}

# Run the application 
shinyApp(ui = ui, server = server)


