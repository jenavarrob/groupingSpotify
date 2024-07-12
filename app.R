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
          plotOutput("plot_sum"),
          plotOutput("plot"),
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
  
    # plot the sum of the data
    output$plot_sum <- renderPlot({
      #data <- read.csv("top50MusicFrom2010-2019.csv")
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
        labs(title="summe values over the years",
             x="jahr",
             y="summe")+
        theme_minimal()
    })

  ## plot data values
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
    
    broadGenre <- sapply(data$the.genre.of.the.track, find_closest_genre, genre_list = broad_genres)
    
    output$plot <- renderPlot({
      # plot x,y for two columns of the data
     
      x <- data$year
      if(input$selection == "option1popularity") 
        y <- data$Popularity
      else if(input$selection == "option2danceability")
        y <- data$Danceability
      else
        y <- data$Beats.Per.Minute
      
      plot(x, y)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

##
# ggplot(sum_data, aes(x=JAHR_START,y=SUM))+
#   geom_line()+
#   geom_point()+
#   labs(title="summe values over the years",
#   x="jahr",
#   y="summe")+
#   theme_minimal()

##  
# plot_ly(sum_data, x=~JAHR_START,y=~SUM, type='scatter'),
# mode ='lines+markers' %>%
#   layout(title="Summe von..",
# xaxis = list(title ="Jahr"),
# yaxis = list(title="Summe"))

###
# genre_counts <- data %>%
#   +     group_by(`the.genre.of.the.track`) %>%
#   +     summarise(Count = n()) %>%
#   +     arrange(desc(Count))
# > View(genre_counts)
# > View(genre_counts)
