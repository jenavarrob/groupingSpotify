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

# read the data
data <- read.csv("top50MusicFrom2010-2019.csv")
choices_list_cols <- setNames(names(data)[5:ncol(data)], names(data)[5:ncol(data)])
# as it was done above, get a choices list but now not for the columns in the data but for the different type of genres in the column the.genre.of.the.track
# get the unique values of the genre column
# genre_list <- unique(data$the.genre.of.the.track)
# # sort the genre list
# genre_list <- sort(genre_list)
# # create a named list of the genre list
# choices_list_genre <- setNames(genre_list, genre_list)

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

broad_genres = c("pop", "rock", "hop", "country", "dance", "latin", "indie", 
                 "metal", "jazz", "classical", "soul", "folk", "rap", "r&b", "house")
broadGenre <- sapply(data$the.genre.of.the.track, find_closest_genre, genre_list = broad_genres)
genre_list <- sort(data$broadGenre)
choices_list_genre <- setNames(genre_list, genre_list)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Spotify Data"),

    # Sidebar with a select input for different characteristics of the songs
    sidebarLayout(
        sidebarPanel(
          selectInput("selection", "Choose an Option:",
            choices = choices_list_cols),
          # add a checklist box for the genre
          checkboxGroupInput("genre", "Choose a genre:",
            choices = choices_list_genre)
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
    # show the table of the data
    output$table <- renderTable({
      data
    })
  
    # plot the sum of the data
    output$plot_sum <- renderPlot({
      sum_data <- data.frame(JAHR_START = data$year, SUM = data[[input$selection]])  ##HERE!!! for test only

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
    output$plot <- renderPlot({
      # plot x,y for two columns of the data
     
      x <- data$year
      y <- data[[input$selection]]
      
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
