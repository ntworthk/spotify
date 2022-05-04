
library(shiny)
library(spotifyr)

# Define UI for application
ui <- fillPage(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "main.css")
  ),
  htmlOutput("album_art"),
  textOutput("track_title"),
  textOutput("artist"),
  textOutput("album")
  
)

# Define server logic
server <- function(input, output, session) {
  
  x <- reactivePoll(
    intervalMillis = 5000,
    session = session, 
    checkFunc = Sys.time,
    valueFunc = function() {
      unlist(get_my_current_playback())[c("item.name", "item.artists.name", "item.artists.name1", "item.artists.name2", "item.album.name", "item.album.images.url1")]
    })
  
  
  output$album_art <- renderText({c('<img width="40%" src="',x()[["item.album.images.url1"]],'">')})
  output$track_title <- renderText(x()[["item.name"]])
  output$artist <- renderText(paste0(na.omit(x()[c("item.artists.name", "item.artists.name1", "item.artists.name2")]), collapse = ", "))
  output$album <- renderText(x()[["item.album.name"]])
  
}

# Run the application 
shinyApp(ui = ui, server = server)
