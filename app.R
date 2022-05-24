
library(shiny)
library(spotifyr)
library(httr)
library(jsonlite)

get_now_playing <- function() {
  authorization <- get_spotify_authorization_code()
  base_url <- "https://api.spotify.com/v1/me/player/currently-playing"
  params <- list(market = NULL)
  res <- RETRY("GET", base_url, config(token = authorization), 
               query = params, encode = "json")
  stop_for_status(res)
  
  if (res$status_code == 204) {
    res <- list(
      item = list(
        name = "Nothing playing",
        artists = list(
          name = "",
          name1 = NA,
          name2 = NA
        ),
        album = list(
          name = "",
          images = list(
            url1 = ""
          )
        ),
        is_playing = "FALSE",
        external_urls = list(
          spotify = ""
        )
      )
    )
  } else {
    res <- fromJSON(content(res, as = "text", encoding = "UTF-8"), 
                    flatten = TRUE)
  }
  
  unlist(res)[c("item.name", "item.artists.name", "item.artists.name1", "item.artists.name2", "item.album.name", "item.album.images.url1", "is_playing", "item.external_urls.spotify", "item.album.uri")]

}


# Define UI for application
ui <- fillPage(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "main.css"),
    tags$title("nwbort on Spotify")
  ),
  htmlOutput("album_art"),
  textOutput("track_title"),
  textOutput("artist"),
  textOutput("album"),
  actionButton("previ", "⏮"),
  actionButton("start", "▶️"),
  actionButton("pause", "⏸"),
  actionButton("nexti", "⏭")
  
)

# Define server logic
server <- function(input, output, session) {
  
  x <- reactivePoll(
    intervalMillis = 5000,
    session = session, 
    checkFunc = Sys.time,
    valueFunc = get_now_playing)
  
  
  output$album_art <- renderText({c('<a href="',paste0("https://open.spotify.com/", gsub(pattern = ":", replacement = "/", x = gsub(x = x()["item.album.uri"], pattern = "spotify:", replacement = ""))),'" target="_blank"><img width="40%" src="',x()[["item.album.images.url1"]],'"></a>')})
  output$track_title <- renderText(x()[["item.name"]])
  output$artist <- renderText(paste0(na.omit(x()[c("item.artists.name", "item.artists.name1", "item.artists.name2")]), collapse = ", "))
  output$album <- renderText(x()[["item.album.name"]])

  observeEvent(input$previ, {try(skip_my_playback_previous())})
  observeEvent(input$start, {try(if(x()[["is_playing"]] == "FALSE") start_my_playback())})
  observeEvent(input$pause, {try(if(x()[["is_playing"]] == "TRUE") pause_my_playback())})
  observeEvent(input$nexti, {try(skip_my_playback())})
  
}

# Run the application 
shinyApp(ui = ui, server = server)
