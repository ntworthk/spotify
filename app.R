
library(shiny)
library(httr)
library(jsonlite)

get_spotify_authorization_code <- function() {
  
  scope <- c("ugc-image-upload", "user-modify-playback-state", "user-follow-modify", "user-read-recently-played", "user-read-playback-position", "playlist-read-collaborative", "app-remote-control", "user-read-playback-state", "user-read-email", "streaming", "user-top-read", "playlist-modify-public", "user-library-modify", "user-follow-read", "user-read-currently-playing", "user-library-read", "playlist-read-private", "user-read-private", "playlist-modify-private")
  client_id = "47b81983b4bf4875ab7d0e02427cde08"
  client_secret = "d02b2a54067f445c8fbe6e6f339c5186"
  
  endpoint <- oauth_endpoint(authorize = "https://accounts.spotify.com/authorize", 
                             access = "https://accounts.spotify.com/api/token")
  app <- oauth_app("spotifyr", client_id, client_secret)
  token <- (purrr::safely(.f = oauth2.0_token))(endpoint = endpoint, 
                                                app = app, scope = scope)
  if (!is.null(token$error)) {
    token$error
  }
  else {
    token$result
  }
}

skip_my_playback_previous <- function(device_id = NULL, authorization = get_spotify_authorization_code()) 
{
  base_url <- "https://api.spotify.com/v1/me/player/previous"
  params <- list(device_id = device_id)
  res <- RETRY("POST", base_url, config(token = authorization), 
               query = params, encode = "json")
  stop_for_status(res)
  res
}

skip_my_playback <- function(device_id = NULL, authorization = get_spotify_authorization_code()) 
{
  base_url <- "https://api.spotify.com/v1/me/player/next"
  params <- list(device_id = device_id)
  res <- RETRY("POST", base_url, config(token = authorization), 
               query = params, encode = "json")
  stop_for_status(res)
  res
}

pause_my_playback <- function(device_id = NULL, authorization = get_spotify_authorization_code()) 
{
  base_url <- "https://api.spotify.com/v1/me/player/pause"
  params <- list(device_id = device_id)
  res <- RETRY("PUT", base_url, config(token = authorization), 
               query = params, encode = "json")
  stop_for_status(res)
  res
}

start_my_playback <- function(device_id = NULL, context_uri = NULL, uris = NULL, 
                               offset = NULL, position_ms = NULL, authorization = get_spotify_authorization_code()) 
{
  base_url <- "https://api.spotify.com/v1/me/player/play"
  query_params = list(device_id = device_id)
  body_params <- list(context_uri = context_uri, uris = uris, 
                      offset = offset, position_ms = position_ms)
  res <- RETRY("PUT", base_url, query = query_params, config(token = authorization), 
               body = body_params, encode = "json")
  stop_for_status(res)
  res
}




get_now_playing <- function() {
  authorization <- get_spotify_authorization_code()
  base_url <- "https://api.spotify.com/v1/me/player/currently-playing"
  params <- list(market = NULL, additional_types = "episode")
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
    
    if (res$currently_playing_type == "episode") {

      # reformat into style of song
      res <- list(
        item = list(
          name = res$item$name,
          artists = list(
            name = res$item$show$publisher
          ),
          album = list(
            name = res$item$show$name,
            images = list(
              url = res$item$show$images$url
            ),
            uri = res$item$show$uri
          ),
          external_urls = list(
            spotify = res$item$external_urls$spotify
          )
        ),
        is_playing = res$is_playing
      )
      
    }
    
  }
  
  if ("is_local" %in% names(res$item)) {
    
    if (res$item$is_local) {
      
      res$item$album$images <- list(url1 = "/favicon.png")
      res$item$album$uri <- ""
    }

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
    intervalMillis = 2000,
    session = session, 
    checkFunc = get_now_playing,
    valueFunc = get_now_playing)
  
  
  output$album_art <- renderText({paste0('<a href="',paste0("https://open.spotify.com/", gsub(pattern = ":", replacement = "/", x = gsub(x = x()[["item.album.uri"]], pattern = "spotify:", replacement = ""))),'" target="_blank"><img width="40%" src="',x()[["item.album.images.url1"]],'"></a>')})
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
