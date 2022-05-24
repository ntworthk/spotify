library(shiny)
options(shiny.port = 5522)
runApp(file.path(this.path::here(), "app.R"))
