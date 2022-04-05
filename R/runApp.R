#' Run shiny app
#' 
#' Function to run the shiny app
#'
#'@export
runShiny <- function() {
  library(shiny)
  runApp(system.file("shinyApp", package="VizumApp"))
}


