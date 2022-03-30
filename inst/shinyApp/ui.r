library("shiny")
library("shinyWidgets")
library("shinycssloaders")


#Source dependencies
#source("./R/moduleVizumap.R")

ui <- fluidPage(
  h1("Vizumap"),
  VizumapUI("Vizumap")
)