library("shiny")
library("shinyWidgets")
library("shinycssloaders")


#Source dependencies
source(system.file("shinyApp", "moduleVizumap.r", package="VizumApp"))
load(system.file("shinyApp/data/", "UB_JOSS.rda", package = "VizumApp"))

ui <- fluidPage(
  h1("VizumApp"),
  absolutePanel(list(a("VizumApp Github", href="https://github.com/SamNelson081/VizumApp"), icon("github")), right="15px", top="25px"),
  VizumapUI("Vizumap")
)
