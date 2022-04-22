library("shiny")
library("shinyWidgets")
library("shinycssloaders")
#load("./UB_JOSS.rda")


#Source dependencies

source(system.file("shinyApp", "moduleVizumap.r", package="VizumApp"))
load(system.file("shinyApp/data/", "UB_JOSS.rda", package = "VizumApp"))




server <- function(input, output, session) {
 
  #Module for Vizumap
  callModule(module = VizumapServer, id = "Vizumap")
}
