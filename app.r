# # # # # # # # # # # # # # # # # # #

#  Install and/or source libraries  #

# # # # # # # # # # # # # # # # # # #

#Shiny Libraries
library("shiny")
library("shinyWidgets")
library("shinycssloaders")


#Source dependencies
source("./modules/moduleVizumap.R")



# # # # # # # # # # # # # # # # # # #

#           User Interface          #

# # # # # # # # # # # # # # # # # # #

ui <- fluidPage(
  h1("Vizumap"),
  VizumapUI("Vizumap")
)



# # # # # # # # # # # # # # # # # # #

#               Server              #

# # # # # # # # # # # # # # # # # # #

server <- function(input, output, session) {
  #Module for Vizumap
  callModule(module = VizumapServer, id = "Vizumap")
}

#Run UI and Server
shinyApp(ui, server)

