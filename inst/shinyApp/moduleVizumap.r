library("Vizumap")
library("leaflet")
library("rgdal")
library("rgeos")
library("sp")
library("ggplot2")
library("htmltools")
library("colourpicker")
library("htmlwidgets")
library("shinyalert")
library("shinyjs")
library("cowplot")
library("scales")
library("DescTools")
library("geojson")
library("shinydashboardPlus")

VizumapUI <- function(id) {
  ns <- NS(id)
  return(
    tagList(
      sidebarLayout(
        sidebarPanel = sidebarPanel(
#          useShinyalert(),
          #LeafletJS Marker Rotate Library  
          includeScript("https://raw.githubusercontent.com/bbecquet/Leaflet.RotatedMarker/master/leaflet.rotatedMarker.js"),
          #JS Colour Gradient Library
          includeScript("https://raw.githubusercontent.com/anomal/RainbowVis-JS/master/rainbowvis.js"),
          #LeafletJS Polygon Fill Library
          includeScript("https://raw.githubusercontent.com/lwsu/leaflet-polygon-fillPattern/master/leaflet-polygon.fillPattern.js"),
          #LeafletJS Material Icon Library
          tags$script(src = 'leaflet.icon-material.js'),
          includeCSS("https://raw.githubusercontent.com/ilyankou/Leaflet.IconMaterial/master/dist/leaflet.icon-material.css"),
          
          #Custom CSS for Map Height
          tags$style(type = "text/css", "div[id$=-map] {height: calc(100vh - 100px) !important;} .panel.box.box-info { padding: 10px; } .col-sm-2 {height: calc(100vh - 80px); overflow-y: scroll}"),
          
          
          p("Use Example data from the US or the Great Barrier Reef to run an example"),
          selectizeInput(ns("exampleDataset"), label="Example Dataset", choices=c("Upload Your Own Data", "US", "GBR")),
          conditionalPanel("input.exampleDataset == 'Upload Your Own Data'", ns = ns,
                   hr(),
                   fileInput(ns("shapefile"), "Upload a Shapefile", multiple=TRUE, accept = c(".shp", ".dbf", ".sbn", ".sbx", ".shx", ".prj")),
                   p("Note: Shapefile uploads require uploading .shp, .shx, .prj and .dbf files for a given shapefile"),
              
                   fileInput(ns("csvDataset"), "Upload a CSV Dataset",
                        multiple = FALSE,
                        accept = c("text/csv",
                                   "text/comma-separated-values,text/plain",
                                   ".csv")),
                        # Input: Checkbox if file has header ----
                        checkboxInput(ns("header"), "Header", TRUE),
              
                        # Input: Select separator ----
                        radioButtons(ns("sep"), "Separator",
                           choices = c(Comma = ",",
                                       Semicolon = ";",
                                       Tab = "\t"),
                           selected = ","),
              
                        # Input: Select quotes ----
                        radioButtons(ns("quote"), "Quote",
                           choices = c(None = "",
                                       "Double Quote" = '"',
                                       "Single Quote" = "'"),
                           selected = '"'),
              
                    uiOutput(ns("colSelector")),
          hr()
          ),
          selectizeInput(ns("plotType"), label="Plot Type", choices=c("Bivariate", "Glyph", "Pixel", "Excedance")),
          conditionalPanel("input.plotType == 'Excedance' && input.exampleDataset == 'Upload Your Own Data'", ns=ns,
             checkboxInput(ns("useCurrent"), "Use a column from the current dataset?"),
              conditionalPanel("input.useCurrent == true", ns=ns, 
                uiOutput(ns("excColCurrent"))           
              ),
              uiOutput(ns("excCol"))
              ),
          actionButton(ns("example"), label = "Run"),
          hr(),
          conditionalPanel("input.plotType == 'Bivariate'", ns = ns,
                           prettyCheckbox(ns("axis"), "Flip Axis", FALSE),
                           fluidRow(
                             column(6, colourpicker::colourInput(ns("paletteL"), "Estimate", "blue")),
                             column(6, colourpicker::colourInput(ns("paletteR"), "Error", "yellow"))
                           ),
                           fluidRow(
                             column(6, prettyCheckbox(ns("flipX"), "Flip Horizontal", FALSE)),
                             column(6, prettyCheckbox(ns("flipY"), "Flip Vertical", FALSE))
                           )),
          conditionalPanel("input.plotType == 'Glyph' || input.plotType == 'Pixel' || input.plotType == 'Excedance'", ns = ns,
                          fluidRow(
                             column(6, colourpicker::colourInput(ns("glyphCol1"), "Colour 1", "red")),
                             column(6, colourpicker::colourInput(ns("glyphCol2"), "Colour 2", "white"))
                           )),
          plotOutput(ns("palettePlot")),
          width = 2
        ),
        mainPanel = mainPanel(
          leafletOutput(ns("map"), width="100%") 
            %>% withSpinner(), 
          width=10
        )
      ),
      absolutePanel(
        plotOutput(ns("bkey")) 
          %>% withSpinner(),
        bottom = "25%",
        right = "50px",
        width = "15%",
        height = "15%"
      ),
      #Custom JS Script
      tags$script(src = 'index.js')
    )
  )
}
  
VizumapServer <- function(input, output, session) {
  
  
  buildGMap <- function(datatype) {
    withProgress(message = 'Running Example', value = 0, {
      
      incProgress(1/4, detail = paste("Reading in Data Files..."))
      
      if(datatype == "US") {
        
        #Load data
        data(us_data)
        
        #Relative path to my machine
 #       shp <- readOGR(dsn = "./data/USshp", layer = "05000")
        shp <- readOGR(dsn = system.file("shinyApp/extdata", "USshp", package = "VizumApp"),
                       layer = "05000")
        
        estimate = "pov_rate"
        error = "pov_moe"
        
        data <- read.uv(data = us_data, estimate = estimate, error = error)
        
        id="GEO_ID"
        
      } else if(datatype == 'GBR') {
        
        load(system.file("shinyApp/data", "burd_data.rda", package = "VizumApp"))
        load(system.file("shinyApp/data", "burd_geo.rda", package = "VizumApp"))
        load(system.file("shinyApp/data", "UB.rda", package = "VizumApp"))
        
        shp <- UB_shp
        
        # Extract the 2005/06 financial year mean concentration estimates, sd, lower and upper CIs
        amc_0506 <- data.frame(m = burd_data$TSS_m$`2005/2006`, sd = burd_data$TSS_sd$`2005/2006`,
                               lci = burd_data$TSS_lci$`2005/2006`, uci = burd_data$TSS_uci$`2005/2006`)
        
        estimate = "m"
        error = "sd"
        # Reorder the columns to what Vizumap wants (estimate, error, ...)
        amc05 <- read.uv(data = amc_0506, estimate = estimate, error = error)
        amc05$scID <- "sortid" # ID that matches the sub-catchment boundaries in the shape file
        
        data <- amc05
        id="scID"
      } else {
        shp <- shapefile()
        
        data <- userDataset()
        
        data$OBJECTID <- as.integer(data$OBJECTID)
        
        id <- as.character(input$idMatch)
        
        
        shp@data$OBJECTID <- as.integer(shp@data[,id]) 
        
        estimate <- "Estimate"
        error <- "Error"
        name <- input$nameInput
      }
      
      shp <- spTransform(shp,"+init=epsg:4326")
      centroids <- gCentroid(shp, byid=TRUE)
      
    
      #Get the data
      incProgress(1/4, detail = paste("Building map and key..."))
      
      m <<- build_gmap(data = data, geoData = shp, id = id, border = "state", palette = "Purples")
      
      data <- data[order(data[id]),]
      return(list(type = "glyph", dataset = data, polygons = shp, centroids = centroids, map = m, palette = c(input$glyphCol1, input$glyphCol2), estimate=estimate, error=error))
      
    })
  }
  
  
  buildBMap <- function(datatype) {
    withProgress(message = 'Running Example', value = 0, {
      
      incProgress(1/4, detail = paste("Reading in Data Files..."))
      
      if(datatype == "US") {
        
        #Load data
        data(us_data)
  
        #Relative path to my machine
        shp <- readOGR(dsn = system.file("shinyApp/extdata", "USshp", package = "VizumApp"),
                       layer = "05000")
        
        estimate = "pov_rate"
        error = "pov_moe"
        name <- "NAME"
        
        data <- read.uv(data = us_data, estimate = estimate, error = error)
        
        id="GEO_ID"
        
      } else if(datatype == 'GBR') {

        load(system.file("shinyApp/data", "burd_data.rda", package = "VizumApp"))
        load(system.file("shinyApp/data", "burd_geo.rda", package = "VizumApp"))
        load(system.file("shinyApp/data", "UB.rda", package = "VizumApp"))
        
        
        shp <- UB_shp 
        
        
        # Extract the 2005/06 financial year mean concentration estimates, sd, lower and upper CIs
        amc_0506 <- data.frame(m = burd_data$TSS_m$`2005/2006`, sd = burd_data$TSS_sd$`2005/2006`,
                               lci = burd_data$TSS_lci$`2005/2006`, uci = burd_data$TSS_uci$`2005/2006`)
        
        estimate = "m"
        error = "sd"
        name <- "SUBBASIN"
        # Reorder the columns to what Vizumap wants (estimate, error, ...)
        amc05 <- read.uv(data = amc_0506, estimate = estimate, error = error)
        amc05$scID <- sortid # ID that matches the sub-catchment boundaries in the shape file
        
        data <- amc05
        id="scID"
      } else {
        shp <- shapefile()
        
        data <- userDataset()
        
        data$OBJECTID <- as.integer(data$OBJECTID)
        
        id <- as.character(input$idMatch)
        
        
        shp@data$OBJECTID <- as.integer(shp@data[,id]) 
          
        estimate <- "Estimate"
        error <- "Error"
        name <- input$nameInput
      }
      
      shp <- spTransform(shp, "+init=epsg:4326")
      
      #Get the data
      incProgress(1/4, detail = paste("Building map and key..."))
      
   
  
      #Bivariate map
      m <- build_bmap(data = data, geoData = shp, id = id, border = "state", 
                      terciles = TRUE, palette = isolate(palette()), 
                      flipAxis = isolate(input$axis))
      m$output_data$hex_code <- as.character(m$output_data$hex_code)
     
      #Key

      incProgress(1/4, detail = paste("Creating Spatial Objects..."))
      
      
      #County Polygons
      split_data_poly <- lapply(unique(m$output_data$group), function(x) {
        df <- as.matrix(m$output_data[m$output_data$group == x, c("long", "lat")])
        polys <- Polygons(list(Polygon(df)), ID = x)
        return(polys)
        return(df)
      })
      
     
      data_polys <- SpatialPolygonsDataFrame(
        SpatialPolygons(split_data_poly),
        data.frame(
          row.names= unique(m$output_data$group), 
          color=unique(cbind(m$output_data$group, m$output_data$hex_code))[,2],
          names=unique(cbind(m$output_data$group, m$output_data[name]))[,2],
          pov=unique(cbind(m$output_data$group, m$output_data[estimate]))[,2],
          moe=unique(cbind(m$output_data$group, m$output_data[error]))[,2]
        )
      )
     
      #Border Lines
      split_data <- lapply(unique(m$bord$group), function(x) {
        df <- as.matrix(m$bord[m$bord$group == x, c("long", "lat")])
        lns <- Lines(Line(df), ID = x)
        return(lns)
      })
      
      #Create Spatial Lines object to add to leaflet
      data_lines = SpatialLines(split_data)
      
      incProgress(1/4, detail = paste("Done!"))
    })
    
    
    data <- data[order(data[id]),]

    return(list(type = "bivariate", dataset = data, polygons = data_polys, polylines = data_lines, map = m))
  }
  
  buildPMap <- function(datatype) {
   
      if(datatype == "US") {
        
        #Load data
        data(us_data)
        
        #Relative path to my machine
        shp <- readOGR(dsn = system.file("shinyApp/extdata", "USshp", package = "VizumApp"),
                       layer = "05000")
        
        estimate = "pov_rate"
        error = "pov_moe"
        
        data <- read.uv(data = us_data, estimate = estimate, error = error)
        
        id="GEO_ID"
        
      } else if(datatype == 'GBR')  {
        
        
        load(system.file("shinyApp/data", "burd_data.rda", package = "VizumApp"))
        load(system.file("shinyApp/data", "burd_geo.rda", package = "VizumApp"))
        load(system.file("shinyApp/data", "UB.rda", package = "VizumApp"))
        
        shp <- UB_shp
        
        # Extract the 2005/06 financial year mean concentration estimates, sd, lower and upper CIs
        amc_0506 <- data.frame(m = burd_data$TSS_m$`2005/2006`, sd = burd_data$TSS_sd$`2005/2006`,
                               lci = burd_data$TSS_lci$`2005/2006`, uci = burd_data$TSS_uci$`2005/2006`)
        
        estimate = "m"
        error = "sd"
        # Reorder the columns to what Vizumap wants (estimate, error, ...)
        amc05 <- read.uv(data = amc_0506, estimate = estimate, error = error)
        amc05$scID <- sortid # ID that matches the sub-catchment boundaries in the shape file
        
        data <- amc05
        id="scID"
      } else {
        shp <- shapefile()
        
        data <- userDataset()
        
        data$OBJECTID <- as.integer(data$OBJECTID)
        
        id <- as.character(input$idMatch)
        
        
        shp@data$OBJECTID <- as.integer(shp@data[,id]) 
        
        estimate <- "Estimate"
        error <- "Error"
        name <- input$nameInput
      }
      
      shp <- spTransform(shp,"+init=epsg:4326")
      
      
      #Get the data
     
      
      data <- data[order(data[id]),]
    return(list(type = "pixel", dataset = data, polygons = as.geojson(shp), palette = c(input$glyphCol1, input$glyphCol2), estimate=estimate, error=error))#, map = m, palette = c(input$glyphCol1, input$glyphCol2), estimate=estimate, error=error))
  }
  
  buildEMap <- function(datatype) {
    
    if(datatype == "US") {
      
      #Load data
      data(us_data)
      
      #Relative path to my machine
      shp <- readOGR(dsn = system.file("shinyApp/extdata", "USshp", package = "VizumApp"),
                     layer = "05000")
      
      estimate = "pov_rate"
      error = "pov_moe"
      
      data <- read.uv(data = us_data, estimate = estimate, error = error)
      
      id="GEO_ID"
      
      
      pd <- quote({ pexp(q, rate, lower.tail = FALSE) })
      #---- define argument listing
      args <- quote({ list(rate = 1/us_data$pov_rate) })
      #---- capture distribution and arguments in a single list
      pdflist <- list(dist = pd, args = args, th = 30)
      
      args_call <- eval(do.call("substitute", list(pdflist$args, list(us_data$pov_rate, us_data$pov_moe))))
   
      data$exc <- pdist(pname = pdflist$dist, th = pdflist$th, args = args_call)
      exc_data <- data
      exc_name <- "exc"

    } else if(datatype == "GBR") {

      
      load(system.file("shinyApp/data", "burd_data.rda", package = "VizumApp"))
      load(system.file("shinyApp/data", "burd_geo.rda", package = "VizumApp"))
      load(system.file("shinyApp/data", "UB_JOSS.rda", package = "VizumApp"))

      shp <- UB_shp
    
      # Extract the 2005/06 financial year mean concentration estimates, sd, lower and upper CIs
      amc_0506 <- data.frame(m = burd_data$TSS_m$`2005/2006`, sd = burd_data$TSS_sd$`2005/2006`,
                             lci = burd_data$TSS_lci$`2005/2006`, uci = burd_data$TSS_uci$`2005/2006`)
      
      estimate = "m"
      error = "sd"
      # Reorder the columns to what Vizumap wants (estimate, error, ...)
      amc05 <- read.uv(data = amc_0506, estimate = estimate, error = error)
      amc05$scID <- "sortid" # ID that matches the sub-catchment boundaries in the shape file
      
      data <- amc05
      id="scID"
      
      exc_data <- exc_9596
      
      exc_name <- "TSS_exc1"
      
    } else {
      
      
      
      shp <- shapefile()
      
      data <- userDataset()
      
      data$OBJECTID <- as.integer(data$OBJECTID)
      
      id <- as.character(input$idMatch)
      
      
      shp@data$OBJECTID <- as.integer(shp@data[,id]) 
      
      estimate <- "Estimate"
      error <- "Error"
      name <- input$nameInput

      
      pd <- quote({ pexp(q, rate, lower.tail = FALSE) })
      #---- define argument listing
      args <- quote({ list(rate = 1/data$Estimate) })
      #---- capture distribution and arguments in a single list
      pdflist <- list(dist = pd, args = args, th = quantile(data$Estimate, 0.8))
      
      args_call <- eval(do.call("substitute", list(pdflist$args, list(data$Estimate, data$Error))))
      
      data$exc <- pdist(pname = pdflist$dist, th = pdflist$th, args = args_call)
      exc_data <- data
      exc_name <- "exc"
      
      
    }
    
    shp <- spTransform(shp,"+init=epsg:4326")
    
    
    if(input$useCurrent) {
      exc_data <- data[input$excColCurrent]
      name <- names(exc_data)[1]
    } else {
      
    }
    
    data <- data[order(data[id]),]
    exc_data <- exc_data[order(exc_data[id]),]
    
    return(list(type = "excedance", dataset = data, polygons = as.geojson(shp), exc = exc_data, palette = c(input$glyphCol1, input$glyphCol2), estimate=estimate, error=error, exc_name = exc_name))# map = m, exc = exc_data, palette = c(input$glyphCol1, input$glyphCol2), estimate=estimate, error=error, exc_name = exc_name))
  }
  
  makeTransparent <- function(p) {
    #Make transparent
    return(p + theme(
      plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
      panel.grid.major = element_blank(), # get rid of major grid
      panel.grid.minor = element_blank(), # get rid of minor grid
      panel.border = element_rect(fill = "transparent", color = NA),
      legend.background = element_rect(fill = "transparent"), # get rid of legend bg
      legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
    ))
  }
  
  observeEvent(input$run_map, {
    print("GO")
  })
  
  palette <- reactive({
    build_palette("usr", colrange = list(colour = c(input$paletteL, input$paletteR), difC = c(4, 4)), flipHorizontal = input$flipX, flipVertical = input$flipY)

     })
  
  shpData <- reactive({
    req(input$example)
    
    if(input$plotType == "Bivariate") {
     
      buildBMap(isolate(input$exampleDataset))
    } else if(input$plotType == "Glyph"){
      buildGMap(isolate(input$exampleDataset))
    } else if(input$plotType == "Pixel") {
      buildPMap(isolate(input$exampleDataset))
    } else if(input$plotType == "Excedance") {
      buildEMap(isolate(input$exampleDataset))
    }
  
  })
  
  
  output$bkey <- renderPlot({
    #Render as GGPlot
    if(isolate(input$plotType) == "Bivariate") {
      key <- Vizumap::view(build_bkey(shpData()$dataset, terciles = TRUE, palette = isolate(palette()), flipAxis = isolate(input$axis), transparent=TRUE))
    } else if(input$plotType == "Glyph"){
      key <- Vizumap::view(build_gkey(shpData()$dataset, glyph = "icone", transparent = TRUE))
    } else if(input$plotType == "Pixel" || input$plotType == "Excedance"){
      key <- c()
    }
    #key <- makeTransparent(key)
    return(key)
  }, bg="transparent")
  
  #Build Leaflet
  output$map <- renderLeaflet({
     leaflet(options = leafletOptions()) %>%
      addProviderTiles("Esri.WorldImagery") %>%
      addControl(sliderInput(session$ns("transparency"), "Transparency", min=0, max=1, step=0.01, value = 0.6), position = "topright") %>%
      setView(lat=-26.74561, lng=135.1865, zoom=4)
  })
  
  
  normalize <- function(x) {
    return ((x - min(x)) / (max(x) - min(x)))
  }
  
  observe({
    
    input$transparency
    
    if(shpData()$type == "bivariate") {
    
    session$sendCustomMessage("bivariate", shpData())
        
      
      
    bbox <- as.numeric(shpData()$m$bbox)
    leafletProxy("map") %>%
      clearShapes() %>%
      clearMarkers() %>%
      addPolygons(data = shpData()$polygons, weight = 1, fillColor = ~color, fillOpacity = input$transparency, color = "black", popup = ~paste0(
        names, "</br>
        <strong>Estimate: </strong>", pov, "</br>
        <strong>Margin of Error: </strong>", moe )) %>%
      addPolylines(data= shpData()$polylines, color="black", weight=2, opacity = 1) %>%
      flyToBounds(bbox[1], bbox[2], bbox[3], bbox[4])
    } else if(shpData()$type == "glyph") {

      error <- normalize(shpData()$data$sd)
      error <- 180 * error
      
      leafletProxy("map") %>% 
        clearShapes() %>% 
        clearMarkers()
      
     
      session$sendCustomMessage("glyph", list(data=shpData(), dataset=input$exampleDataset))
      
    } else if(shpData()$type == "pixel") {
      
      leafletProxy("map") %>% 
        clearShapes() %>% 
        clearMarkers()
      
      session$sendCustomMessage("pixel", shpData())
      
    } else if(shpData()$type == "excedance") {
    
      leafletProxy("map") %>%
        clearShapes() %>%
        clearMarkers()

      session$sendCustomMessage("excedance", shpData())
      
    }
  })
  
   observeEvent(input$axis, {
    if(input$axis) {
      colourpicker::updateColourInput(session, "paletteL", "Error")
      colourpicker::updateColourInput(session, "paletteR", "Estimate")
    } else {
      colourpicker::updateColourInput(session, "paletteL", "Estimate")
      colourpicker::updateColourInput(session, "paletteR", "Error")
    }
   
  })
  
  output$palettePlot <- renderPlot({
    if(input$plotType == "Bivariate") {
      Vizumap::view(palette())
    }
    else {
      ggdraw(cowplot::get_legend(
        ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
           geom_point(aes(color = Sepal.Length)) + 
           scale_color_gradient("Estimate", low = input$glyphCol2, high = input$glyphCol1, limit=c(0,1))
        ))
    }
  })
  
  shapefile <- reactive({
    req(input$shapefile)
    
    shpdf <- input$shapefile
    
    # Name of the temporary directory where files are uploaded
    tempdirname <- dirname(shpdf$datapath[1])
    
    # Rename files from FileInput default to named files
    for (i in 1:nrow(shpdf)) {
      file.rename(shpdf$datapath[i], paste0(tempdirname, "/", shpdf$name[i]))
    }
    
    #Read in Shapefile
    reader <- tryCatch({
      readOGR(paste(tempdirname,
                    shpdf$name[grep(pattern = "*.shp$", shpdf$name)],
                    sep = "/"))
    }, error = function(e) {
      shinyalert("Whoops!", "We couldn't read in that shapefile! Make sure you are uploading the 
                  .shp, .shx, .prj and .dbf files all at once so we can read it! 
                  (Hint: Use Ctrl + Click or Click and Drag to select multiple files)", type = "error")
    })
    
    #If error was thrown, return
    if(is.null(reader)) {
      return()
    }
    
    #Translate to Lat-Long object
    return(reader)
    
  })
  
  userDataset <- reactive({
    
    req(input$csvDataset)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$csvDataset$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
   
    return(df)
  })
  
  excDataset <- reactive({
    userDataset <- reactive({
      
      req(input$excDataset)
      
      # when reading semicolon separated files,
      # having a comma separator causes `read.csv` to error
      tryCatch(
        {
          df <- read.csv(input$excDataset$datapath,
                         header = input$header,
                         sep = input$sep,
                         quote = input$quote)
        },
        error = function(e) {
          # return a safeError if a parsing error occurs
          stop(safeError(e))
        }
      )
      return(df)
    })
  })
  
  output$colSelector <- renderUI({
   
    req(input$shapefile)
    req(input$csvDataset)
    
    columns <- unique(c(names(shapefile()@data), names(userDataset())))
    
    return(list(
      selectizeInput(session$ns("idMatch"), "Matching ID (Column in both SHP and CSV)", columns),
      selectizeInput(session$ns("nameInput"), "Column ID that labels the data", columns)
    ))
  })
  
  output$excColCurrent <- renderUI({
    req(input$useCurrent)
    if(input$useCurrent) {
      req(input$shapefile)
      req(input$csvDataset)
      
      return(selectizeInput(session$ns("excColCurrent"), "Excedance Column", unique(c(names(shapefile()@data), names(userDataset())))))
    }
   
  })
  
  
  
  session$sendCustomMessage("init", "")
}
