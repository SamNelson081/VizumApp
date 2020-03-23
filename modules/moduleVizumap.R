library("Vizumap")
library("leaflet")
library("rgdal")
library("sp")
library("ggplot2")
library("htmltools")
library("colourpicker")

VizumapUI <- function(id) {
  ns <- NS(id)
  return(
    tagList(
      sidebarLayout(
        sidebarPanel = sidebarPanel(
          p("Use Example data from the US or the Great Barrier Reef to run an example"),
          selectizeInput(ns("exampleDataset"), label="Example Dataset", choices=c("US", "GBR")),
          selectizeInput(ns("plotType"), label="Plot Type", choices=c("Bivariate", "Glyph")),
          actionButton(ns("example"), label = "Run Example"),
          hr(),
          #fileInput(ns("shp"), label="Upload Shapefile and Datafile", multiple = T, accept = c("csv", "shp", "shx", "prj", "dbf")),
          #prettyRadioButtons(ns("map_type"), label="Map Type", choices=c("Bivariate", "Pixel", "Glyph"), selected = "Bivariate"),
          #actionButton(ns("run_map"), "Go"),
          #hr(),
          prettyCheckbox(ns("axis"), "Flip Axis", FALSE),
          fluidRow(
            column(6, colourInput(ns("paletteL"), "Estimate", "blue")),
            column(6, colourInput(ns("paletteR"), "Error", value="yellow")),
          ),
          fluidRow(
            column(6, prettyCheckbox(ns("flipX"), "Flip Horizontal", FALSE)),
            column(6, prettyCheckbox(ns("flipY"), "Flip Vertical", FALSE)),
          ),
          plotOutput(ns("palettePlot")),
          width = 2
        ),
        mainPanel = mainPanel(
          leafletOutput(ns("map"), width="100%", height = "1300px")#"calc(100vh - 100px)") 
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
      )
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
        shp <- readOGR(dsn = "./data/USshp", layer = "05000")
        
        estimate = "pov_rate"
        error = "pov_moe"
        
        data <- read.uv(data = us_data, estimate = estimate, error = error)
        
        id="GEO_ID"
        
      } else {
        
        load("./data/GBR/burd_data.rda")
        load("./data/GBR/burd_geo.rda")
        load("./data/GBR/UB.RData")
        
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
      }
      
      shp <- spTransform(shp,"+init=epsg:4326")
      
      
      #Get the data
      incProgress(1/4, detail = paste("Building map and key..."))
      
      print("F")
      m <<- build_gmap(data = data, shapefile = shp, id = id, border = "state", palette = "Purples")
      
    })
  }
  
  
  buildBMap <- function(datatype) {
    withProgress(message = 'Running Example', value = 0, {
      
      incProgress(1/4, detail = paste("Reading in Data Files..."))
      
      if(datatype == "US") {
        
        #Load data
        data(us_data)
        
        #Relative path to my machine
        shp <- readOGR(dsn = "./data/USshp", layer = "05000")
        
        estimate = "pov_rate"
        error = "pov_moe"
        
        data <- read.uv(data = us_data, estimate = estimate, error = error)
        
        id="GEO_ID"
        
      } else {
        
        load("./data/GBR/burd_data.rda")
        load("./data/GBR/burd_geo.rda")
        load("./data/GBR/UB.RData")
        
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
      }
      
      shp <- spTransform(shp,"+init=epsg:4326")
      
      
      #Get the data
      incProgress(1/4, detail = paste("Building map and key..."))
      

      #Bivariate map
      m <- build_bmap(data = data, shapefile = shp, id = id, border = "state", terciles = TRUE, palette = isolate(palette()), flipAxis = isolate(input$axis))
      m$output_data$hex_code <- as.character(m$output_data$hex_code)
      
      #Key
      #k <- build_bkey(data, terciles = TRUE, flipAxis = input$axis)
      
      incProgress(1/4, detail = paste("Creating Spatial Objects..."))
      
      #County Polygons
      split_data_poly <- lapply(unique(m$output_data$group), function(x) {
        df <- as.matrix(m$output_data[m$output_data$group == x, c("long", "lat")])
        polys <- Polygons(list(Polygon(df)), ID = x)
        return(polys)
        return(df)
      })
      
      #Create a Spatial Polygons Dataframe including the Spatial data and Hex Code 
      if(datatype == "US") {
        data_polys <- SpatialPolygonsDataFrame(
          SpatialPolygons(split_data_poly),
          data.frame(
            row.names= unique(m$output_data$group), 
            color=unique(cbind(m$output_data$group, m$output_data$hex_code))[,2],
            names=unique(cbind(m$output_data$group, m$output_data$NAME))[,2],
            pov=unique(cbind(m$output_data$group, m$output_data$pov_rate))[,2],
            moe=unique(cbind(m$output_data$group, m$output_data$pov_moe))[,2]
          )
        )
      } else {
        data_polys <- SpatialPolygonsDataFrame(
          SpatialPolygons(split_data_poly),
          data.frame(
            row.names= unique(m$output_data$group), 
            color=unique(cbind(m$output_data$group, m$output_data$hex_code))[,2],
            names=unique(cbind(m$output_data$group, m$output_data$SUBBASIN))[,2],
            pov=unique(cbind(m$output_data$group, m$output_data$m))[,2],
            moe=unique(cbind(m$output_data$group, m$output_data$sd))[,2]
          )
        )
      }
      
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
    
    return(list(dataset = data, polygons = data_polys, polylines = data_lines, map = m))
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
    } else {
      buildGMap(isolate(input$exampleDataset))
    }
  
  })
  
  
  output$bkey <- renderPlot({
    #Render as GGPlot
    key <- view(build_bkey(shpData()$dataset, terciles = TRUE, palette = isolate(palette()), flipAxis = isolate(input$axis)))
    #key <- makeTransparent(key)
    return(key)
  }, bg="transparent")
  
  #Build Leaflet
  output$map <- renderLeaflet({
     leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
      addProviderTiles("Esri.WorldImagery") %>%
      addControl(sliderInput(session$ns("transparency"), "Transparency", min=0, max=1, step=0.01, value = 0.6), position = "topright") %>%
      setView(lat=-26.74561, lng=135.1865, zoom=4)
     
  })
  
  observe({
    bbox <- as.numeric(shpData()$m$bbox)
    leafletProxy("map") %>%
      clearShapes() %>%
      addPolygons(data = shpData()$polygons, weight = 1, fillColor = ~color, fillOpacity = input$transparency, color = "black", popup = ~paste0(
        names, "</br>
        <strong>Estimate: </strong>", pov, "</br>
        <strong>Margin of Error: </strong>", moe )) %>%
      addPolylines(data= shpData()$polylines, color="black", weight=2, opacity = 1) %>%
      flyToBounds(bbox[1], bbox[2], bbox[3], bbox[4])
      #flyToBounds(-148.2715, 60.32695, -30.41016, 4.740675) US Values
    
  })
  
   observeEvent(input$axis, {
    if(input$axis) {
      updateColourInput(session, "paletteL", "Error")
      updateColourInput(session, "paletteR", "Estimate")
    } else {
      updateColourInput(session, "paletteL", "Estimate")
      updateColourInput(session, "paletteR", "Error")
    }
   
  })
  
  output$palettePlot <- renderPlot({
    makeTransparent(view(palette()))
  })
}
