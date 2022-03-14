library(shiny)
library(sf)
library(shinyWidgets)
library(dygraphs)
library(tmap)





source("utilityFunctions.R")

ui <- fluidPage(
  
  sidebarLayout(
    
    sidebarPanel(
      
      
      #### WIDGET TO LOAD DATA FILE
      fileInput("dataFile", "Selecting data file",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      #### WIDGET TO LOAD SHAPE FILE
      fileInput(inputId = "filemap",
                label = "Upload map. Choose shapefile",
                multiple = TRUE,
                accept = c('.shp','.dbf','.shx','.prj')),
      
      #### WIDGET TO SELECT COLUMN CONTAINING REGIONS' NAMES ON DATA FILE
      
      selectInput("colLocNameDataFile",
                  label= "Selecting the column containing localities' names in the data file",
                  choices=NULL
      ),
      
      #### WIDGET TO SELECT COLUMN CONTAINING REGIONS' NAMES ON SHAPE FILE
      
      selectInput("colLocNameShapeFile",
                  label= "Selecting the column containing locations' names in the shape file",
                  choices=NULL
      ),
      
      #### WIDGET TO SELECT THE VARIABLE TO PLOT
      
      selectInput("variable",
                  label= "Selecting the variable to plot",
                  choices=NULL,
                  selected = NULL
      ),
      
      selectInput("locToPlot",
                  label= "Selecting the set of location to plot for time series plot",
                  choices=NULL,
                  selected = NULL,
                  multiple = TRUE
      ),
      
      ####  WIDGET TO SELECT THE SPECIFIC DATE FOR SPATIAL PLOT 
      
      sliderTextInput("ChoosedDate",
                      label="Selecting date for spatial plot",
                      choices=c("first_date","last_date"),
                      selected = NULL,
                      animate = TRUE
      ),
      
      #### DOWNLOAD BUTTON FOR SPATIAL PLOT ####
      selectInput("FormatDownloadSpatialPlot",label="Select format to use to download the Spatial Plot",choices = c("jpeg","pdf","html"),selected = "jpeg"),
      downloadButton("DownloadSpatialPlot", label = "Download Spatial Plot"),
      
      #### DOWNLOAD BUTTON FOR TIME SERIES PLOT ####
      selectInput("FormatDownloadTimeSeriesPlot",label="Select format to use to download the Time Series Plot",choices = c("jpeg","pdf","html"),selected = "jpeg"),
      downloadButton("DownloadTimeSeriesPlot", label = "Download Time Series Plot")
      
      
      
    ),
    
    mainPanel(
      #### TIME SERIES PLOT
      dygraphOutput("TimeSeriesPlot"),
      
      br(),br(),
      
      ####SPATIAL PLOT
      tmapOutput("SpatialPlot")
    )
  )
)



server <- function(input,output,session){
  
  options(shiny.maxRequestSize=30*1024^2)
  
  ### loading data file
  data <- reactive({
    req(input$dataFile)
    loadDataFile(input$dataFile)
  })
  
  ### loading shape file
  map <- reactive({
    
    req(input$filemap)
    loadShapeFile(input$filemap)
  })
  
  
  #### Updating values in dropdown using to choose locations' name columns in each dataframe
  DataColumns <- reactive({names(data())})
  
  observeEvent(DataColumns(), {
    updateSelectInput(inputId = "colLocNameDataFile", choices = DataColumns()[-1])
  })
  
  ShapeColumns <- reactive({names(map())})
  
  observeEvent(ShapeColumns(), {
    updateSelectInput(inputId = "colLocNameShapeFile", choices = ShapeColumns())
  })
  
  
  ### updating columns names in data 
  updatedMap <- reactive({
    req(input$colLocNameShapeFile)
    renameColumn(map(),input$colLocNameShapeFile,"location_name")
  })
  
  
  ### updating columns names in map 
  updatedData <- reactive({
    req(input$colLocNameDataFile)
    renameColumn(data(),input$colLocNameDataFile,"location_name")
    
  })
  
  
  #### updating values in dropdown using to select the variable to plot
  observeEvent(updatedData(), {
    values <- names(updatedData())[!names(updatedData()) %in% c("date","location_name")]
    updateSelectInput(inputId = "variable", choices = values)
    v <- c("all",unique(updatedData()$location_name))
    updateSelectInput(inputId="locToPlot",choices=v,selected="all")
  })
  
  #### updating values in dates slicer
  observeEvent(data(),{
    
    updateSliderTextInput(session,"ChoosedDate",choices = as.character(data()[,1]))
  })
  
  
  #### getting selected date and variable and locations for generating plots
  date <- reactive({
    req(input$ChoosedDate)
    as.Date(input$ChoosedDate)
    
  })
  
  variable <- reactive({
    req(input$variable)
    input$variable
  })
  
  setOfLocations <- reactive({
    
    req(input$locToPlot)
    input$locToPlot
  })
  
  

  #### generate data for plots
  dataForSpatialPlot <- reactive({
    generateDataForSpatialPlot(updatedData(),updatedMap(),variable(),date())
  })
  
  dataForTimeSeriesPlot <- reactive({
    generateDataForTimeSeriesPlot(updatedData(),variable(),setOfLocations())
  })
  
  #### TIME SERIES PLOT ####
  
  TimeSeriesPlot <- reactive({
    
    dygraph(dataForTimeSeriesPlot()) %>% dyRangeSelector() %>%
      dyLegend(show = 'onmouseover',width = 400)
    
  })
  output$TimeSeriesPlot <- renderDygraph({
    
    TimeSeriesPlot()
  })
  
  
  #### SPATIAL PLOT ####
  
  SpatialPlot <- reactive({
    
    tmap_mode('view')
    tmap_options(check.and.fix = TRUE)
    
    tm_shape(dataForSpatialPlot())+
      tm_symbols(col=variable(),popup.vars = TRUE)
  })
  output$SpatialPlot <- renderTmap({
    
    SpatialPlot()
  })
  
  #### DOWNLOAD TIME SERIES PLOT ####
  
  formatDownloadTimeSeriesPlot <- reactive({
    
    req(input$FormatDownloadTimeSeriesPlot)
    input$FormatDownloadTimeSeriesPlot
  })
  
  
  
  output$DownloadTimeSeriesPlot <- downloadHandler(
    
    filename = function(){
      
      paste("timeSeriesPlot.",formatDownloadTimeSeriesPlot(),sep='')
    }
    ,
    content = function(file){
      
      if(formatDownloadTimeSeriesPlot() == "html"){
        
        htmlwidgets::saveWidget(TimeSeriesPlot(),file = file) 
        
      }
      else{
        
        htmlwidgets::saveWidget(TimeSeriesPlot(),file="temp.html")
        webshot(url="temp.html",file=file)
      }
      
    }
    
  )
  
  
  #### DOWNLOAD SPATIAL PLOT
  
  formatDownloadSpatialPlot <- reactive({
    req(input$FormatDownloadSpatialPlot)
    input$FormatDownloadSpatialPlot
  })
  
  
  output$DownloadSpatialPlot <- downloadHandler(
    
    filename = function(){
      
      paste("SpatialPlot.",formatDownloadTimeSeriesPlot(),sep='')
    }
    ,
    content = function(file){
      
      tmap_save(SpatialPlot(),file=file) 
    }
    
  )
  
  
  
  
  
  
  
}

shinyApp(ui = ui, server = server)
