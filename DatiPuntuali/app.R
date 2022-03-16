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
                           ".csv","xlsx")),
      
      #### WIDGET TO LOAD SHAPE FILE
      fileInput(inputId = "filemap",
                label = "Upload map. Choose shapefile",
                multiple = TRUE,
                accept = c('.shp','.dbf','.shx','.prj',".RData")),
     
      
      #### WIDGET TO SELECT COLUMN CONTAINING LOCATIONS' NAMES ON SHAPE FILE
      
      selectInput("colLocNameShapeFile",
                  label= "Selecting the column containing locations' names in the shape file",
                  choices=NULL
      ),
      
      
      #### WIDGET TO SELECT COLUMN CONTAINING LOCATIONS' NAMES ON DATA FILE
      
      selectInput("colLocNameDataFile",
                  label= "Selecting the column containing locations' names in the data file",
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
      
      ####  TEXT AREA TO INSERT TO USE EXTREMA OF INTERVALS TO USE IN THE LEGEND OF THE SPATIAL PLOT ####
      
      textAreaInput(
        inputId="ValuesForLegend",
        label="Insert values to use in the legend of the spatial plot",
        value = "",
        placeholder = "value1,value2,...,valueN"
      ),
      
      #### DOWNLOAD BUTTON FOR SPATIAL PLOT ####
      selectInput("FormatDownloadSpatialPlot",label="Select format to use to download the Spatial Plot",choices = c("jpeg","pdf","html"),selected = "jpeg"),
      downloadButton("DownloadSpatialPlot", label = "Download Spatial Plot"),
      
      #### DOWNLOAD BUTTON FOR TIME SERIES PLOT ####
      selectInput("FormatDownloadTimeSeriesPlot",label="Select format to use to download the Time Series Plot",choices = c("jpeg","pdf","html"),selected = "jpeg"),
      downloadButton("DownloadTimeSeriesPlot", label = "Download Time Series Plot")
      
      
      
    ),
    
    mainPanel(
      
      #### ERROR TEXT OUTPUT ####
      
      textOutput("error"),
      
      br(),br(),
      
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
  dataFileName <- reactive({
    req(input$dataFile)
    input$dataFile
  })
  
  
  data <- reactive({
    validate(
      validate_dataFile(dataFileName()) %then%
      validate_dataFormat(dataFileName())
    )
    loadDataFile(dataFileName())
  })
  
  
  
  ### loading shape file
  
  shapeFileName <- reactive({
    
    req(input$filemap)
    input$filemap
  })
  
  map <- reactive({
    validate(
      
      validate_ShapeFile(shapeFileName())%then%
      validate_mapFormat(shapeFileName())
    )
    loadShapeFile(shapeFileName())
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
  
  mapLocNameCol <- reactive({
    req(input$colLocNameShapeFile)
    input$colLocNameShapeFile
  })
  
  
  updatedMap <- reactive({
    validate(
      validate_mapLocNameCol(mapLocNameCol(),map())
    )
    renameColumn(map(),mapLocNameCol(),"location_name")
  })
  
  
  ### updating columns names in map 
  
  dataLocNameCol <- reactive({
    req(input$colLocNameDataFile)
    input$colLocNameDataFile
    
  })
  
  updatedData <- reactive({
    validate(
      validate_mapLocNameCol(mapLocNameCol(),map()) %then%
      validate_dataLocNameCol(dataLocNameCol(),data(),updatedMap())
    )
    renameColumn(data(),dataLocNameCol(),"location_name")
    
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
  
  
  #### getting selected date,variable and locations for generating plots
  date <- reactive({
    req(input$ChoosedDate)
    as.Date(input$ChoosedDate)
    
  })
  
  variable <- reactive({
    req(input$variable)
    validate(
      validate_variable(input$variable,updatedData())
    )
    input$variable
  })
  
  setOfLocations <- reactive({
    
    req(input$locToPlot)
    input$locToPlot
  })
  
  ###update TextArea default value
  
  observeEvent(variable(),{
    
    values <- get_bins("",data(),variable())
    updateTextAreaInput(inputId = "ValuesForLegend",value = values)
  })
  
  ValuesForLegend <- reactive({
    
    req(input$ValuesForLegend)
    validate(
      validate_valuesForLegend(input$ValuesForLegend)
    )
    get_bins(input$ValuesForLegend,data(),variable())
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
      tm_symbols(col=variable(),popup.vars = TRUE,breaks =ValuesForLegend())
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
  
  output$error <- renderText({
    
    validate(
      validate_dataFile(dataFileName())%then%
        validate_dataFormat(dataFileName()),
      validate_ShapeFile(shapeFileName())%then%
        validate_mapFormat(shapeFileName()),
      validate_mapLocNameCol(mapLocNameCol(),map())%then%
        validate_dataLocNameCol(dataLocNameCol(),data(),updatedMap())
    )
  })
  
  
  
  
  
}

shinyApp(ui = ui, server = server)

