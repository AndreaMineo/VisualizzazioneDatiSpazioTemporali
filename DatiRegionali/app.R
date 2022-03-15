library(shiny)
library(sf)
library(shinyWidgets)
library(dygraphs)
library(tmap)
library(webshot)





source("utilityFunctions.R")

ui <- fluidPage(
  
  sidebarLayout(
    
    sidebarPanel(
      
      
      #### WIDGET TO LOAD DATA FILE
      fileInput("dataFile", "Selecting data file",
                multiple = FALSE,
                accept = c(".xlsx",
                           ".csv")),
      
      #### WIDGET TO LOAD SHAPE FILE
      fileInput(inputId = "filemap",
                label = "Upload map. Choose shapefile",
                multiple = TRUE,
                accept = c('.shp','.dbf','.shx','.prj')),
      
      #### WIDGET TO SELECT COLUMN CONTAINING REGIONS' NAMES ON DATA FILE
      
      selectInput("colRegNameDataFile",
        label= "Selecting the column containing regions' names in the data file",
        choices=NULL
      ),
      
      #### WIDGET TO SELECT COLUMN CONTAINING REGIONS' NAMES ON SHAPE FILE
      
      selectInput("colRegNameShapeFile",
                  label= "Selecting the column containing regions' names in the shape file",
                  choices=NULL
      ),
      
      #### WIDGET TO SELECT THE VARIABLE TO PLOT
      
      selectInput("variable",
                  label= "Selecting the variable to plot",
                  choices=NULL,
                  selected = NULL
      ),
      
      selectInput("RegToPlot",
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
        placeholder = "value1,value2,value3,value4,value5,value6"
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
      
      br(), br(),
      
      ####SPATIAL PLOT
      tmapOutput("SpatialPlot")
    )
  )
)



server <- function(input,output,session){
  
  dataFileName <- reactive({
    req(input$dataFile)
    input$dataFileName
  })
  
  
  ### loading data file
 data <- reactive({
   req(input$dataFile)
   validate(
     need(file_ext(dataFileName()$datapath) %in% c("csv","xlsx"),"Invalid")
   )
   loadDataFile(input$dataFile)
   })
 
 ### loading shape file
 map <- reactive({
   
   req(input$filemap)
   loadShapeFile(input$filemap)
 })
  
 
 #### Updating values in dropdown using to choose regions' name columns in each dataframe
 DataColumns <- reactive({names(data())})
 
 observeEvent(DataColumns(), {
   updateSelectInput(inputId = "colRegNameDataFile", choices = DataColumns()[-1])
 })
 
 ShapeColumns <- reactive({names(map())})
 
 observeEvent(ShapeColumns(), {
   updateSelectInput(inputId = "colRegNameShapeFile", choices = ShapeColumns())
 })
 
 
 ### updating columns names in data 
 updatedMap <- reactive({
   req(input$colRegNameShapeFile)
   renameColumn(map(),input$colRegNameShapeFile,"region_name")
 })
 
 
 ### updating columns names in map 
 updatedData <- reactive({
   req(input$colRegNameDataFile)
   renameColumn(data(),input$colRegNameDataFile,"region_name")
   
 })
 
 
 #### updating values in dropdown using to select the variable to plot
 observeEvent(updatedData(), {
   values = names(updatedData())[!names(updatedData()) %in% c("date","region_name")]
   updateSelectInput(inputId = "variable", choices = values)
   v <- c("all",unique(updatedData()$region_name))
   updateSelectInput(inputId="RegToPlot",choices=v,selected="all")
 })
 
 #### updating values in dates slicer
 observeEvent(data(),{
   
   updateSliderTextInput(session,"ChoosedDate",choices = as.character(data()[,1]))
 })
 

 #### getting selected date,variable,setOfRegions,ValuesForLegend
 
 date <- reactive({
   req(input$ChoosedDate)
   as.Date(input$ChoosedDate)
 
 })
 
 variable <- reactive({
   req(input$variable)
   input$variable
 })
 
 setOfRegions <- reactive({
   
   req(input$RegToPlot)
   input$RegToPlot
 })
 
 
 ###update TextArea default value
 
 observeEvent(variable(),{
   
   values <- get_bins("",data(),variable())
   updateTextAreaInput(inputId = "ValuesForLegend",value = values)
 })
 
 ValuesForLegend <- reactive({
   
   req(input$ValuesForLegend)
   get_bins(input$ValuesForLegend,data(),variable())
 })
 
 
 #### generate data for plots
 dataForSpatialPlot <- reactive({
   generateDataForSpatialPlot(updatedData(),updatedMap(),variable(),date())
 })
 
 dataForTimeSeriesPlot <- reactive({
   generateDataForTimeSeriesPlot(updatedData(),variable(),setOfRegions ())
 })
 
 
 #### TIMESERIES PLOT 
 
 TimeSeriesPlot <- reactive({
    
   dygraph(dataForTimeSeriesPlot()) %>% dyRangeSelector() %>%
     dyLegend(show = 'onmouseover',width = 400)
   
 })
 
 output$TimeSeriesPlot <- renderDygraph({
   validate(
     need(file_ext(dataFileName()$datapath) %in% c("csv","xlsx"),"Invalid")
   )
    TimeSeriesPlot()
 })
 
 
 
 #### SPATIAL PLOT 
 
 SpatialPlot <- reactive({
   
   tmap_mode('view')
   tmap_options(check.and.fix = TRUE)
   
   tm_shape(dataForSpatialPlot())+
     tm_polygons(col=variable(),breaks = ValuesForLegend())
 })
 
 output$SpatialPlot <- renderTmap({
  
   validate(
     need(file_ext(dataFileName()$datapath) %in% c("csv","xlsx"),"Invalid")
   )
    SpatialPlot()
 })
 
 
 #### DOWNLOAD TIME SERIES PLOT 
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
