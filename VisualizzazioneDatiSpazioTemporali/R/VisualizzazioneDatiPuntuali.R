#' Applicazione per la visualizzazione di dati spazio temporali caso dati puntuali
#'
#' @description Shiny app per la visualizzazione di dati spazio temporali.
#' Per maggiori informazioni riferirsi a:
#'
#' https://github.com/AndreaMineo/VisualizzazioneDatiSpazioTemporali
#' @export



VisualizzazioneDatiPuntuali <- function(){

  ui <- shiny::fluidPage(

    shiny::titlePanel("Spatio-Temporal data visualization"),
    shiny::sidebarLayout(

      shiny::sidebarPanel(

        shiny::h4("User's Input"),

        shiny::fileInput(inputId = "filemap",
                         label = "Upload map file",
                         multiple = TRUE,
                         accept = c('.shp','.dbf','.shx','.prj',".RData")),

        shiny::fluidRow(
          shiny::column(9,shiny::fileInput("dataFile", "Selecting data file",
                                            multiple = FALSE,
                                            accept = c("text/csv",
                                                       "text/comma-separated-values,text/plain",
                                                       ".csv","xlsx"))
          ),
          shiny::column(3,shiny::selectInput("delimiter",label="delimiter",choices =c(",",";","/"),selected = ","))
        ),


        #### WIDGET TO SELECT COLUMN CONTAINING LOCATIONS' NAMES ON SHAPE FILE

        shiny::selectInput("colLocNameShapeFile",
                           label= "Selecting the column containing locations' names in the shape file",
                           choices=NULL
        ),




        shiny::selectInput("colLocNameDataFile",
                           label= "Selecting the column containing locations' names in the data file",
                           choices=NULL
        ),



        shiny::selectInput("variable",
                           label= "Selecting the variable to plot",
                           choices=NULL,
                           selected = NULL
        ),
        shiny::selectInput(inputId = "PlotMode",
                           label="Selecting plot mode",
                           choices = c("With basemap","Without basemap"),
                           selected = "With basemap"),
        shiny::selectInput("locToPlot",
                           label= "Selecting the set of location to plot for time series plot",
                           choices=NULL,
                           selected = NULL,
                           multiple = TRUE
        ),



        shinyWidgets::sliderTextInput("ChoosedDate",
                                      label="Selecting date for spatial plot",
                                      choices=c("first_date","last_date"),
                                      selected = NULL,
                                      animate = TRUE
        ),



        shiny::textAreaInput(
          inputId="ValuesForLegend",
          label="Insert values to use in the legend of the spatial plot",
          value = "",
          placeholder = "value1,value2,...,valueN"
        ),

        shiny::selectInput("FormatDownloadSpatialPlot",label="Select format to use to download the Spatial Plot",choices = c("jpeg","pdf","html"),selected = "jpeg"),
        shiny::downloadButton("DownloadSpatialPlot", label = "Download Spatial Plot"),


        shiny::selectInput("FormatDownloadTimeSeriesPlot",label="Select format to use to download the Time Series Plot",choices = c("jpeg","pdf","html"),selected = "jpeg"),
        shiny::downloadButton("DownloadTimeSeriesPlot", label = "Download Time Series Plot")



      ),

      shiny::mainPanel(

        #### TIME SERIES PLOT
        shiny::h4("Time Series Plot"),
        dygraphs::dygraphOutput("TimeSeriesPlot"),

        shiny::br(),shiny::br(),

        ####SPATIAL PLOT
        shiny::h4("Spatial Plot"),
        tmap::tmapOutput("SpatialPlot")
      )
    )
  )



  server <- function(input,output,session){

    options(shiny.maxRequestSize=500*1024^2)


    ### loading data file
    dataFileName <- shiny::reactive({
      shiny::req(input$dataFile)
      input$dataFile
    })

    delimiter <- reactive({
      req(input$delimiter)
      input$delimiter
    })
    data <- shiny::reactive({
      shiny::validate(
        validate_dataFile(dataFileName()) %then%
          validate_dataFormat(dataFileName(),delimiter())
      )
      loadDataFile(dataFileName(),delimiter())
    })



    ### loading shape file

    shapeFileName <- shiny::reactive({

      shiny::req(input$filemap)
      input$filemap
    })

    map <- shiny::reactive({
      shiny::validate(

        validate_ShapeFile(shapeFileName())%then%
          validate_mapFormat(shapeFileName(),"geostatistic")
      )
      loadShapeFile(shapeFileName(),"geostatistical")
    })



    DataColumns <- shiny::reactive({names(data())})

    shiny::observeEvent(DataColumns(), {
      shiny::updateSelectInput(inputId = "colLocNameDataFile", choices = DataColumns()[-1])
    })

    ShapeColumns <- shiny::reactive({names(map())})

    shiny::observeEvent(ShapeColumns(), {
      shiny::updateSelectInput(inputId = "colLocNameShapeFile", choices = ShapeColumns())
    })



    mapLocNameCol <- shiny::reactive({
      shiny::req(input$colLocNameShapeFile)
      input$colLocNameShapeFile
    })


    updatedMap <- shiny::reactive({
      shiny::validate(
        validate_mapLocNameCol(mapLocNameCol(),map())
      )
      renameColumn(map(),mapLocNameCol(),"location_name")
    })




    dataLocNameCol <- shiny::reactive({
      shiny::req(input$colLocNameDataFile)
      input$colLocNameDataFile

    })

    updatedData <- shiny::reactive({
      shiny::validate(
        validate_mapLocNameCol(mapLocNameCol(),map()) %then%
          validate_dataLocNameCol(dataLocNameCol(),data(),updatedMap())
      )
      renameColumn(data(),dataLocNameCol(),"location_name")

    })



    shiny::observeEvent(updatedData(), {
      values <- names(updatedData())[!names(updatedData()) %in% c("date","location_name")]
      shiny::updateSelectInput(inputId = "variable", choices = values)
      v <- c("all",unique(updatedData()$location_name))
      shiny::updateSelectInput(inputId="locToPlot",choices=v,selected="all")
    })


    shiny::observeEvent(data(),{
      s <- unique(data()[,1])
      shinyWidgets::updateSliderTextInput(session=session,inputId = "ChoosedDate",choices = as.character(s))
    })



    date <- shiny::reactive({
      shiny::req(input$ChoosedDate)
      as.Date(input$ChoosedDate)

    })

    variable <- shiny::reactive({
      shiny::req(input$variable)
      shiny::validate(
        validate_variable(input$variable,updatedData())
      )
      input$variable
    })

    setOfLocations <- shiny::reactive({

      shiny::req(input$locToPlot)
      input$locToPlot
    })



    shiny::observeEvent(variable(),{

      values <- get_bins("",data(),variable())
      shiny::updateTextAreaInput(inputId = "ValuesForLegend",value = values)
    })

    ValuesForLegend <- shiny::reactive({

      shiny::req(input$ValuesForLegend)
      shiny::validate(
        validate_valuesForLegend(input$ValuesForLegend)
      )
      get_bins(input$ValuesForLegend,data(),variable())
    })





    dataForSpatialPlot <- shiny::reactive({
      generateDataForSpatialPlotLocation(updatedData(),updatedMap(),variable(),date())
    })

    dataForTimeSeriesPlot <- shiny::reactive({
      generateDataForTimeSeriesPlotLocation(updatedData(),variable(),setOfLocations())
    })



    TimeSeriesPlot <- shiny::reactive({

      d <- dygraphs::dyRangeSelector(dygraphs::dygraph(dataForTimeSeriesPlot()))
      dygraphs::dyLegend(d,show = 'onmouseover',width = 400)

    })
    output$TimeSeriesPlot <- dygraphs::renderDygraph({

      TimeSeriesPlot()
    })


    PlotMode <- shiny::reactive({
      req(input$PlotMode)
      input$PlotMode
    })

    SpatialPlot <- shiny::reactive({


      if(PlotMode()=="With basemap"){
        tmap::tmap_options(check.and.fix = TRUE,basemaps.alpha = 1)
      }else{
        tmap::tmap_options(check.and.fix = TRUE,basemaps.alpha = 0)
      }


      tmap::tm_shape(dataForSpatialPlot())+
        tmap::tm_symbols(col=variable(),popup.vars = TRUE,breaks =ValuesForLegend())
    })

    output$SpatialPlot <- tmap::renderTmap({

      SpatialPlot()
    })



    formatDownloadTimeSeriesPlot <- shiny::reactive({

      shiny::req(input$FormatDownloadTimeSeriesPlot)
      input$FormatDownloadTimeSeriesPlot
    })

    formatDownloadSpatialPlot <- shiny::reactive({

      shiny::req(input$FormatDownloadSpatialPlot)
      input$FormatDownloadSpatialPlot
    })


    output$DownloadTimeSeriesPlot <- shiny::downloadHandler(

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
          webshot::webshot(url="temp.html",file=file)
          unlink("temp.html")
        }

      }

    )




    output$DownloadSpatialPlot <- shiny::downloadHandler(

      filename = function(){

        paste("spatialPlot.",formatDownloadSpatialPlot(),sep='')
      }
      ,
      content = function(file){

        if(formatDownloadSpatialPlot() == "html"){
          tmap::tmap_save(SpatialPlot(),file=file)
        }else{

          tmap::tmap_save(SpatialPlot(),"temp.html")
          webshot::webshot(url="temp.html",file=file)
          unlink("temp.html")
        }


      }

    )

  }

  shiny::shinyApp(ui = ui, server = server)

}
