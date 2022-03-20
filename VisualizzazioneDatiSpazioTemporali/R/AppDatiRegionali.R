#' Applicazione per la visualizzazione di dati spazio temporali caso dati regionali
#'
#' @description Shiny app per la visualizzazione di dati spazio temporali.
#' Per maggiori informazioni riferirsi a:
#'
#' https://github.com/AndreaMineo/VisualizzazioneDatiSpazioTemporali
#' @export



AppDatiRegionali <- function(){

  ui <- shiny::fluidPage(

    shiny::titlePanel(
      shiny::h1("Spatio-Temporal data visualization",align="center")
    ),

    shiny::sidebarLayout(

      shiny::sidebarPanel(

        shiny::h4("User's Input"),

        shiny::fileInput(inputId = "filemap",
                         label = "Upload map. Choose shapefile",
                         multiple = TRUE,
                         accept = c('.shp','.dbf','.shx','.prj',".RData")),

        shiny::fluidRow(
        shiny::column(10,shiny::fileInput("dataFile", "Selecting data file",
                         multiple = FALSE,
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv","xlsx"))
        ),
        shiny::column(2,shiny::selectInput("delimiter",label="delimiter",choices =c(",",";","/"),selected = ","))
      ),


        shiny::selectInput("colRegNameShapeFile",
                           label= "Selecting the column containing regions' names in the shape file",
                           choices=NULL
        ),




        shiny::selectInput("colRegNameDataFile",
                           label= "Selecting the column containing regions' names in the data file",
                           choices=NULL
        ),



        shiny::selectInput("variable",
                           label= "Selecting the variable to plot",
                           choices=NULL,
                           selected = NULL
        ),
        shiny::selectInput("regToPlot",
                           label= "Selecting the set of regions to plot for time series plot",
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
        shiny::downloadButton("DownloadTimeSeriesPlot", label = "Download Time Series Plot"),



      ),

      shiny::mainPanel(

        #### TIME SERIES PLOT
        shiny::h4("Time Series Plot"),
        dygraphs::dygraphOutput("TimeSeriesPlot"),

        shiny::br(),shiny::br(),

        ####SPATIAL PLOT
        shiny::h4("Spatial Plot(spatial entities as regions in the space)"),
        tmap::tmapOutput("SpatialPlot")
      )
    )
  )



  server <- function(input,output,session){

    options(shiny.maxRequestSize=30*1024^2)


    ### loading data file
    dataFileName <- shiny::reactive({
      shiny::req(input$dataFile)
      input$dataFile
    })

    delimiter <- shiny::reactive({

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
          validate_mapFormat(shapeFileName())
      )
      loadShapeFile(shapeFileName())
    })



    DataColumns <- shiny::reactive({names(data())})

    shiny::observeEvent(DataColumns(), {
      shiny::updateSelectInput(inputId = "colRegNameDataFile", choices = DataColumns()[-1])
    })

    ShapeColumns <- shiny::reactive({names(map())})

    shiny::observeEvent(ShapeColumns(), {
      shiny::updateSelectInput(inputId = "colRegNameShapeFile", choices = ShapeColumns())
    })



    mapRegNameCol <- shiny::reactive({
      shiny::req(input$colRegNameShapeFile)
      input$colRegNameShapeFile
    })


    updatedMap <- shiny::reactive({
      shiny::validate(
        validate_mapRegNameCol(mapRegNameCol(),map())
      )
      renameColumn(map(),mapRegNameCol(),"region_name")
    })




    dataRegNameCol <- shiny::reactive({
      shiny::req(input$colRegNameDataFile)
      input$colRegNameDataFile

    })

    updatedData <- shiny::reactive({
      shiny::validate(
        validate_mapRegNameCol(mapRegNameCol(),map()) %then%
          validate_dataRegNameCol(dataRegNameCol(),data(),updatedMap())
      )
      renameColumn(data(),dataRegNameCol(),"region_name")

    })



    shiny::observeEvent(updatedData(), {
      values <- names(updatedData())[!names(updatedData()) %in% c("date","region_name")]
      shiny::updateSelectInput(inputId = "variable", choices = values)
      v <- c("all",unique(updatedData()$region_name))
      shiny::updateSelectInput(inputId="regToPlot",choices=v,selected="all")
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

    setOfRegations <- shiny::reactive({

      shiny::req(input$regToPlot)
      input$regToPlot
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
      generateDataForSpatialPlotRegion(updatedData(),updatedMap(),variable(),date())
    })

    dataForTimeSeriesPlot <- shiny::reactive({
      generateDataForTimeSeriesPlotRegion(updatedData(),variable(),setOfRegations())
    })



    TimeSeriesPlot <- shiny::reactive({

      d <- dygraphs::dyRangeSelector(dygraphs::dygraph(dataForTimeSeriesPlot()))
      dygraphs::dyLegend(d,show = 'onmouseover',width = 400)

    })
    output$TimeSeriesPlot <- dygraphs::renderDygraph({

      TimeSeriesPlot()
    })




    SpatialPlot <- shiny::reactive({

      tmap::tmap_mode('view')
      tmap::tmap_options(check.and.fix = TRUE)

      tmap::tm_shape(dataForSpatialPlot())+
        tmap::tm_polygons(col=variable(),breaks = ValuesForLegend())
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
