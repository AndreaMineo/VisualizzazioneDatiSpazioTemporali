
library(tmap)


loadDataFile <- function(filename,delimiter){

  extension <- tools::file_ext(filename$datapath)
  if(extension =='csv'){

    data <- read.csv(filename$datapath,sep=delimiter)
  }
  if(extension=='xlsx'){

    data <- readxl::read_excel(filename$datapath)
  }

  names(data)[1] = "date"
  data$date <- as.Date(data$date)

  return(data)

}


loadShapeFile<- function(InputFilesdf){

  if(nrow(InputFilesdf)==1){
    map <- NULL
    a <- load(InputFilesdf$datapath)
    for(i in 1:length(a)){
      if(class(get(a[i]))[1] %in% list('sf','SpatialPolygonsDataFrame')){
          map <- get(a[i])
          break
        }
      }
  }
  else{
    tempdirname <- dirname(InputFilesdf$datapath[1])

    # Rename files
    for (i in 1:nrow(InputFilesdf)) {
      file.rename(
        InputFilesdf$datapath[i],
        paste0(tempdirname, "/", InputFilesdf$name[i])
      )
    }
    map <- sf::st_read(paste(tempdirname,
                         InputFilesdf$name[grep(pattern = "*.shp$", InputFilesdf$name)],
                         sep = "/"
    ))

  }

  return(map)


}



renameColumn <- function(df,oldName,newName){

  names(df)[names(df) == oldName] <- newName
  return(df)

}

generateDataForSpatialPlotRegion <- function(data,map,variable,date){

  df <- data[data$date == date,c(variable,"region_name")]
  m <- map[,c("region_name")]
  merged <- merge(df,m,by="region_name")

  return(sf::st_as_sf(merged))


}



generateDataForSpatialPlotLocation <- function(data,map,variable,date){

  df <- data[data$date == date,c(variable,"location_name")]
  m <- map[,c("location_name")]
  merged <- merge(df,m,by="location_name")

  return(sf::st_as_sf(merged))


}


generateDataForTimeSeriesPlotRegion <- function(data,variable,setOfLocations){

  if("all" %in% setOfLocations){
    s <- utils::unstack(data[,c(variable,"region_name")])
  }else{

    s <- utils::unstack(data[data$region_name %in% setOfLocations,c(variable,'region_name')])
  }
  s[,"date"] <- (unique(data$date))
  a <- xts::xts(x=s[,-ncol(s)],order.by=s$date)
  return(a)

}




generateDataForTimeSeriesPlotLocation <- function(data,variable,setOfLocations){

  if("all" %in% setOfLocations){
    s <- utils::unstack(data[,c(variable,"location_name")])
  }else{

    s <- utils::unstack(data[data$location_name %in% setOfLocations,c(variable,'location_name')])
  }
  s[,"date"] <- (unique(data$date))
  a <- xts::xts(x=s[,-ncol(s)],order.by=s$date)
  return(a)

}




get_bins <- function(x,data,variable){

  if(x==''){
    x_min <- min(data[,variable])
    x_max <- max(data[,variable])
    interval <- (x_max-x_min)/5

    return(c(x_min,x_min+interval,x_min+2*interval,x_min+3*interval,x_min+4*interval,x_max))
  }else{
    return(as.numeric(unlist(strsplit(x,","))))
  }
}





