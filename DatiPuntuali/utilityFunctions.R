library(readxl)
library(sf)
library(tools)
library(xts)



#### Function used to load data file as a dataframe
loadDataFile <- function(filename){
  
  extension <- file_ext(filename$datapath)
  
  if(extension =='csv'){
    
    data <- read.csv(filename$datapath)
  }
  if(extension=='xlsx'){
    
    data <- read_excel(filename$datapath)
  }
  
  names(data)[1] = "date"
  data$date <- as.Date(data$date)
  
  return(data) 
  
}

#### function used to load shape file as a sf dataframe
loadShapeFile<- function(InputFilesdf){
  
  if(length(InputFilesdf$datapath)==1){
    extension <- file_ext(InputFilesdf$datapath)
    map <- NULL
    if(extension== 'RData'){
      a <- load(InputFilesdf$datapath)
      for(i in 1:length(a)){
        
        if(class(get(a[i]))[1] %in% list('sf','SpatialPolygonsDataFrame')){
          map <- get(a[i])
          break
        }
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
    map <- st_read(paste(tempdirname,
                         InputFilesdf$name[grep(pattern = "*.shp$", InputFilesdf$name)],
                         sep = "/"
    ))
    
  }
  
  return(map)
  
  
}

#### function used to rename a column of a dataframe

renameColumn <- function(df,oldName,newName){
  
  names(df)[names(df) == oldName] <- newName
  return(df)
  
}


### function used to generate data for spatial plot

generateDataForSpatialPlot <- function(data,map,variable,date){
  
  df <- data[data$date == date,c(variable,"location_name")]
  m <- map[,c("location_name")] 
  merged <- merge(df,m,by="location_name")
  
  return(st_as_sf(merged))
  
  
}


### function used to generate data for time series plot

generateDataForTimeSeriesPlot <- function(data,variable,setOfLocations){
  
  if("all" %in% setOfLocations){
    s <- unstack(data[,c(variable,"location_name")])
  }else{
    
    s <- unstack(data[data$location_name %in% setOfLocations,c(variable,'location_name')])
  }
  
  s[,"date"] <- (unique(data$date))
  a <- xts(x=s[,-ncol(s)],order.by=s$date)
  return(a)
               
}


### function used to get the bins to use in the legend of the spatial plot

get_bins <- function(x,data,variable){
  
  if(x==''){
    x_min <- min(data[,variable])
    x_max <- max(data[,variable])
    interval <- (x_max-x_min)/6
    
    return(c(x_min,x_min+interval,x_min+2*interval,x_min+3*interval,x_min+4*interval,x_max))
  }else{
    
    return(as.numeric(unlist(strsplit(x,","))))
  }
}




