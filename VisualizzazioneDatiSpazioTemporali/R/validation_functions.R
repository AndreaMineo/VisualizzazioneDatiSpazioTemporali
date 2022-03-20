

`%then%` <- function(a, b) {
  if (is.null(a)) b else a
}



validate_dataFile <- function(filename){

  ext <- tools::file_ext(filename$datapath)

  if(ext %in% c("csv","xlsx")){
    return(NULL)

  }else{

    return("Invalid data file.Use as data file a csv file or a xlsx file")
  }
}


validate_ShapeFile <- function(fileDataFrame){

  l <- length(fileDataFrame$datapath)
  if(l==1){
    ext <- tools::file_ext(fileDataFrame$datapath)
    if(ext == "RData"){

      return(NULL)
    }else{

      return("Invalid shape file. Use as shape file a RData file  or upload multiple files(shp,dbf,prj,shx)")
    }
  }else{

    a <- c("shp"=FALSE,"dbf"=FALSE,"prj"=FALSE,"shx"=FALSE)
    for(i in 1:nrow(fileDataFrame)){
      ext <- tools::file_ext(fileDataFrame$datapath[i])
      if(ext %in% c("shp","prj","dbf","shx")){
        a[ext] <- TRUE
      }
    }
    if(a["shp"] & a["prj"] & a["dbf"] & a["shx"]){

      return(NULL)
    }else{

      return("Error. When uploading multiple files shp,dbf,rpj and shx files have to be included")
    }
  }
}


validate_dataFormat <- function(filename,delimiter){

  extension <- tools::file_ext(filename$datapath)

  if(extension =='csv'){

    data <- tryCatch({read.csv(filename$datapath,sep = delimiter)},error = function(e){return(NULL)})
    if(is.null(data)){
      return("Error,impossible to read selected csv file. Please check if the delimiter value is correct")
    }
  }
  if(extension=='xlsx'){

    data <- readxl::read_excel(filename$datapath)
  }
  s <- data[,1]

  if(!is.character(s)){
    return("Uploaded data is invalid.The first column must be of type character and must contains the timestamps of the time series")
  }

  converted <- tryCatch({as.Date(s)},error = function(e){return(s)})

  ### test if the first column can be converted to Data
  if(class(converted[1])!= "Date"){

    return("Uploaded data is invalid.Impossible to convert the first column into a Date column")
  }else{

    test <- lapply(names(data),function(x) (is.numeric(data[,x])))

    ### test if data contains at least a numeric column
    if(length(grep(TRUE,test))>0){

      return(NULL)
    }else{

      return("Uploaded data is invalid,it doesn't contain any numeric column")
    }
  }

}

validate_mapFormat <- function(filename){

  map <- NULL
  if(length(filename$datapath)==1){
    a <- load(filename$datapath)
    for(i in 1:length(a)){
      if(class(get(a[i]))[1] %in% list('sf','SpatialPolygonsDataFrame')){
        map <- get(a[i])
        break
      }
    }
    if(is.null(map)){


      return("Uploaded map is invalid, RData file must contain an istance of the class sf or an istance of the class SpatialPolygonsDataFrame")
    }else{
      return(NULL)
    }
  }else{
    return(NULL)
  }

}

validate_mapRegNameCol <- function(name,map){

  temp <- map
  names(temp)[names(temp) == name] <- "region_name"
  if(is.character(temp$region_name)){
    return(NULL)
  }else{
    return("The column of map containing regions' name must be of type character")
  }
}

validate_mapLocNameCol <- function(name,map){

  temp <- map
  names(temp)[names(temp) == name] <- "location_name"
  if(is.character(temp$location_name)){
    return(NULL)
  }else{
    return("The column of map containing locations' name must be of type character")
  }
}

validate_dataRegNameCol <- function(name,data,updatedMap){

  if(is.character(data[,name])){
    names_of_map <- sort(unique(updatedMap$region_name))
    names_of_data <- sort(unique(data[,name]))
    if(any(names_of_data %in% names_of_map)){
      return(NULL)
    }else{
      return("Error.None of the entries in the selected column of data have a match in the selected column of map")
    }
  }else{
    return("The column of data containing regions' name must be of type character")
  }

}

validate_dataLocNameCol <- function(name,data,updatedMap){

  if(is.character(data[,name])){
    names_of_map <- sort(unique(updatedMap$location_name))
    names_of_data <- sort(unique(data[,name]))
    if(any(names_of_data %in% names_of_map)){
      return(NULL)
    }else{
      return("Error.None of the entries in the selected column of data have a match in the selected column of map")
    }
  }else{
    return("The column of data containing locations' name must be of type character")
  }

}

validate_variable <- function(name,data){

  if(is.numeric(data[,name])){
    return(NULL)
  }else{
    s <- paste("Error,",name,"is not a numeric column")
    return(s)
  }

}

validate_valuesForLegend <- function(values){

  if(length(grep(",",values))==0){

    return("Error.When inserting values to use in the legend of the spatial plot separate them using a comma")
  }
  if(all(is.na(as.numeric(unlist(strsplit(values,",")))))){
    return("Error.Only numeric values can be used to generate the legend of the spatial plot")
  }

  values_as_numeric <- as.numeric(unlist(strsplit(values,",")))
  sorted_values_as_numeric <- sort(values_as_numeric)

  if(any(sorted_values_as_numeric == values_as_numeric)){
    return(NULL)
  }else{
    return("Error.Values used to generate the legend of the spatial plot must be ordered in ascending order")
  }
}

