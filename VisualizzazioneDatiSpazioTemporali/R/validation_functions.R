

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

  l <- nrow(fileDataFrame)
  if(l==1){
    ext <- tools::file_ext(fileDataFrame$datapath)
    if(ext %in% c("RData","rda")){

      return(NULL)
    }else{

      return("Invalid shape file. Use as shape file a RData file, a rda file or upload multiple files(shp,dbf,prj,shx)")
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

  if(!(is.character(s) | is.numeric(s))){
    return("Uploaded data is invalid.The first column must be of type character or numeric and must contains the timestamps of the time series")
  }

  if(is.character(s)){
    converted <- tryCatch({as.POSIXct(s)},error = function(e){return(s)})
    if(class(converted)[1]!="POSIXct"){
      return("Updated data is invalid. Impossible to convert elements of the first column into POSIXct objects. Use valid formatted strings or a numeric elements as timestamps")
    }
  }


  ### test if the first column can be converted to Data

  test <- lapply(names(data)[-1],function(x) (is.numeric(data[,x])))

    ### test if data contains at least a numeric column
  if(length(grep(TRUE,test))>0){

    return(NULL)
  }else{

    return("Uploaded data is invalid. Data must contains at least a numeric column")
  }

}

validate_ShapeFileFormat <- function(filename,type){
  if(type=="areal"){
    valid_geometry <- c("MULTIPOLYGON","POLYGON")
  }else{
    valid_geometry <- c("MULTIPOLYGON","POLYGON","MULTIPOINT","POINT")
  }
  map <- loadShapeFile(filename,type)
  if(all(sf::st_geometry_type(map,by_geometry = TRUE) %in% valid_geometry)){
    return(NULL)
  }else{
    return(paste("Invalid Map. Spatial data must have one of the following geometry type",as.character(valid_geometry)))
  }
}

validate_RDataFormat <- function(filename,type){
  if(type=="areal"){
    valid_classes <- c("sf","SpatialPolygonsDataFrame","SpatVector")
    valid_geometry_terra <- c("polygons")
    valid_geometry_sf <- c("MULTIPOLYGON","POLYGON")
  }else{
    valid_classes <- c("sf","SpatialPolygonsDataFrame","SpatialPointsDataFrame","SpatVector")
    valid_geometry_terra <- c("polygons","points")
    valid_geometry_sf <- c("MULTIPOLYGON","POLYGON","MULTIPOINT","POINT")
  }
  map <- NULL
  a <- load(filename$datapath)
  for(i in 1:length(a)){
    if(class(get(a[i]))[1] %in% valid_classes){
      map <- get(a[i])
      break
    }
  }
  if(is.null(map)){
    return("Uploaded map is invalid, RData file must contain an istance of one of the following classes (sf,SpatialPolygonsDataFrame,SpatVector)")
  }

  if(class(map)[1]=="sf"){
    if(all(sf::st_geometry_type(map,by_geometry = TRUE) %in% valid_geometry_sf)){
      return(NULL)
    }else{
      return(paste("Invalid Map. The istance of class sf must have on of the following geometry types",paste(valid_geometry_sf,collapse = ",")))
    }
  }
  if(class(map)[1]=="SpatVector"){
    if(all(terra::geomtype(map) %in% valid_geometry_terra)){
      return(NULL)
    }else{
      return(paste("Invalid Map. The istance of class SpatVector must have on of the following geometry types",paste(valid_geometry_terra,collapse = ",")))
    }
  }
}

validate_mapFormat <- function(filename,type){

  l <- nrow(filename)
  if(l==1){
    validate_RDataFormat(filename,type)
  }else{
    validate_ShapeFileFormat(filename,type)
  }
}

validate_mapRegNameCol <- function(name,map){

  temp <- map
  names(temp)[names(temp) == name] <- "region_name"
  if(is.character(temp$region_name) | is.numeric(temp$region_name)){
    return(NULL)
  }else{
    return("The column of map containing regions' name must contain character or numeric elements")
  }
}

validate_mapLocNameCol <- function(name,map){

  temp <- map
  names(temp)[names(temp) == name] <- "location_name"
  if(is.character(temp$location_name)|is.numeric(temp$location_name)){
    return(NULL)
  }else{
    return("The column of map containing locations' name must contain character or numeric elements")
  }
}

validate_dataRegNameCol <- function(name,data,updatedMap){

  if(is.character(data[,name]) | is.numeric(data[,name])){
    names_of_map <- sort(unique(updatedMap$region_name))
    names_of_data <- sort(unique(data[,name]))
    if(any(names_of_data %in% names_of_map)){
      return(NULL)
    }else{
      return("Error.None of the entries in the selected column of data have a match in the selected column of map")
    }
  }else{
    return("The column of data containing regions' name must contain character or numeric elements")
  }

}

validate_dataLocNameCol <- function(name,data,updatedMap){

  if(is.character(data[,name]) | is.numeric(data[,name])){
    names_of_map <- sort(unique(updatedMap$location_name))
    names_of_data <- sort(unique(data[,name]))
    if(any(names_of_data %in% names_of_map)){
      return(NULL)
    }else{
      return("Error.None of the entries in the selected column of data have a match in the selected column of map")
    }
  }else{
    return("The column of data containing locations' name must contain character or numeric elements")
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

  if(all(sorted_values_as_numeric == values_as_numeric)){
    return(NULL)
  }else{
    return("Error. Values used to generate the legend of the spatial plot must be ordered in ascending order")
  }
}



