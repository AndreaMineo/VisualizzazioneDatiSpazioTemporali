library(tools)

### using to chain condition together

`%then%` <- function(a, b) {
  if (is.null(a)) b else a
}


validate_dataFile <- function(filename){
  
  ext <- file_ext(filename$datapath)
  
  if(ext %in% c("csv","xlsx")){
    return(NULL)
  }else{
    
    return("Invalid data file.Use as data file a csv file or a xlsx file")
  }
}


validate_ShapeFile <- function(fileDataFrame){
  
  l <- length(fileDataFrame$datapath)
  if(l==1){
    ext <- file_ext(fileDataFrame$datapath)
    if(ext == "RData"){
      
      return(NULL)
    }else{
      
      return("Invalid shape file. Use as shape file a RData file  or upload multiple files(shp,dbf,prj,shx)")
    }
  }else{
    
    a <- c("shp"=FALSE,"dbf"=FALSE,"prj"=FALSE,"shx"=FALSE)
    for(i in 1:nrow(fileDataFrame())){
      ext <- file_ext(fileDataFrame()$datapath[i])
      if(ext %in% c("shp","rpj","dbf","shx")){
        a[ext] <- TRUE
      }
    }
    
    if(a["shp"] & a["rpj"] & a["dbf"] & a["shx"]){
      
      return(NULL)
    }else{
      
      return("Error. When uploading multiple files shp,dbf,rpj and shx files have to be included")
    }
  }
}
