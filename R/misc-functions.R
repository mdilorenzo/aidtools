#' A Data Binding Function
#'
#' This function turns cross-section units into cross-sectional, time-series units. 
#' @param df Data frame object to replicate.
#' @param begin Numeric starting value
#' @param end Numeric ending value
#' @keywords 
#' @export
#' @examples
#' tsbind(df = data_frame, begin = 2000, end = 2010)

tsbind <- function(df, begin, end){
  
  ts <- df[rep(seq_len(nrow(df)), length(begin:end)), ]
  ts$year <- rep(begin:end, each = nrow(df))
  
  return(ts)
  
}

#' A Function for Calculating Polygon Overlap
#'
#' Calculate percentage of overlap between two polygons contained in SpatialPolygons objects relative to first polygon.
#' @param p1 First SpatialPolygons object
#' @param p2 Second SpatialPolygons object
#' @param i1 Index of polygon in first SpatialPolygons object
#' @param i2 Index of polygon in second SpatialPolygons object
#' @keywords 
#' @export
#' @examples
#' propover()

propover <- function(p1, p2, i1, i2) {
  
  if(!proj4string(p1)==proj4string(p2)) 
    warning("proj4strings do not match.")
  
  overlap <- suppressWarnings(
    raster::intersect(p1[i1], p2[i2])
  )
  
  if(class(overlap) == "NULL") 
    return(0)
  
  intersection <- overlap@polygons[[1]]@Polygons[[1]]@coords
  original <- p1@polygons[[i1]]@Polygons[[1]]@coords
  
  value <- areapl(intersection)/areapl(original)
  value <- ifelse(value > 1, 1, value)
  
  return(value)
  
}



