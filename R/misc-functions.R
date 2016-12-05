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




# 
# poly2grid <- function(poly, N){
#   ## Based on http://r-sig-geo.2731867.n2.nabble.com/Split-polygon-by-line-td7588625.html
#   grid_lines <- gridlines(poly, 
#                           easts = pretty(bbox(poly)[1, ], n = N),
#                           norths = pretty(bbox(poly)[2, ], n = N))
#   poly_int <- gIntersection(poly, grid_lines) # intersect lines with polygon
#   poly_int <- spTransform(poly_int, CRS(proj4string(poly)))
#   buff_int <- gBuffer(poly_int, width = 0.0000001) # buffer intersected lines 
#   gridded_poly <- gDifference(poly, buff_int)    
# }

