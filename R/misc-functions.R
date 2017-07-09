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


#' A Function for Matching Text with Exclusions
#'
#'  text_matcher() returns logical vector indicating whether each element in x matches terms included in include_text. If exclude_text (character vector of words) is nonempty, text_matcher() first filters out any of the terms included in exclude_text before matching the include_text
#' @param x Character vector containing text to search.
#' @param include_text Character vector containing terms to find.
#' @param exclude_text Character vector containing terms to exclude. Defaults to NULL. If exclude_text (character vector of words) is nonempty, text_matcher() first filters out any of the terms included in exclude_text before matching the include_text
#' @param ignore_case logical. Sets value of ignore.case in gsub and grepl.
#' @keywords 
#' @export
#' @examples
#' textmatcher(x = c("hello", "not hello"), include_text = "hello", exclude_text = "not hello", ignore_case = TRUE)

text_matcher <- function(x, 
                         include_text, 
                         exclude_text = NULL,
                         ignore_case = TRUE) {
  
  description <- x
  include_text <- paste(include_text, collapse = "|")
  exclude_text <- paste(exclude_text, collapse = "|")
  
  if(nchar(exclude_text) > 0) {
    
    description <- gsub(exclude_text,
                        "",
                        description,
                        ignore.case = ignore_case)
  }
  
  include_match <- grepl(include_text,
                         description,
                         ignore.case = ignore_case)
  
  include_match
  
}
