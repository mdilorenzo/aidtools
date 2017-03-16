#' A Function for Splitting AidData Projects Across Project Locations
#'
#' Get amount for equal splits of projects across locations 
#' @param proj Data frame containing projects and financial amounts
#' @param loc Data frame containing locations
#' @param amt_var Name of variable in proj containing financial amounts
#' @param ID Name of identifying variable in both data frames
#' @keywords 
#' @export
#' @examples
#' psplit()

psplit <- function(proj, loc, amt_var, ID){
  
  proj[,ID] <- as.character(proj[,ID])
  projects <- unique(proj[,ID])
  
  split <- foreach(i = projects, .combine = "rbind") %do% {
    
    n_loc <- nrow(loc[loc[,ID] == i, ])
    value <- proj[,amt_var][proj[,ID] == i]
    
    split_value <- value/n_loc
    split_value[split_value == "Inf"] <- NA
    
    c(project_id = i, split_value = split_value)
  } %>% 
    data.frame()
  
  rownames(split) <- 1:nrow(split)
  split$project_id <- as.character(split$project_id)
  split$split_value <- as.numeric(as.character(split$split_value))
  return(split)
  
}


#' A Function for Splitting AidData Projects Across Project Locations and Years
#'
#' Get amount for equal splits of projects across locations, split across years of project duration
#' @param proj Data frame containing projects and financial amounts
#' @param loc Data frame containing locations
#' @param amt_var Name of variable in proj containing financial amounts
#' @param ID Name of identifying variable in both data frames
#' @param start_var Name of variable in proj indicating start year of project
#' @param end_var Name of variable in proj indicating end year of project
#' @keywords 
#' @export
#' @examples
#' psplityear()

psplityear <- function(proj, loc, amt_var, ID, start_var, end_var){

    splityear <- foreach(i = unique(proj[,ID]), .combine = "rbind") %do% {
      
      n_loc <- nrow(loc[loc[,ID] == i, ])
      value <- proj[,amt_var][proj[,ID] == i]
      
      start <- proj[,start_var][proj[,ID] == i]
      end <- proj[,end_var][proj[,ID] == i]
      
      split_value_year <- (value/n_loc)/length(start:end)
      split_value_year[split_value_year == "Inf"] <- NA
      
      c(project_id = i, split_value_year = split_value_year)
    }
    
  return(splityear)
    
}




#' A Function for Merging World Bank IBRD-IDA Project Amounts with a Shapefile
#'
#' Get amount for equal splits of World Bank IBRD-IDA projects across locations. The returned object is the shapefile appended with a column counting dollar amounts of World Bank projects for every year.
#' @param shapefile Shapefile to append data slot with columns for financial amounts by year.
#' @param max_precision Maximum precision score of project locations to include.
#' @param amt_var Desired financial amounts: "commitments" or "disbursements".
#' @param sector Sectors to include. Default is "all" which returns all sectors.
#' @param project_status Vector containing "Implementation" and/or "Completion". Both by default.
#' @keywords 
#' @export
#' @examples
#' dollars2shape(shapefile = shape, max_precision = 2, amt_var = "commitments", sectors = c(120, 121))

dollars2shape <- function(shapefile, 
                          max_precision = 3,
                          amt_var = "commitments",
                          sectors = "all",
                          project_status = c("Implementation", "Completion")) {
  
  ## Create temporary file
  wb_dir <- tempfile()
  url <- "http://tinyurl.com/gob7w9o"
  download.file(url, wb_dir)
  
  ## Filepaths
  proj_dir <- "WorldBank_GeocodedResearchRelease_Level1_v1.4.1/data/projects.csv"
  loc_dir <- "WorldBank_GeocodedResearchRelease_Level1_v1.4.1/data/locations.csv"
  
  ## Read as .csv
  proj <- read.csv(unz(wb_dir, proj_dir), stringsAsFactors = FALSE)
  loc <- read.csv(unz(wb_dir, loc_dir), stringsAsFactors = FALSE)
  
  ## Delete temporary file
  unlink(wb_dir)
  
  ## Filter based on status
  proj <- proj %>% filter(status %in% project_status)
  
  ## Assign some shortcuts
  id <- "project_id"
  amt_var <- paste("total_", amt_var, sep = "")
  
  ## Check if specific sectors needed and filter
  if(length(sectors) > 1){
    proj <- sectorfilter(sectors = sectors, projects = proj)
    loc <- loc[loc$project_id %in% unique(proj$project_id), ]
  }
  
  ## Generate equal splits with psplit() function
  loc <- psplit(proj, loc, amt_var = amt_var, ID = as.character(id)) %>%
    right_join(loc, by = id)
  
  ## Get actual start year, merge with locations,
  ## get location year amounts, filter by precision
  locyearamt <- proj %>%
    mutate(year = as.numeric(substr(start_actual_isodate, 1, 4))) %>%
    select(project_id, year) %>%
    right_join(loc, by = id) %>%
    filter(precision_code <= max_precision) %>%
    group_by(longitude, latitude, year) %>%
    summarise(total_commitments = sum(split_value)) %>%
    ungroup()
  
  ## Make sure point locations match shape IDs
  shape_ids <- c()
  for(i in 1:nrow(shapefile@data)){
    shape_ids[i] <- slot(shapefile, "polygons")[[i]] %>% 
      slot("ID")
  }
  
  ## Create ID in shapefile
  shapefile@data$ID <- shape_ids
  
  ## Create SpatialPolygons and SpatialPoints objects
  shape_poly <- SpatialPolygons(shapefile@polygons,
                                proj4string = CRS(proj4string(shapefile)))
  
  coord_points <- SpatialPoints(coords = as.matrix(locyearamt[,c("longitude", "latitude")]),
                                proj4string = CRS(proj4string(shapefile)))
  
  ## Generate ID
  coord_points$ID <- shape_ids[over(coord_points, shape_poly)]
  
  ## Turn points into data frame
  coord_points <- coord_points %>%
    as.data.frame() %>%
    mutate(ID = as.character(ID))
  
  ## Merge with location-year-amounts, summarise by ID
  ID_year_amt <- locyearamt %>%
    left_join(coord_points, by = c("longitude", "latitude")) %>%
    group_by(ID, year) %>%
    summarise(total_commitments = sum(total_commitments)) %>%
    spread(key = year, value = total_commitments, fill = 0)
  
  ## Rename columns
  colnames(ID_year_amt) <- c("ID", paste("wb_ibrd", colnames(ID_year_amt[,-1]), sep = "_"))
  
  ## Merge with shapefile
  shapefile@data <- left_join(shapefile@data, ID_year_amt, by = "ID")
  
  ## Return shapefile
  return(shapefile)
  
}



#' A Function for Counting World Bank IBRD-IDA Projects
#'
#' Get count of new World Bank IBRD-IDA projects across locations by year. The returned object is the shapefile appended with a column counting new World Bank projects for every year.
#' @param shapefile Shapefile to append data slot with columns for financial amounts by year.
#' @param max_precision Maximum precision score of project locations to include.
#' @param sectors Sectors to include as numeric vector. Default is "all" which returns all sectors.
#' @param project_status Vector containing "Implementation" and/or "Completion". Both by default.
#' @keywords 
#' @export
#' @examples
#' projects2shape(shapefile = shape, max_precision = 2, sectors = c(120, 121))

projects2shape <- function(shapefile, 
                           max_precision = 3,
                           sectors = "all",
                           project_status = c("Implementation", "Completion")) {

  ## Create temporary file
  wb_dir <- tempfile()
  url <- "http://tinyurl.com/gob7w9o"
  download.file(url, wb_dir)
  
  ## Filepaths
  proj_dir <- "WorldBank_GeocodedResearchRelease_Level1_v1.4.1/data/projects.csv"
  loc_dir <- "WorldBank_GeocodedResearchRelease_Level1_v1.4.1/data/locations.csv"
  
  ## Read as .csv
  proj <- read.csv(unz(wb_dir, proj_dir), stringsAsFactors = FALSE)
  loc <- read.csv(unz(wb_dir, loc_dir), stringsAsFactors = FALSE)
  
  ## Delete temporary file
  unlink(wb_dir)
  
  ## Filter based on status
  proj <- proj %>% filter(status %in% project_status)
  
  ## Assign a shortcut
  id <- "project_id"
  
  ## Check if specific sectors needed and filter with sectorfilter()
  if(length(sectors) > 1){
    proj <- sectorfilter(sectors = sectors, projects = proj)
    loc <- loc[loc$project_id %in% unique(proj$project_id), ]
  }
  

    ## Get actual start year, merge with locations,
  ## get location year counts, filter by precision
  locyearcount <- proj %>%
    dplyr::mutate(year = as.numeric(substr(start_actual_isodate, 1, 4))) %>%
    dplyr::select(project_id, year) %>%
    dplyr::right_join(loc, by = id) %>%
    dplyr::filter(precision_code <= max_precision) %>%
    dplyr::select(project_id, year, longitude, latitude) %>%
    dplyr::ungroup()
  
  
  ## Make sure point locations match shape IDs
  shape_ids <- c()
  for(i in 1:nrow(shapefile@data)){
    shape_ids[i] <- slot(shapefile, "polygons")[[i]] %>% 
      slot("ID")
  }
  
  ## Create ID in shapefile
  shapefile@data$ID <- shape_ids
  
  ## Create SpatialPolygons and SpatialPoints objects
  shape_poly <- SpatialPolygons(shapefile@polygons,
                                proj4string = CRS(proj4string(shapefile)))
  
  coord_points <- SpatialPoints(coords = as.matrix(locyearcount[,c("longitude", "latitude")]),
                                proj4string = CRS(proj4string(shapefile)))
  
  ## Generate ID
  locyearcount$ID <- shape_ids[over(coord_points, shape_poly)]
  
  ## Merge with location-year-amounts, summarise by ID
  ID_year_count <- locyearcount %>%
    dplyr::group_by(ID, year) %>%
    dplyr::summarise(new_projects = length(unique(project_id))) %>%
    tidyr::spread(key = year, value = new_projects, fill = 0)
  
  ## Rename columns
  colnames(ID_year_count) <- c("ID", 
                               paste("wb_projects", colnames(ID_year_count[,-1]), sep = "_"))
  
  ## Merge with shapefile
  shapefile@data <- left_join(shapefile@data, ID_year_count, by = "ID")
  
  ## Return shapefile
  return(shapefile)
  
}



#' Filter AidData Projects by Sector Codes
#'
#' Returns subset of proj object of rows containing sector codes in the sectors argument 
#' @param sectors Sectors to include as numeric vector. 
#' @keywords projects Data object containing projects.
#' @export
#' @examples
#' sectorfilter(sectors = sector, proj = data_name)

sectorfilter <- function(sectors, projects){
  
    sectors <- as.character(sectors)
    sector_codes <- strsplit(projects$ad_sector_codes, "\\|")
    keep_project <- c()
    
    for(i in 1:nrow(projects)){
      keep_project[i] <- any(sectors %in% sector_codes[[i]])
    }
    
    projects <- projects[keep_project, ]
    
    if(nrow(projects) == 0){
      warning('No projects with matching sectors')
      return(projects)
    } else 
      
    return(projects)
    
}


#' Split AidData Project Commitments Equally Across Activities
#'
#' Splits AidData project commitment amounts evenly across activities. 
#' @param dat Name of object containing AidData core data set
#' @export
#' @examples
#' activitysplit(dat = dat)

activitysplit <- function(dat){
  
  result <- foreach(i = 1:nrow(dat), .combine = "rbind") %do% {
    activities <- unlist(strsplit(dat$aiddata_activity_codes[i], "|", fixed = T))
    if(length(activities) < 1) return(NULL)
    amount <- dat$commitment_amount_usd_constant[i]/length(activities)
    cbind(project = rep(dat$aiddata_id[i], length(activities)),
          donor = rep(dat$donor[i], length(activities)),
          recipient = rep(dat$recipient[i], length(activities)),
          year = rep(dat$year[i], length(activities)),
          activity = activities,
          amount = rep(as.numeric(amount), length(activities))) %>% data.frame()
  }
  
  result$amount <- as.numeric(as.character(result$amount))
  return(result)
  
}



#' Count Activities in an AidData Project
#'
#' Returns a count of unique activities tagged to a project.
#' @param project Row and column indices of AidData project. 
#' @export
#' @examples
#' activity_counter(project = aiddata_core$aiddata_activity_codes[1])

activity_counter <- function(project){
  n_act <- strsplit(project, "|", fixed = TRUE) %>%
    unlist() %>%
    length()
  return(n_act)
} %>%
  Vectorize()
