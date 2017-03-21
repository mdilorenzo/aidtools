#' A Function for Getting SDG Estimates for Projects with AidData Activity Codes
#'
#' Get project-level estimates of contributions to SDGs.
#' @param dat Data frame containing projects and financial amounts. Name of activity codes variable assumed to be "aiddata_activity_codes" and financial amount variable "commitment_amount_usd_constant".
#' @param single_activity Logical value indicating whether all the projects in the data frame have only a single activity. Can use activity_counter() to pre-filter data. By default the function can deal with cases that have single or multiple activities, but applying the function to cases with single activities takes more time than is necessary. 
#' @keywords 
#' @export
#' @examples
#' sdg_coder(dat = aiddata_core_research_release, single_activity = FALSE)

sdg_coder <- function(dat, single_activity = FALSE, distr_all = FALSE, multi_count = FALSE){
  
  ## Logical check of whether to distribute entire value of project
  ## using Jennifer method
  if(distr_all == TRUE){
    
    ## Merge, multiply by dollar amounts, select
    merged <- left_join(dat, j_wts, by = "aiddata_activity_codes") %>%
      mutate_each(funs(.*commitment_amount_usd_constant), starts_with("goal_")) %>%
      select(aiddata_id, goal_1:goal_17)
    
    merged[is.na(merged)] <- 0
    
    return(merged)
    
  }
  
  ## Logical check of whether to double count rather than splitting
  if(multi_count == TRUE){
    
    wts[,-1] <- apply(wts[,-1], 2, function(x) ifelse(x > 0, 1, 0))

  }
  
  ## Check single_activity status. If TRUE, simple merge and multiplier.
  if(single_activity == TRUE){
    
    ## Rename activity code variable to match
    wts <- wts %>%
      rename(aiddata_activity_codes = activity_code)
    
    ## Merge, multiply by dollar amounts, select
    merged <- left_join(dat, wts, by = "aiddata_activity_codes") %>%
      mutate_each(funs(.*commitment_amount_usd_constant), starts_with("goal_")) %>%
      select(aiddata_id, goal_1:goal_17)
    
    merged[is.na(merged)] <- 0
    
    return(merged)
    
  }
  
  ## If single_activity == FALSE, split projects up by activities
  if(single_activity == FALSE){
    
    ## Loop through rows
    result <- foreach(i = 1:nrow(dat), .combine = "rbind") %do% {
      
      ## Set amount to zero to start
      amt <- 0
      
      ## Get vector of activity codes
      act <- unlist(c(strsplit(dat$aiddata_activity_codes[i],
                               "|",
                               fixed = TRUE))) %>% unique()
      
      ## Split project amounts into number of unique activities
      if(length(act) > 0){
        
        amt <- dat$commitment_amount_usd_constant[i] / length(act)
        
      }
      
      ## Create a matrix of the activity-to-goal weights for relevant activities
      links <- wts %>%
        filter(activity_code %in% act) %>%
        select(-activity_code)
      
      ## If no matches, create vector of zeroes of length 17
      if(nrow(links) < 1){
        
        links <- matrix(rep(0, 17), nrow = 1, ncol = 17)
        colnames(links) <- paste("goal", 1:17, sep = "_")
        
      }
      
      ## Generate vector of SDG estimates
      sdg_estimates <- colSums(links * amt)
      
      ## Concatenate with project_id
      c(aiddata_id = dat$aiddata_id[i], sdg_estimates)
      
    }
    
    return(result)
    
  }
  
}

