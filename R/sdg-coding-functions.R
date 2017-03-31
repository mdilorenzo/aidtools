#' A Function for Getting SDG Estimates for Projects with AidData Activity Codes
#'
#' Get project-level estimates of contributions to SDGs. This function requires three input variables with specific names: aiddata_id, aiddata_activity_codes, and commitment_amount_usd_constant. If coalesced_purpose == TRUE, then there should be a column called coalesced_purpose_code.
#' @param dat Data frame containing projects and financial amounts. Name of activity codes variable assumed to be "aiddata_activity_codes" and financial amount variable "commitment_amount_usd_constant".
#' @param single_activity Logical value indicating whether all the projects in the data frame have only a single activity. Can use activity_counter() to pre-filter data. By default the function can deal with cases that have single or multiple activities, but applying the function to cases with single activities takes more time than is necessary. If TRUE, run on subset of data without multiple activity codes and bind with other data later. 
#' @param coalesced_purpose Logical value indicating whether function should use alternative method of coalesced purpose code to SDG weights (p_wts). If TRUE, run on subset of data without activity codes and bind with other data later. 
#' @keywords 
#' @export
#' @examples
#' sdg_coder(dat = aiddata_core_research_release, single_activity = FALSE)

sdg_coder <- function(dat, single_activity = FALSE, coalesced_purpose = FALSE){
  
  ## Make sure dollar amounts are numeric
  dat$commitment_amount_usd_constant <- as.numeric(as.character(dat$commitment_amount_usd_constant))
  
  ## Logical check of whether to use coalesced purpose codes
  if(coalesced_purpose == TRUE){
    
    dat$coalesced_purpose_code <- as.character(dat$coalesced_purpose_code)
    
    ## Renaming variables
    combined_g_wts <- combined_g_wts %>% 
      filter(method_goal == "P") %>%
      select(-method_goal) %>%
      rename(coalesced_purpose_code = code_goal)
    
    combined_g_wts$coalesced_purpose_code <- as.character(combined_g_wts$coalesced_purpose_code)
    
    ## Merge, multiply by dollar amounts, select
    merged <- left_join(dat, combined_g_wts, by = "coalesced_purpose_code") %>%
      mutate_each(funs(.*commitment_amount_usd_constant), starts_with("goal_")) %>%
      select(aiddata_id, goal_1:goal_17)
    
    merged[is.na(merged)] <- 0
    
    return(merged)
    
  }
  
  ## Check single_activity status. If TRUE, simple merge and multiplier.
  if(single_activity == TRUE){
    
    ## Rename activity code variable to match
    ## Renaming variables
    combined_g_wts <- combined_g_wts %>% 
      filter(method_goal == "A") %>%
      select(-method_goal) %>%
      rename(aiddata_activity_codes = code_goal)
    
    ## Merge, multiply by dollar amounts, select
    merged <- left_join(dat, combined_g_wts, by = "aiddata_activity_codes") %>%
      mutate_each(funs(.*commitment_amount_usd_constant), starts_with("goal_")) %>%
      select(aiddata_id, goal_1:goal_17)
    
    merged[is.na(merged)] <- 0
    
    return(merged)
    
  }
  
  ## If single_activity == FALSE, split projects up by activities
  if(single_activity == FALSE){
    
    
    combined_g_wts <- combined_g_wts %>% 
      filter(method_goal == "A") %>%
      select(-method_goal) %>%
      rename(aiddata_activity_codes = code_goal)
    
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
      links <- combined_g_wts %>%
        filter(aiddata_activity_codes %in% act) %>%
        select(-aiddata_activity_codes)
      
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








#' A Function for Getting Target-level SDG Estimates for Projects with AidData Activity Codes
#'
#' Get project-level estimates of contributions to SDG targets. This function requires three input variables with specific names: aiddata_id, aiddata_activity_codes, and commitment_amount_usd_constant. If coalesced_purpose == TRUE, then there should be a column called coalesced_purpose_code.
#' @param dat Data frame containing projects and financial amounts. Name of activity codes variable assumed to be "aiddata_activity_codes" and financial amount variable "commitment_amount_usd_constant".
#' @param single_activity Logical value indicating whether all the projects in the data frame have only a single activity. Can use activity_counter() to pre-filter data. By default the function can deal with cases that have single or multiple activities, but applying the function to cases with single activities takes more time than is necessary. If TRUE, run on subset of data without multiple activity codes and bind with other data later. 
#' @param coalesced_purpose Logical value indicating whether function should use alternative method of coalesced purpose code to SDG weights (p_wts). If TRUE, run on subset of data without activity codes and bind with other data later. 
#' @keywords 
#' @export 
#' @examples
#' sdg_target_coder(dat = aiddata_core_research_release)


target_coder <- function(dat, 
                         single_activity = FALSE, 
                         coalesced_purpose = FALSE){
  
  ## Make sure dollar amounts are numeric
  dat$commitment_amount_usd_constant <- as.numeric(as.character(dat$commitment_amount_usd_constant))
  
  ## Logical check of whether to use coalesced purpose codes
  if(coalesced_purpose == TRUE){
    
    combined_t_wts <- combined_t_wts %>% 
      filter(method_target == "P") %>%
      select(-method_target) %>%
      rename(coalesced_purpose_code = code_target)

    dat$coalesced_purpose_code <- as.character(dat$coalesced_purpose_code)
    combined_t_wts$coalesced_purpose_code <- as.character(combined_t_wts$coalesced_purpose_code)
    
    ## Merge, multiply by dollar amounts, select
    merged <- left_join(dat, combined_t_wts, by = "coalesced_purpose_code") %>%
      mutate_each(funs(.*commitment_amount_usd_constant), starts_with("t_")) %>%
      select(aiddata_id, t_1.1:t_17.19)
    
    merged[is.na(merged)] <- 0
    
    return(merged)
    
  }
  
  ## Check single_activity status. If TRUE, simple merge and multiplier.
  if(single_activity == TRUE){
    
    combined_t_wts <- combined_t_wts %>% 
      filter(method_target == "A") %>%
      select(-method_target) %>%
      rename(aiddata_activity_codes = code_target)
    
    ## Merge, multiply by dollar amounts, select
    merged <- left_join(dat, combined_t_wts, by = "aiddata_activity_codes") %>%
      mutate_each(funs(.*commitment_amount_usd_constant), starts_with("t_")) %>%
      select(aiddata_id, t_1.1:t_17.19)
    
    merged[is.na(merged)] <- 0
    
    return(merged)
    
  }
  
  ## If single_activity == FALSE, split projects up by activities
  if(single_activity == FALSE){
    
    combined_t_wts <- combined_t_wts %>% 
      filter(method_target == "A") %>%
      select(-method_target) %>%
      rename(aiddata_activity_codes = code_target)
    
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
      links <- combined_t_wts %>%
        filter(aiddata_activity_codes %in% act) %>%
        select(-aiddata_activity_codes)
      
      ## If no matches, create vector of zeroes of length 17
      if(nrow(links) < 1){
        
        links <- matrix(rep(0, 169), nrow = 1, ncol = 169)
        colnames(links) <- colnames(combined_t_wts[,-1])
        
      }
      
      ## Generate vector of SDG estimates
      sdg_estimates <- colSums(links * amt)
      
      ## Concatenate with project_id
      c(aiddata_id = dat$aiddata_id[i], sdg_estimates)
      
    }
    
    return(result)
    
  }
  
}





