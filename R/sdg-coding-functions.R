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

sdg_coder <- function(dat, 
                      single_activity = FALSE, 
                      coalesced_purpose = FALSE){
  
  ## Make sure dollar amounts are numeric
  dat$commitment_amount_usd_constant <- as.numeric(
    as.character(dat$commitment_amount_usd_constant)
    )
  
  ## Logical check of whether to use coalesced purpose codes
  if(coalesced_purpose == TRUE){
    
    dat$coalesced_purpose_code <- as.character(dat$coalesced_purpose_code)
    
    ## Renaming variables
    combined_g_wts <- combined_g_wts %>% 
      filter(method_goal == "P") %>%
      select(-method_goal) %>%
      rename(coalesced_purpose_code = code_goal)
    
    combined_g_wts$coalesced_purpose_code <- as.character(
      combined_g_wts$coalesced_purpose_code
      )
    
    ## Merge, multiply by dollar amounts, select
    merged <- left_join(dat, combined_g_wts, 
                        by = "coalesced_purpose_code") %>%
      #mutate_each(funs(.*commitment_amount_usd_constant), 
      #            starts_with("goal_")) %>%
      mutate_at(vars(starts_with("goal_")),
                funs(.*commitment_amount_usd_constant)) %>%
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
    merged <- left_join(dat, combined_g_wts, 
                        by = "aiddata_activity_codes") %>%
      # mutate_each(funs(.*commitment_amount_usd_constant), 
      #             starts_with("goal_")) %>%
      mutate_at(vars(starts_with("goal_")),
                funs(.*commitment_amount_usd_constant)) %>%
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
      
      ## Create matrix of activity-to-goal weights for relevant activities
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
  dat$commitment_amount_usd_constant <- as.numeric(
    as.character(dat$commitment_amount_usd_constant)
    )
  
  ## Logical check of whether to use coalesced purpose codes
  if(coalesced_purpose == TRUE){
    
    combined_t_wts <- combined_t_wts %>% 
      filter(method_target == "P") %>%
      select(-method_target) %>%
      rename(coalesced_purpose_code = code_target)

    dat$coalesced_purpose_code <- as.character(dat$coalesced_purpose_code)
    combined_t_wts$coalesced_purpose_code <- as.character(
      combined_t_wts$coalesced_purpose_code
      )
    
    ## Merge, multiply by dollar amounts, select
    merged <- left_join(dat, combined_t_wts, 
                        by = "coalesced_purpose_code") %>%
      # mutate_each(funs(.*commitment_amount_usd_constant), 
      #             starts_with("t_")) %>%
      mutate_at(vars(starts_with("t_")),
                funs(.*commitment_amount_usd_constant)) %>%
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
    merged <- left_join(dat, combined_t_wts, 
                        by = "aiddata_activity_codes") %>%
      # mutate_each(funs(.*commitment_amount_usd_constant), 
      #             starts_with("t_")) %>%
      mutate_at(vars(starts_with("t_")),
                funs(.*commitment_amount_usd_constant)) %>%
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
      
      ## Create matrix of activity-to-goal weights for relevant activities
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




































#' A Function for Getting SDG Estimates using Direct Manual Coding of SDGs
#'
#' Get project-level estimates of contributions to SDG goals targets using the direct coding method. 
#' @param codes Column / row entry containing SDG codes. 
#' @param project_value Column / row entry containing SDG codes
#' @param target_level Logical. If TRUE, provides target-level estimates. Defaults to FALSE.
#' @keywords 
#' @export 
#' @examples
#' sdg_direct_single(codes = data$sdg_codes[1], project_value = data$project_value[1])

sdg_direct_single <- function(codes, 
                              project_value,
                              target_level = FALSE) {
  
  ## Replace missing values as zero
  project_value <- ifelse(is.na(project_value), 0, project_value)
  
  ## Return zeroes for Do Not Code cases
  if(codes == "DNC"){
    
    return(
      ifelse(target_level == TRUE,
             return(c(rep(NA, 187))),
             return(c(rep(NA, 18))))
    )
    
  } 
  
  else 
    
  {
    
    ## 'Untangle' codes
    sdg_codes <- untangle(codes, char = "|")
    
    ## Split value of project evenly
    split_value <- project_value / length(sdg_codes)
    
    ## Assign to column in sdgs data frame
    sdgs$value <- ifelse(sdgs$sdg_code %in% sdg_codes, 1, 0) * split_value
    
    ## Collect target-level estimates
    target_estimates <- sdgs$value
    
    ## Collect goal-level estimates
    goal_estimates <- sdgs %>%
      group_by(goals) %>%
      summarise(value = sum(value, na.rm = T)) %>%
      select(value) %>%
      unlist() %>%
      c()
    
    ## Name columns
    names(target_estimates) <- sdgs$sdg_code
    names(goal_estimates) <- c(paste0("sdg_", 1:17), "ENV")
    
    ## Decide what gets returned
    ifelse(target_level,
           return(target_estimates),
           return(goal_estimates)
    )
  }
  
}






#' A Function for Getting SDG Estimates using Direct Manual Coding of SDGs
#'
#' This function uses the sdg_direct_single function in a loop for application to a full data set.
#' @param sdg_data Data frame object.
#' @param code_column Quoted name of column containing SDG codes.
#' @param value_column Quoted name of column containing financial values.
#' @param target Logical. If TRUE, provides target-level estimates. Defaults to FALSE.
#' @keywords 
#' @export 
#' @examples
#' sdg_direct()

sdg_direct <- function(sdg_data, 
                       code_column,
                       value_column,
                       target = FALSE) {
  
  placeholder <- matrix(NA,
                        nrow = nrow(sdg_data),
                        ncol = ifelse(target, 187, 18))
  
  for(i in 1:nrow(placeholder)) {
    
    placeholder[i, ] <- as.numeric(
      c(sdg_direct_single(sdg_data[[code_column]][i], 
                          sdg_data[[value_column]][i], 
                          target_level = target)))
    
  }
  

  ## Create placeholder matrix
  placeholder <- as.data.frame(placeholder)
  
  ## Replace column names depending on target- vs. goal-level
  if(target){
    
    colnames(placeholder) <- c(paste0("t_",
                                      sdgs$sdg_code[-length(sdgs$sdg_code)]),
                               "ENV")
    
  } else {
    
    colnames(placeholder) <- c(paste0("sdg_", 1:17), "ENV")
    
  }
  
  ## Bind with base data
  placeholder <- cbind(sdg_data,
                       placeholder)
  
  return(placeholder)
  
}




