## Basic custom R functions to load


## Plus-minus function

plusmin <- function(value, by){
  
  return(c(lower = value - by,
           upper = value + by))
  
}


## Bootstrap
bootstrap <- function(model, B) {
  
  store <- NULL
  
  dat <- as.data.frame(model$model)
  
  for(i in 1:B){
    draw <- dat[sample(1:nrow(dat), nrow(dat), replace = T), ]
    fit <- update(model, data = draw)
    store <- rbind(store, coef(fit))
  }
  
  store <- data.frame(store)
  
  out <- data.frame(
    cbind(variable = colnames(store),
          beta = apply(store, 2, mean),
          se = apply(store, 2, sd))
  )
  
  row.names(out) <- 1:nrow(out)
  
  out$beta <- factor2numeric(out$beta)
  out$se <- factor2numeric(out$se)
  
  return(out)
  
}






## Calculate significance of interaction term
mcce_ci_calc <- function(var1, var2, var2.value, 
                         model,
                         model_vcov = vcov(model)){
  
  b1_var <- model_vcov[var1, var1]
  
  int_var <- names(coef(model))[
    grepl(var1, names(coef(model))) & grepl(var2, names(coef(model)))
    ]
  
  b3_var <- model_vcov[int_var, int_var]
  b1b3_covar <- model_vcov[var1, int_var] 
  
  se <- sqrt(b1_var + (var2.value^2)*b3_var + 2*var2.value*b1b3_covar)
  MCCE <- summary(model)$coefficients[var1, 1] + 
    var2.value*summary(model)$coefficients[int_var, 1]
  
  return(c(var2.value = var2.value, 
           beta = MCCE, 
           std.error = se, 
           ci.95.lower = MCCE - 1.96 * se, 
           ci.95.upper = MCCE + 1.96 * se,
           ci.90.lower = MCCE - 1.645 * se, 
           ci.90.upper = MCCE + 1.645 * se))
  
}


# This function calculates confidence intervals for an interaction effect using a cluster-adjusted variance-covariance matrix.
ci_cluster <- function(var1, var2, var2.value, model, vcov.cluster){
  
  b1_var <- vcov.cluster[var1, var1]
  int_var <- paste(var1, var2, sep=":")
  b3_var <- vcov.cluster[int_var, int_var]
  b1b3_covar <- vcov.cluster[var1, int_var] 
  
  se <- sqrt(b1_var + (var2.value^2)*b3_var + 2*var2.value*b1b3_covar)
  MCCE <- summary(model)$coefficients[var1, 1] + 
    var2.value*summary(model)$coefficients[int_var, 1]
  
  return(c(var2.value = var2.value, 
           beta = MCCE, 
           std.error = se, 
           ci.95.lower = MCCE-1.96*se, 
           ci.95.upper = MCCE+1.96*se,
           ci.90.lower = MCCE-1.645*se, 
           ci.90.upper = MCCE+1.645*se,
           ci.80.lower = MCCE-1.282*se, 
           ci.80.upper = MCCE+1.282*se))
}



## Clustered SEs

cluster_ses <- function(fit, cluster_variable){
  
  fit_vars <- unique(c(colnames(eval(fit$model)), 
                       cluster_variable))
  
  cluster_data <- eval(fit$call$data)
  cluster_data <- cluster_data[ , colnames(cluster_data) %in% fit_vars]
  cluster_data <- na.exclude(cluster_data)
  
  ## Calculate DF adjustment
  clust_var <- cluster_data[, colnames(cluster_data) %in% cluster_variable]
  M <- length(unique(
    cluster_data[, colnames(cluster_data) %in% cluster_variable])
  )
  N <- dim(cluster_data)[1]
  K <- fit$rank
  dfc <- (M/(M-1))*((N-1)/(N-K))
  
  #calculate the uj's
  uj  <- apply(estfun(fit), 2, function(x) tapply(x, as.character(clust_var), sum))
  
  vcovCL <- dfc * sandwich(fit, meat = crossprod(uj)/N)
  
  return(vcovCL)
  
}



## Survey summarize

survey_summarize <- function(x) {
  
  x <- x
  n_resp <- x %>%
    .[!. == ""] %>%
    length()
  
  all_responses <- x %>%
    .[!. == ""] %>%
    strsplit(split = ",", fixed = FALSE) %>%
    unlist() %>%
    as.character()
  
  u_resp <- sort(unique(all_responses))
  
  freq_responses <- sapply(u_resp, 
                           function(z) 
                             length(all_responses[all_responses == z])
  ) %>% c()
  
  output <- cbind(item_no = u_resp, 
                  frequency = freq_responses) %>%
    data.frame() %>%
    mutate(item_no = as.numeric(as.character(item_no)),
           frequency = as.numeric(as.character(frequency))) %>%
    mutate(proportion = round(frequency / n_resp * 100, 2),
           n_responses = n_resp) %>%
    arrange(desc(proportion)) %>%
    mutate(n_with_pct = paste0(frequency,
                               " (",
                               proportion,
                               "%)"))
  
  return(output)
  
}



## Untangle
untangle <- function(x) {
  
  x %>%
    unlist() %>%
    c() %>%
    .[!is.na(.)] %>%
    as.character() %>%
    strsplit(split = ",") %>%
    unlist() %>%
    trimws() %>%
    unique()
  
}


untangle_all <- function(x) {
  
  x %>%
    unlist() %>%
    c() %>%
    .[!is.na(.)] %>%
    as.character() %>%
    strsplit(split = ",") %>%
    unlist() %>%
    trimws()
  
}


theme_aiddata <- function (base_size = 12, base_family = "") { 
  theme(panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "#F3F4F4"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "#F3F4F4"),
        text = element_text(family = "Avenir LT 65 Medium", 
                            size = 11),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank())
}


theme_aiddata_plain <- function (base_size = 12, base_family = "") { 
  theme(panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "#F3F4F4"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "#F3F4F4"),
        text = element_text(family = "Arial", 
                            size = 11),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank())
}


theme_gates <- function (base_size = NA, base_family = "") { 
  theme(panel.background = element_rect(fill = "#EFEDEE",
                                        colour = "#EFEDEE",
                                        size = 0, linetype = "solid"),
        panel.grid.major.x = element_line(size = 0.25, 
                                          linetype = 'solid',
                                          colour = "#AAA092"), 
        panel.grid.minor.x = element_blank(),
        #panel.grid.minor.y = element_line(size = 0, linetype = 'solid',
        #                                  colour = "#AAA092"), 
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_line(size = 0, linetype = 'solid',
                                          colour = "#AAA092"),
        text = element_text(family = "Arial", 
                            size = 10,
                            colour = "black"),
        panel.spacing = unit(c(0, 0, 0, 0), "cm"),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(colour = "black"),
        axis.text.x = element_text(colour = "black"),
        axis.ticks.x = element_blank(),
        axis.title.x = element_text(size = 7),
        axis.line.y = element_line(colour = "black", size = .25),
        axis.line.x = element_blank())
}


theme_matt <- function (base_size = 12, base_family = "") { 
  theme(panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "#F3F4F4"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "#F3F4F4"),
        text = element_text(family = "Garamond Bold", 
                            size = 11),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank())
}






theme_brookings <- function (base_size = 12, base_family = "") { 
  theme(panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.25, linetype = "solid"),
        panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                        colour = "#D8DCDB"), 
        panel.grid.major.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        text = element_text(family = "Montserrat Medium", size = 9)) 
}




# Background
c(239, 237, 238)/255
rgb(0.9372549, 0.9294118, 0.9333333)

# Grid
c(170, 160, 146)/255
rgb(0.6666667, 0.6274510, 0.5725490)

## Four of Gates' colors
gates_colors <- c("#977C00",
                  "#CE6B29",
                  "#8CB7C7",
                  "#9B242D")


## bracketize: Put number of responses in brackets next to item name
bracketize <- function(name, number) {
  paste0(name, 
         " [",
         number,
         "]")
}

## percented: Put percent next to N
percented <- function(number) {
  paste0(number, "%")
}



generic_document <- function(filename, note_text) {
  
  fileConn <- file(filename)
  writeLines(note_text, fileConn)
  close(fileConn)
  
}



tidy_date <- function(){
  
  paste0(substr(Sys.Date(), 6, 7),
         substr(Sys.Date(), 9, 10),
         substr(Sys.Date(), 1, 4))
  
}

tidy_date()

untangle <- function(x, char = ",") {
  
  x %>%
    unlist() %>%
    c() %>%
    .[!is.na(.)] %>%
    as.character() %>%
    strsplit(split = char, fixed = TRUE) %>%
    unlist() %>%
    trimws() %>%
    unique()
  
}



## Function for recoding to dummy variables
dummify <- function(x, zero_cat, one_cat){
  
  x <- ifelse(x %in% zero_cat,
              0,
              ifelse(x %in% one_cat,
                     1, 
                     NA))
  return(x)
}

## Function for getting mean with n obs in brackets
mean_with_n <- function(x) {
  
  return(paste0(mean(x, na.rm = T), " [", length(x[!is.na(x)]), "]"))
  
}


## Function for getting the first word in a character string
first_word <- function(x, separator = " "){
  
  sapply(1:length(x),
         function(i)
           strsplit(x[i], separator) %>%
           c() %>%
           unlist() %>%
           .[1])
  
} 


## Shortcut to excluding regions from WDI search

wdi_region_terms <- function(){
  
  "Arab World|World|income|develop|IBRD|islands|world|South Asia|fragile|small states|East Asia|demograph|poor|indebt|Middle|OECD|classified|sub-sah|North America|South America|Latin America|&|European Union|area|Baltics|IDA blend|IDA only|IDA total"
  
}



## Even / odd indices functions

is.even <- function(x){
  x %% 2 == 0
}


is.odd <- function(x){
  x %% 2 != 0
}


## Convert factor to numeric
factor2numeric <- function(x){
  
  as.numeric(as.character(x))
  
}



## See unique values for a whole data set by variable
see_values <- function(x) {
  
  apply(x, 2, print(unique))
  
}


## See unique values with % breakdown
breakdown <- function(x) {
  
  x <- as.character(x)
  x[is.na(x)] <- "NA"
  
  values <- unique(as.character(x))
  
  pct <- sapply(1:length(values),
                function(i)
                  length(x[x == values[i]]) / length(x) * 100)
  
  paste0(values, " (", round(pct, 2), "%)") %>% trimws()
  
}

## Combine see_values with breakdown
see_values_pct <- function(x) {
  
  apply(x, 2, breakdown)
  
}





## East Asian ISO-3 codes for State Department

east_asia_iso3 <- function(){
  
  strsplit(paste0("ASM|AUS|BRN|KHM|COK|TLS|FSM|FJI|PYF|GUM|",
                  "IDN|JPN|KIR|LAO|MYS|MHL|MNG|MMR|NRU|NCL|",
                  "NZL|NIU|PRK|MNP|PLW|PNG|PHL|PCN|WSM|SGP|",
                  "SLB|KOR|TWN|THA|TON|TUV|VUT|VNM"),
           split = "|", fixed = TRUE) %>%
    unlist() %>%
    as.character()
  
}



## Generate covariate labels for stargazer

star_labels <- function(model, covariate_list) {
  
  included_vars <- summary(model)$coefficients[ , 1] %>% 
    names() %>%
    .[!. == "(Intercept)"]
  
  return(covariate_list[included_vars])
  
}



## Count in intervals

count_by <- function(start = 1, count_by = 2, length_out = 5){
  
  numbers <- c(start)
  
  for(i in 2:length_out){
    
    numbers[i] <- numbers[i - 1] + count_by
    
  }
  
  return(numbers)
  
}


## Get quick interpretation text for regression models
lm_interpret <- function(fit, tex_ready = FALSE){
  
  require(broom)
  
  tidy_fit <- tidy(fit) %>%
    filter(!term == "(Intercept)") %>%
    mutate(direction = ifelse(estimate > 0, 
                              "positive", 
                              "negative")) %>%
    mutate(sig = ifelse(
      p.value < 0.05,
      " and is statistically distinguishable from zero ",
      " but is not statistically distinguishable from zero ")) %>%
    mutate(interpret = paste0("The estimated coefficient on ",
                              term,
                              " is ",
                              direction,
                              sig,
                              " (p = ",
                              round(p.value, 3),
                              ")."))
  
  summary_text <- paste(tidy_fit$interpret, collapse = " ")
  
  if(tex_ready){
    summary_text <- cat(gsub("_", "\\\\_", summary_text))
  }
  
  return(summary_text)
  
  
}




## Connect list of items together in character string

and_or_items <- function(x, connector = "and"){
  
  if(length(x) == 1){
    
    x
    
  } else {
    
    paste(
      paste0(x[1:(length(x) - 1)], collapse = ", "),
      connector,
      x[length(x)])
    
  }
  
}


multi_lm_interpret <- function(model_list,
                               var_list,
                               tex_ready = TRUE){
  
  require(broom)
  require(foreach)
  
  ## Clean up regression output for all models in list
  all_fits <- foreach(i = 1:length(model_list), .combine = "rbind") %do% {
    
    
    tidy(model_list[[i]]) %>%
      filter(!term == "(Intercept)") %>%
      mutate(direction = ifelse(estimate > 0, 
                                "positive", 
                                "negative")) %>%
      mutate(sig = ifelse(p.value < 0.05, 1, 0)) %>%
      mutate(model = i)
    
    
  }
  
  ## Keep only relevant variables
  all_fits <- all_fits %>%
    filter(term %in% var_list)
  
  all_fits <- all_fits %>%
    group_by(term) %>%
    summarise(
      direction = ifelse(length(unique(direction)) > 1,
                         "changes sign across models",
                         paste0("is consistently ",
                                unique(direction))),
      significance = ifelse(
        max(sig) == 0,
        " and is never statistically distinguishable from zero.",
        ifelse(min(sig) == 1,
               " and is statistically significant across all models.",
               paste0(" and is ", 
                      ifelse(length(unique(model[sig == 1])) == 1,
                             "only",
                             ""),
                      " statistically significant in Model",
                      ifelse(length(unique(model[sig == 1])) == 1,
                             " ",
                             "s "),
                      and_or_items(unique(model[sig == 1])),
                      "."
               )
        )
      )
    ) %>%
    mutate(interpret = paste0("The estimated coefficient on ",
                              term,
                              " ",
                              direction,
                              significance))
  
  
  summary_text <- paste(all_fits$interpret, collapse = " ")
  
  if(tex_ready){
    summary_text <- cat(gsub("_", "\\\\_", summary_text))
  }
  
  return(summary_text)
  
}



## Score calculator for Let's Go Fishin' CPR simulation
fishing <- function(r = 1, shock = 0) {
  
  stock <- 0:21
  
  dat <- cbind(fish_remaining = stock,
               start_with = as.character(
                 round(stock * (1 + r) + .01, 0) - shock
               )) %>%
    data.frame()
  
  dat <- dat %>%
    mutate_all(funs(as.character(.)))
  
  dat$start_with[as.numeric(dat$start_with) > 21] <- "21"
  dat$start_with[as.numeric(dat$start_with) < 1] <- "Game over!"
  
  print(dat)
  
}



## Marginal effects, standard errors, and 95 CI for quadratic terms

mcce_quad <- function(term, value, model, var_mat = vcov(model)){
  
  ## Make term squared
  term_squared <- paste0("I(", term, "^2)")
  
  ## Get MCCE
  beta_term <- coef(model)[term]
  beta_term_sq <- coef(model)[term_squared]
  
  mcce <- as.numeric(beta_term + 2 * beta_term_sq * value)
  
  var_term <- var_mat[term, term]
  var_term_sq <- var_mat[term_squared, term_squared]
  covar_term <- var_mat[term, term_squared]
  
  var_mcce <- var_term +
    4 * value^2 * var_term_sq +
    4 * value * covar_term
  
  se_mcce = sqrt(var_mcce)

  return(c(coef = mcce,
           se = se_mcce,
           lower = mcce - 2 * se_mcce,
           upper = mcce + 2 * se_mcce))
  
  
}




## Check if all letters are upper case in character string

all_upper <- function(x){
  
  sapply(
    1:length(x),
    function(i) 
      all(unlist(strsplit(x[i], "")) %in% LETTERS)
  )
  
}


## Get acronyms out of parentheses (or any text)

multilacronym <- function(x, chars = c("(", ")")){
  
  reg_ex <- paste0("\\", chars[1], ".*?\\", chars[2])
  gsub_ex <- paste0("\\", chars[1], "|\\", chars[2])
  
  sapply(1:length(x),
         function(i)
           gsub(gsub_ex, "",
                regmatches(x[i], 
                           gregexpr(reg_ex, x[i]))[[1]]))
  
} 



## Negative infinity / NaN fixer function

neg_nan_fix <- function(x){
  
  ifelse(x == -Inf | is.nan(x), NA, x)
  
}



## Save a plot in multiple formats and data for plot object

pcomms_save <- function(p, fig_name, ...){
  
  ## Save plot data
  write.csv(as.data.frame(ggplot_build(p)$plot$data),
            paste0(fig_name, "-data.csv"),
            row.names = FALSE)
  
  ## Duplicate version for non .jpeg / .png
  
  as_is <- c(".jpeg", ".png")
  
  for(i in 1:length(as_is)){
    
    ggsave(p, file = paste0(fig_name, as_is[i]), ...)
    
  }
  
  ## Replace text as Arial for non .jpeg/.png formats
  
  p_non <- p
  
  ## Get non .jpeg file types
  non_jpeg <- c(".eps")
  
  for(i in 1:length(p_non$layers)){
    
    p_non$layers[[i]]$aes_params$family <- "Arial"
    
  }
  
  ## Save image as various file types
  for(i in 1:length(non_jpeg)){
    
    ggsave(p_non, file = paste0(fig_name, non_jpeg[i]), ...)
    
  }
  
  ## Create file names for zipping
  file_names <- c(paste0(fig_name, c(".jpeg", ".png", ".eps")),
                  paste0(fig_name, "-data.csv"))
  
  ## Zip
  zip(zipfile = paste0(fig_name, ".zip"),
      files = file_names)
  
}


