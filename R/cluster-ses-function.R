


cluster_ses <- function(fit, cluster_variable){
  
  fit_vars <- unique(c(colnames(eval(fit$model)), 
                cluster_variable))
  
  cluster_data <- eval(fit$call$data)
  cluster_data <- cluster_data[,colnames(cluster_data) %in% fit_vars]
  cluster_data <- na.exclude(cluster_data)
  
  ## Calculate DF adjustment
  clust_var <- cluster_data[, colnames(cluster_data) %in% cluster_variable]
  M <- length(unique(cluster_data[, colnames(cluster_data) %in% cluster_variable]))
  N <- dim(cluster_data)[1]
  K <- fit$rank
  dfc <- (M/(M-1))*((N-1)/(N-K))
  
  #calculate the uj's
  uj  <- apply(estfun(fit), 2, function(x) tapply(x, clust_var, sum))
  
  vcovCL <- dfc * sandwich(fit, meat = crossprod(uj)/N)
  
  return(vcovCL)
  
}

