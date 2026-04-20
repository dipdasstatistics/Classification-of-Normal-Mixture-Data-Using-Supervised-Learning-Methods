###############################################################################
###### ********Load library mvtnorm for this function*********###########


data_generation <- function(n_obs, n_group, n_var, mean_unif_limits, sigma_type){
  
  # data_generation function has been created to generate data from a multivariate
  # normal distribution. 
  
  
  # n_obs is number of observations that we wants to generate.
  
  if (n_obs <= 3){
    stop("Number of observations that you want to generate should
                      be greater than 3")
  }
  
  
  
  # n_group is number of groups that we want to have in our dataset.
  if (n_obs %% n_group > 0)
  {
    stop("Number of observations must be a multiple of number of groups")
  }
  
  
  # n_var determines the number of variables of the multivariate data.
  if (n_var <2 || n_var >15)
  {
    stop("number of variables in the multivariate data must be in between 2 to 15")
  }
  
  
  
  # Checking whether mean_unif_limits has proper length
  if (length(mean_unif_limits) !=2)
  {
    stop("mean_unif_limits must be a vector of length 2")
  }
  
  
  # sigma indicates variance covariance matrix which will be used to generate 
  # the data from multivariate normal
  if (sigma_type != "identity" && sigma_type != "toeplitz" && sigma_type != "equicorrelation") 
  {
    stop("sigma must be either identity or toeplitz or equicorrelation matrix")
  }

  
  # Creating null matrix to store the generated data
  dat <- matrix(NA, nrow = n_obs, ncol = (n_var+1), byrow = T)
  
  # Number of observations that every groups will have
  obs_per_group <- n_obs/n_group
  
  
  
  # Preparing the sigma matrix for given inputted sigma_type variable.
  if (sigma_type == "identity")
  {
     sigma_mat <- diag(n_var)
  }
  if (sigma_type == "toeplitz")
  {
    sigma_mat <- Toeplitz_matrix(r = 0.5, nrow = n_var, ncol = n_var)
  }  
  if (sigma_type == "equicorrelation") 
  {
    sigma_mat <- equicorr_matrix(r = 0.2, nrow = n_var, ncol = n_var)
  }
  
  # Generating the data from multivariate normal distribution
  for (i in 1:n_group)
  {
    # The mean vector will be generated from uniform distribution with inputted
    # minimum and maximum
    mean_vec <- runif(n = n_var, min = mean_unif_limits[1], max = mean_unif_limits[2])
    
    dat[(((i-1)*obs_per_group)+1):(i*obs_per_group), 1:n_var] <- rmvnorm(n = obs_per_group, mean = mean_vec, sigma = sigma_mat)
    dat[(((i-1)*obs_per_group)+1):(i*obs_per_group), (n_var+1)] <- i
  }
  
  # Converting the data matrix into data frame and naming the group column
  dat <- data.frame(dat)
  for(i in 1:n_var)
  {
    colnames(dat)[i] <- paste("Variable", i, sep = "_")
  }
  colnames(dat)[(n_var+1)] <- "group"
  
  
  return(dat)
} 
