#########################################################################
###### ********Variance covariance function generation*********##########

# Identity variance covariance matrix can be generated using the 
# command:  diag(x = 6)


# Toeplitz variance covariance matrix can be generated using the following 
# function

Toeplitz_matrix <- function(r = 0.5, nrow, ncol)
{
  # This function will generate Toeplitz matrix for given value of r, number of 
  # rows and number of columns.
  
  # Creating a matrix to store the values of the elements of toeplitz matrix
  Toeplitz_matrix <- matrix(data = NA, nrow = nrow, ncol = ncol)
  
  # Calculating the values of the elements of toeplitz matrix
  for (i in 1:nrow)
  {
    for(j in 1:ncol)
    {
      Toeplitz_matrix[i, j] <- r^abs(i - j)
    }
  }
  
  return(Toeplitz_matrix)
}




# Equicorrelation variance covariance matrix can be generated using the following 
# function

equicorr_matrix <- function(r = 0.2, nrow, ncol)
{
  # This function will generate Equicorrelation matrix for given value of r, 
  # number of rows and number of columns.
  
  # Creating a matrix to store the values of the elements of equicorrelation matrix
  equicorr_matrix <- matrix(data = 1, nrow = nrow, ncol = ncol)
  
  # Calculating the values of the elements of equicorrelation matrix
  for (i in 1:nrow)
  {
    for(j in 1:ncol)
    {
      if (i != j)
      {
        equicorr_matrix[i, j] <- r
      }
      else next
    }
  }
  
  return(equicorr_matrix)
}

