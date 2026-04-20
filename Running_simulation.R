rm(list = ls())

#setwd(dir = "C:/Users/das70453/OneDrive - Texas Tech University/Thesis/simulation") # Departmental PC

# setwd("~/Library/CloudStorage/OneDrive-TexasTechUniversity/Thesis/simulation") # Macbook

setwd("/home/das70453/thesis/simulation") # High Performace cloud computing

source(file = "required_packages.R")

source(file = "accuracy_supervised_function.R")

source(file = "var_cov_mat_functions.R")

source(file = "data_generation.R")


args <- commandArgs(TRUE)


number_of_observations <- as.numeric(args[1])
r <- as.numeric(args[2])
sigma_for_data <- args[3]
RandomSeed <- as.numeric(args[4])
set.seed(RandomSeed)


# The following inputs are required as the input of the functions

number_of_groups <- 4

number_of_variables <- 10

mean_generation_unif_r <- c(-r, r)

prop_sample_train <- 0.7

data <- data_generation(n_obs = number_of_observations, n_group = number_of_groups, n_var = number_of_variables, mean_unif_limits = mean_generation_unif_r, sigma_type = sigma_for_data)


# Partitioning the data frame into training and test data
index_sample <- sample(x = nrow(data), size = prop_sample_train * nrow(data), replace = F)
train <- data[index_sample,]
test <- data[-index_sample,]

# Checking accuracy
result <- accuracy_supervised_models(train_covariates = train[,(1:number_of_variables)], train_group = train$group, test_covariates = test[,(1:number_of_variables)], test_group = test$group)

filename=paste("data/n_obs", number_of_observations, "_r", r, "sigma" ,sigma_for_data,"_Seed", RandomSeed, ".txt", sep = "")

write.table(result, file = filename, append = FALSE, quote = TRUE, sep = " ")
