rm(list = ls())

setwd(dir = "C:/Users/das70453/OneDrive - Texas Tech University/Thesis/simulation") # department pc

#setwd("~/Library/CloudStorage/OneDrive-TexasTechUniversity/Thesis/simulation") # Macbook


result=c()

for (RandomSeed in 1:100)
{
  for (number_of_observations in c(1000, 2000, 5000, 10000))
  {
    for (r in c(0.5, 1, 5))
    {
      for (sigma_for_data in c("identity", "toeplitz", "equicorrelation"))
      {
        filename <- paste("data/n_obs", number_of_observations, "_r", r, "sigma" ,sigma_for_data,"_Seed", RandomSeed, ".txt", sep = "")
        
        if (file.exists(filename)) {
          tryCatch({
            is = c(is, RandomSeed)
            df = read.table(filename, sep = " ")
            
            df$number_of_observations <- number_of_observations
            df$r <- r
            df$sigma_for_data <- sigma_for_data
            result=rbind(result,df)
          }, error = function(e) {
          })
        } 
      }
    }
  }
}



write.table(x = result, file = "result.csv", sep = ",", col.names = NA)

rm(list = ls())
result <- read.table(file = "result.csv", header = TRUE, sep = ",", row.names = 1)
head(result)




answer <- function(n_obs, mean_unif_r, sigma, dataset)
{
  accuracy <- c()
  method_names <- c("Multinomial Logistic", "SVM Classifier", 
                    "KNN classifier", "Decision Tree", 
                    "Bagging", "Random Forest", "Boosting")
  
 for (i in 1: length(method_names))
 {
 temp <- subset(x = dataset, method_names == method_names[i] & number_of_observations==n_obs & r ==mean_unif_r & sigma_for_data == sigma)

 accuracy[i] <- with(temp, mean(accuracy_by_method))
 }

return(data.frame(method_names, accuracy))
}


### This function will be used to subset the entire dataset.
answer_data_frame <- function(N){
  dat <- NULL
  r_val <- c(0.5, 1, 5)
  sigma_names <- c("identity", "toeplitz", "equicorrelation")
  for (i in r_val)
    {
    for(j in sigma_names)
     {
      temp <- answer(n_obs = N, mean_unif_r = i, sigma = j, dataset = result)
      temp <- cbind(temp, r = rep(i, each = 7), sigma = rep(j, times =7)) 
      dat <- rbind(dat, temp)
      
    }
  }

return(dat)  
}


library(dae)
# Interaction plot for n = 1000
interaction.ABC.plot(response = accuracy, x.factor = r, groups.factor = method_names,
                     trace.factor = sigma , data = answer_data_frame(1000), 
                     title = NULL)+theme(legend.position = "bottom")


# Interaction plot for n = 2000
interaction.ABC.plot(response = accuracy, x.factor = r, groups.factor = method_names,
                     trace.factor = sigma ,data = answer_data_frame(2000),
                     title = NULL)+theme(legend.position = "bottom")

# Interaction plot for n = 5000
interaction.ABC.plot(response = accuracy, x.factor = r, groups.factor = method_names,
                     trace.factor = sigma ,data = answer_data_frame(5000),
                     title = NULL)+theme(legend.position = "bottom")

# Interaction plot for n = 10000
interaction.ABC.plot(response = accuracy, x.factor = r, groups.factor = method_names,
                     trace.factor = sigma ,data = answer_data_frame(10000),
                     title = NULL)+theme(legend.position = "bottom")



answer(n_obs = 1000, mean_unif_r = 0.5, sigma = "identity", dataset = result)
answer(n_obs = 1000, mean_unif_r = 0.5, sigma = "toeplitz", dataset = result)
answer(n_obs = 1000, mean_unif_r = 0.5, sigma = "equicorrelation", dataset = result)
answer(n_obs = 1000, mean_unif_r = 1, sigma = "identity", dataset = result)
answer(n_obs = 1000, mean_unif_r = 1, sigma = "toeplitz", dataset = result)
answer(n_obs = 1000, mean_unif_r = 1, sigma = "equicorrelation", dataset = result)
answer(n_obs = 1000, mean_unif_r = 5, sigma = "identity", dataset = result)
answer(n_obs = 1000, mean_unif_r = 5, sigma = "toeplitz", dataset = result)
answer(n_obs = 1000, mean_unif_r = 5, sigma = "equicorrelation", dataset = result)


answer(n_obs = 2000, mean_unif_r = 0.5, sigma = "identity", dataset = result)
answer(n_obs = 2000, mean_unif_r = 0.5, sigma = "toeplitz", dataset = result)
answer(n_obs = 2000, mean_unif_r = 0.5, sigma = "equicorrelation", dataset = result)
answer(n_obs = 2000, mean_unif_r = 1, sigma = "identity", dataset = result)
answer(n_obs = 2000, mean_unif_r = 1, sigma = "toeplitz", dataset = result)
answer(n_obs = 2000, mean_unif_r = 1, sigma = "equicorrelation", dataset = result)
answer(n_obs = 2000, mean_unif_r = 5, sigma = "identity", dataset = result)
answer(n_obs = 2000, mean_unif_r = 5, sigma = "toeplitz", dataset = result)
answer(n_obs = 2000, mean_unif_r = 5, sigma = "equicorrelation", dataset = result)


answer(n_obs = 5000, mean_unif_r = 0.5, sigma = "identity", dataset = result)
answer(n_obs = 5000, mean_unif_r = 0.5, sigma = "toeplitz", dataset = result)
answer(n_obs = 5000, mean_unif_r = 0.5, sigma = "equicorrelation", dataset = result)
answer(n_obs = 5000, mean_unif_r = 1, sigma = "identity", dataset = result)
answer(n_obs = 5000, mean_unif_r = 1, sigma = "toeplitz", dataset = result)
answer(n_obs = 5000, mean_unif_r = 1, sigma = "equicorrelation", dataset = result)
answer(n_obs = 5000, mean_unif_r = 5, sigma = "identity", dataset = result)
answer(n_obs = 5000, mean_unif_r = 5, sigma = "toeplitz", dataset = result)
answer(n_obs = 5000, mean_unif_r = 5, sigma = "equicorrelation", dataset = result)



answer(n_obs = 10000, mean_unif_r = 0.5, sigma = "identity", dataset = result)
answer(n_obs = 10000, mean_unif_r = 0.5, sigma = "toeplitz", dataset = result)
answer(n_obs = 10000, mean_unif_r = 0.5, sigma = "equicorrelation", dataset = result)
answer(n_obs = 10000, mean_unif_r = 1, sigma = "identity", dataset = result)
answer(n_obs = 10000, mean_unif_r = 1, sigma = "toeplitz", dataset = result)
answer(n_obs = 10000, mean_unif_r = 1, sigma = "equicorrelation", dataset = result)
answer(n_obs = 10000, mean_unif_r = 5, sigma = "identity", dataset = result)
answer(n_obs = 10000, mean_unif_r = 5, sigma = "toeplitz", dataset = result)
answer(n_obs = 10000, mean_unif_r = 5, sigma = "equicorrelation", dataset = result)
