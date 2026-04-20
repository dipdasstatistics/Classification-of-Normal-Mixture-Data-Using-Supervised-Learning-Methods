accuracy_supervised_models <- function(train_covariates, train_group, test_covariates, test_group)
{ 
  # This function will calculate the accuracy of the following supervised
  # learning algorithms: Multinomial Logistic, Support Vector Machine, 
  # K nearest Neighborhood classifier, Decision Tree,
  # Bagging, Random Forest, Boosting
  
  train_data <- data.frame(train_covariates, train_group)
  test_data <- data.frame(test_covariates, test_group)
  
  ######### ***************** Multinomial logistic **************#############
  ##### Package nnet is required to run multinomial logistic regression####
  
  # Fitting multinomial logistic model on the training data and predict on test data
  multi_logistic <- multinom(formula = train_group ~ ., data = train_data, model = TRUE, trace = F)
  group_predicted_logistic <- predict(object = multi_logistic, newdata = test_data)
  logistic_accuracy_table <- with(test_data, table(test_group, group_predicted_logistic))
  
  # Accuracy of multinomial logistic model
  accuracy_logistic <- sum(diag(logistic_accuracy_table))/sum(logistic_accuracy_table)
  
  
  
  ######### ***************** Support vector machine **************#############
  ##### Package e1071 is required to run support vector machine algorithm####
  
  # Fitting support vector machine model on the training data and predict on test data
  fit_svm <- svm(as.factor(train_group)~., data = train_data, scale = FALSE, kernel = "linear", cost = 10)
  group_predicted_svm <- predict(object = fit_svm, newdata = test_data)
  svm_accuracy_table <- with(test_data, table(test_group, group_predicted_svm))
  
  # Accuracy of support vector machine model
  accuracy_svm <- sum(diag(svm_accuracy_table))/sum(svm_accuracy_table)
  
  
  ######### ********* K nearest neighbor classifier *******##########
  ##### Package class is required to run KNN algorithm####
  
  # Normalizing the features
  train_scale <- scale(train_data[,-ncol(train_data)])
  test_scale <- scale(test_data[,-ncol(test_data)])
  
  # Fitting KNN model on the training data and predict on test data
  group_predicted_knn <- knn(train = train_scale, test = test_scale, cl = train_data$train_group, k = sqrt(nrow(train_data))) #K is usually taken to be the half of number of sample
  knn_accuracy_table <- table(test_data$test_group, group_predicted_knn)
  
  # Accuracy of the KNN model
  accuracy_knn <- sum(diag(knn_accuracy_table))/sum(knn_accuracy_table)
  
  
  ############ ********* Decision Treee **********#############
  ######## Package rpart is required to run decision tree algorithm #######
  
  # Fitting decision tree model on the training data and predict on test data
  fit_dec_tree <- rpart(formula = train_data$train_group ~ ., data = train_data, method = "class")
  group_predicted_dc <- predict(object = fit_dec_tree, newdata = test_data, type = "class")
  
  # Accuracy of the decision tree model
  dc_accuracy_table <- table(test_data$test_group, group_predicted_dc)
  accuracy_dc <- sum(diag(dc_accuracy_table))/sum(dc_accuracy_table)
  
  
  # Next selecting the decision tree model with optimal alpha and predicting on 
  # test data
  cp_table <- fit_dec_tree$cptable
  optimal_cp <- cp_table[which.min(cp_table[,4]),1]
  pruned_dec_tree <- prune(tree = fit_dec_tree, optimal_cp)
  group_predicted_dc_pruned <- predict(object = pruned_dec_tree, newdata = test_data, type = "class")
  
  # Accuracy of the pruned decision tree model
  dc_pruned_accuracy_table <- table(test_data$test_group, group_predicted_dc_pruned)
  accuracy_dc_pruned <- sum(diag(dc_pruned_accuracy_table))/sum(dc_pruned_accuracy_table)
  
  
  ############## ********* Bagging method *************#############
  ######## Package ipred is required to run bagging method algorithm #######
  
  # Fitting bagging method on the training data and predict on test data
  # Taking 150 bootstrap samples, 
  bagging_fit <- bagging(formula = as.factor(train_group)~., data = train_data, nbagg = 150, cobb = T, control = rpart.control(minsplit = 2, cp = 0))
  group_predicted_bagging <- predict(object = bagging_fit, newdata = test_data)
  
  
  # Accuracy of the bagging method
  bagging_accuracy_table <- table(test_data$test_group, group_predicted_bagging)
  accuracy_bagging <- sum(diag(bagging_accuracy_table))/sum(bagging_accuracy_table)
  
  
  ############## ********* Random Forest method *************#############
  ######## Package randomForest is required to run random forest algorithm #####
  
  # Fitting random forest method on the training data and predict on test data
  fit_rf <- randomForest(formula = factor(train_group)~., data = train_data, method = "class")
  group_predicted_rf <- predict(object = fit_rf, newdata = test_data)
  
  # Accuracy of the random forest method
  rf_accuracy_table <- table(test_data$test_group, group_predicted_rf)
  accuracy_rf <- sum(diag(rf_accuracy_table))/sum(rf_accuracy_table)
  
  
  ############## ********* Boosting method *************#############
  ######## Package caret is required to run boosting algorithm #######
  
  # Fitting boosting method on the training data and predict on test data
    fitControl <- trainControl(## 10-fold CV 
    method = "repeatedcv",
    number = 10,
    ## repeated ten times
    repeats = 10)

    fit_gbm <- train(as.factor(train_group) ~ ., data = train_data, 
                   method = "gbm", 
                   trControl = fitControl,
                   ## This last option is actually one
                   ## for gbm() that passes through
                   verbose = FALSE)
  group_predicted_boosting <- predict(object = fit_gbm, newdata = test_data, outputmargin = T)
  
  # Accuracy of the boosting method
  boosting_accuracy_table <- table(test_data$test_group, group_predicted_boosting)
  accuracy_boosting <- sum(diag(boosting_accuracy_table))/sum(boosting_accuracy_table)
  
  
  # Returning the comparison results
  method_names <- c("Multinomial Logistic", "Support Vector Machine", 
                    "K nearest Neighborhood classifier", "Decision Tree", 
                    "Bagging", "Random Forest", "Boosting")
  accuracy_by_method <- c(accuracy_logistic, accuracy_svm, accuracy_knn, accuracy_dc_pruned, accuracy_bagging, accuracy_rf, accuracy_boosting)
  
  accuracy_comparison <- data.frame(method_names, accuracy_by_method)
  
  return(accuracy_comparison)
  

}




