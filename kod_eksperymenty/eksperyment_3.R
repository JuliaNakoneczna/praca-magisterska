library(noisemodel)
library(NoiseFiltersR)
library(dplyr)
library(randomForest)
library(caret)
library(ipred)
library(rpart)
library(nnet)
source("D:/2 stopień/4 semestr/praca dyplomowa/kody/hybridRepairFilter_poprawione.R")

# Funkcja pomocnicza do szumu NAR
noise_vector_NAR <- function(x, y, noise_level, epsilon = 1e-4){
  y <- as.factor(y)
  model <- randomForest(x = x, y = y, ntree = 100)
  class_error <- model$confusion[, "class.error"] + epsilon
  class_proportions <- table(y) / length(y)
  scaled_coeff <- noise_level / sum(class_error * class_proportions)
  noise_levels <- pmin(class_error * scaled_coeff, 1)
  return(noise_levels)}
####################################


# Funkcja do wyliczania metryk
metrics_fun_classification <- function(true_y, predicted_y){
  conf_matrix <- confusionMatrix(predicted_y, true_y)
  metrics <- rbind(conf_matrix$byClass)
  
  accuracy <- as.numeric(conf_matrix$overall["Accuracy"])
  precision <- mean(metrics[, "Precision"], na.rm = TRUE)
  recall <- mean(metrics[, "Recall"], na.rm = TRUE)
  F1_score <- mean(metrics[, "F1"], na.rm = TRUE)
  
  return(data.frame(Accuracy = accuracy, Precision = precision, Recall = recall, F1_score = F1_score))}
####################################


# Funkcja pomocnicza - klasyfikacja

classification_fun <- function(train_data, test_data, label, classifier, classifier_par, classifier_name, data_variant){
  
  test_data <- test_data[, names(train_data), drop = FALSE]
  y_test <- test_data[[label]]
  
  if (classifier == "knn"){
    arg <- c(list(formula = as.formula(paste(label, "~ .")), data = train_data), classifier_par)
    model <- do.call(ipredknn, arg)
    model_predict <- predict(model, test_data, type = "class")}
  else if (classifier == "tree"){
    arg <- c(list(formula = as.formula(paste(label, "~ .")), data = train_data, method = "class"), classifier_par)
    model <- do.call(rpart, arg)
    model_predict <- predict(model, test_data, type = "class")}
  else if (classifier == "logregression"){
    arg <- c(list(formula = as.formula(paste(label, "~ .")), data = train_data, trace = FALSE), classifier_par)
    model <- do.call(multinom, arg)
    model_predict <- predict(model, test_data, type = "class")}
  
  metrics <- metrics_fun_classification(y_test, model_predict)
  metrics$Variant <- data_variant
  metrics$Classifier <- classifier_name
  return(metrics)}
  

experiment_3 <- function(data, data_name, label, algorithm, algorithm_name, algorithm_par = list(), noise_level = 0.2, noise_type = "NCAR", 
                         classifier, classifier_par = list(), classifier_name, iterations = 20, seed = 12345, save_dir = "praca_magisterska_wyniki_eksperyment3"){
  result_list <- list()
  
  for (i in 1:iterations){
    seed_i <- seed + i
    set.seed(seed_i)
    
    train_index <- as.vector(createDataPartition(data[[label]], p = 0.7, list = FALSE))
    train_data <- data[train_index, ]
    test_data <- data[-train_index, ]
    y_train <- train_data[[label]]
    x_train <- train_data[, names(train_data) != label]
    y_test <- test_data[[label]]
    x_test <- test_data[, names(test_data) != label]
    
    
    if (noise_type == "NCAR"){
      noise_model <- sym_uni_ln(x = x_train, y = y_train, level = noise_level)}
    else if (noise_type == "NAR"){
      noise_model <- asy_uni_ln(x = x_train, y = y_train, level = noise_vector_NAR(x = x_train, y = y_train, noise_level = noise_level))}
    else if (noise_type == "NNAR"){
      noise_model <- nei_bor_ln(x = x_train, y = y_train, level = noise_level)}
    
    noise_train_data <- noise_model$xnoise
    noise_train_data[[label]] <- noise_model$ynoise
    
    if(data_name == "spam_standarize" && algorithm_name == "EF"){
      noise_data_spam <- subset(noise_train_data, subset=type=="spam", select=-type)
      noise_data_nonspam <- subset(noise_train_data, subset=type=="nonspam", select=-type)
      sd.spam   <- apply(noise_data_spam, 2, sd)
      sd.nospam <- apply(noise_data_nonspam, 2, sd)
      sd.zero.id <- which(sd.spam==0 | sd.nospam==0)
      if(length(sd.zero.id) > 0){ 
        noise_train_data <- noise_train_data[, -sd.zero.id]}}
  
    arg <- c(list(formula = as.formula(paste(label, "~ .")), data = noise_train_data), algorithm_par)
    algorithm_results <- do.call(algorithm, arg)
    clean_train_data <- algorithm_results$cleanData
    
    metrics_original_data <- classification_fun(train_data, test_data, label, classifier, classifier_par, classifier_name, "Original")
    metrics_noise_data <- classification_fun(noise_train_data, test_data, label, classifier, classifier_par, classifier_name, "Noise")
    metrics_clean_data <- classification_fun(clean_train_data, test_data, label, classifier, classifier_par, classifier_name, "Clean")
    
    metrics <- bind_rows(metrics_original_data, metrics_noise_data, metrics_clean_data)
    metrics$Data <- data_name
    metrics$Algorithm <- algorithm_name
    metrics$Classifier <- classifier_name
    metrics$Noise_level <- noise_level
    metrics$Noise_type <- noise_type

    result_list[[i]] <- metrics
    result_list_ALL <- bind_rows(result_list)
    name <- paste(data_name, algorithm_name, classifier_name, noise_level, noise_type, sep = "_")
    assign(name, result_list_ALL)
    save_path <- file.path(save_dir, paste0(name, ".RData"))  
    save(list = name, file = save_path)}
  
  return(result_list_ALL)}