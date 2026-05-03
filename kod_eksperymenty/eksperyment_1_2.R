library(noisemodel)
library(NoiseFiltersR)
library(dplyr)
library(randomForest)
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
metrics_fun <- function(true_noise, predicted_noise, n){
  TP <- length(intersect(true_noise, predicted_noise))
  FP <- length(setdiff(predicted_noise, true_noise))
  FN <- length(setdiff(true_noise, predicted_noise))
  TN <- n - (TP + FP + FN)
  
  accuracy <- (TP + TN) / n
  precision <- if((TP + FP) == 0) 0 else TP / (TP + FP)
  recall <- if((TP + FN) == 0) 0 else TP / (TP + FN)
  jaccard <- if((TP + FP + FN) == 0) 0 else TP / (TP + FP + FN)
  
  return(data.frame(Accuracy = accuracy, Precision = precision, Recall = recall, Jaccard = jaccard))}



experiment_1 <- function(data, data_name, label, algorithm, algorithm_name, algorithm_par = list(), noise_level = 0.2, noise_type = "NCAR", iterations = 20, seed = 12345, save_dir = "praca_magisterska_wyniki"){
  n <- nrow(data)
  result_list <- list()
  
  Y <- data[[label]]
  X <- data[, names(data) != label]
  
  for (i in 1:iterations){
    seed_i <- seed + i
    set.seed(seed_i)
    
    if (noise_type == "NCAR"){
      noise_model <- sym_uni_ln(x = X, y = Y, level = noise_level)}
    else if (noise_type == "NAR"){
      noise_model <- asy_uni_ln(x = X, y = Y, level = noise_vector_NAR(x = X, y = Y, noise_level = noise_level))}
    else if (noise_type == "NNAR"){
      noise_model <- nei_bor_ln(x = X, y = Y, level = noise_level)}
  
    noise_data <- noise_model$xnoise
    noise_data[[label]] <- noise_model$ynoise
    true_noise <- unlist(noise_model$idnoise)
    
    if(data_name %in% c("spam_standarize", "spam_no_standarize") && algorithm_name == "PRISM"){
      noise_data_spam <- subset(noise_data, subset=type=="spam", select=-type)
      noise_data_nonspam <- subset(noise_data, subset=type=="nonspam", select=-type)
      sd.spam   <- apply(noise_data_spam, 2, sd)
      sd.nospam <- apply(noise_data_nonspam, 2, sd)
      sd.zero.id <- which(sd.spam==0 | sd.nospam==0)
      if(length(sd.zero.id) > 0){ 
        noise_data <- noise_data[, -sd.zero.id]}}
    
    arg <- c(list(formula = as.formula(paste(label, "~ .")), data = noise_data), algorithm_par)
    time_start <- Sys.time()
    algorithm_results <- do.call(algorithm, arg)
    time <- as.numeric(difftime(Sys.time(), time_start, units = "mins"))
    
    removed_noise <- algorithm_results$remIdx
    repaired_noise <- algorithm_results$repIdx
    predicted_noise <- union(removed_noise, repaired_noise)

    metrics <- metrics_fun(true_noise, predicted_noise, n)
    metrics$Time <- time
    metrics$Data <- data_name
    metrics$Algorithm <- algorithm_name
    metrics$Noise_level <- noise_level
    metrics$Noise_type <- noise_type
    metrics$True_noise_number <- length(true_noise)
    metrics$Predicted_noise_number <- length(predicted_noise)
    
    result_list[[i]] <- metrics
    result_list_ALL <- bind_rows(result_list)
    name <- paste(data_name, algorithm_name, noise_level, noise_type, sep = "_")
    assign(name, result_list_ALL)
    save_path <- file.path(save_dir, paste0(name, ".RData"))  
    save(list = name, file = save_path)}

  return(result_list_ALL)}