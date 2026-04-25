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
  
  if (!dir.exists(save_dir)){
    dir.create(save_dir)}
  
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
    
    formula <- as.formula(paste(label, "~ ."))
    arg <- c(list(formula = formula, data = noise_data), algorithm_par)
    time_start <- Sys.time()
    algorithm_results <- do.call(algorithm, arg)
    time <- as.numeric(difftime(Sys.time(), time_start, units = "mins"))
    
      removed_noise <- if(is.null(algorithm_results$remIdx)) integer(0) else algorithm_results$remIdx
      repaired_noise <- if(is.null(algorithm_results$repIdx)) integer(0) else algorithm_results$repIdx
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


ALL_algorithms <- list(
  list(algorithm = ENN, name = "ENN", par = list()),
  list(algorithm = AENN, name = "AENN", par = list()),
  list(algorithm = BBNR, name = "BBNR", par = list()),
  list(algorithm = ENG, name = "ENG", par = list()),
  #list(algorithm = PRISM, name = "PRISM", par = list()),
  list(algorithm = C45robustFilter, name = "C45robustFilter", par = list()),
  list(algorithm = CVCF, name = "CVCF", par = list()),
  list(algorithm = C45iteratedVotingFilter, name = "C45iteratedVotingFilter", par = list()),
  list(algorithm = IPF, name = "IPF", par = list()),
  list(algorithm = EF, name = "EF", par = list(nfolds = 8)),
  list(algorithm = dynamicCF, name = "dynamicCF", par = list()),
  list(algorithm = HARF, name = "HARF", par = list()),
  list(algorithm = edgeBoostFilter, name = "edgeBoostFilter", par = list()),
  list(algorithm = ORBoostFilter, name = "ORBoostFilter", par = list(N = 15)),
  list(algorithm = hybridRepairFilter, name = "hybridRepairFilter_repair", par = list(noiseAction = "repair")),
  list(algorithm = hybridRepairFilter, name = "hybridRepairFilter_hybrid", par = list(noiseAction = "hybrid")),
  list(algorithm = EWF, name = "EWF", par = list(noiseAction = "hybrid")),
  list(algorithm = GE, name = "GE", par = list()))

ALL_banknote_exp1 <- lapply(ALL_algorithms, function(alg){
  experiment_1(data = banknote_standarize, data_name = "banknote", label = "Status", algorithm = alg$algorithm, algorithm_name = alg$name, algorithm_par = alg$par)})

banknote_0.2_NCAR <- list(
  algorithms = ALL_algorithms,
  results = ALL_banknote_exp1)
save(banknote_0.2_NCAR, file = file.path("praca_magisterska_wyniki/zbiorcze", "banknote_0.2_NCAR.RData"))

ALL_spam_exp1 <- lapply(ALL_algorithms, function(alg){
  experiment_1(data = spam_standarize, data_name = "spam", label = "type", algorithm = alg$algorithm, algorithm_name = alg$name, algorithm_par = alg$par)})

spam_0.2_NCAR <- list(
  algorithms = ALL_algorithms,
  results = ALL_spam_exp1)
save(spam_0.2_NCAR, file = file.path("praca_magisterska_wyniki/zbiorcze", "spam_0.2_NCAR.RData"))

edgeBoostFilter(type~., data = spam_standarize)
experiment_1(data = spam_standarize, data_name = "spam", label = "type", algorithm = edgeBoostFilter, algorithm_name = "edgeBoost", iterations = 5)