library(dplyr)
library(tidyr)
library(xtable)
library(ggplot2)

algorithms <- list(
  list(algorithm = ENN, name = "ENN", par = list()),
  list(algorithm = PRISM, name = "PRISM", par = list()),
  list(algorithm = CVCF, name = "CVCF", par = list()),
  list(algorithm = EF, name = "EF", par = list(nfolds = 8)),
  list(algorithm = HARF, name = "HARF", par = list()),
  list(algorithm = ORBoostFilter, name = "ORBoostFilter", par = list(N = 15)),
  list(algorithm = hybridRepairFilter, name = "hybridRepairFilter_repair", par = list(noiseAction = "repair")),
  list(algorithm = EWF, name = "EWF", par = list(noiseAction = "hybrid")))

noise_levels <- c(0.1, 0.2, 0.3, 0.4)
noise_types <- c("NCAR", "NAR", "NNAR")

banknote_noise_level <- lapply(noise_levels, function(level){
  results <- lapply(algorithms, function(alg){
  experiment_1(data = banknote_standarize, data_name = "banknote_standarize", label = "Status", algorithm = alg$algorithm, algorithm_name = alg$name, algorithm_par = alg$par, noise_level = level)})
  list(algorithms = algorithms, noise_level = level, results = results)})
save(banknote_noise_level, file = file.path("praca_magisterska_wyniki/zbiorcze", "banknote_noise_levels.RData"))

spam_noise_level <- lapply(noise_levels, function(level){
  results <- lapply(algorithms, function(alg){
    experiment_1(data = spam_standarize, data_name = "spam_standarize", label = "type", algorithm = alg$algorithm, algorithm_name = alg$name, algorithm_par = alg$par, noise_level = level)})
  list(algorithms = algorithms, noise_level = level, results = results)})
save(spam_noise_level, file = file.path("praca_magisterska_wyniki/zbiorcze", "spam_noise_levels.RData"))

vehicle_noise_level <- lapply(noise_levels, function(level){
  results <- lapply(algorithms, function(alg){
    experiment_1(data = vehicle_standarize, data_name = "vehicle_standarize", label = "Class", algorithm = alg$algorithm, algorithm_name = alg$name, algorithm_par = alg$par, noise_level = level)})
  list(algorithms = algorithms, noise_level = level, results = results)})
save(vehicle_noise_level, file = file.path("praca_magisterska_wyniki/zbiorcze", "vehicle_noise_levels.RData"))


banknote_noise_type <- lapply(noise_types, function(type){
  results <- lapply(algorithms, function(alg){
    experiment_1(data = banknote_standarize, data_name = "banknote_standarize", label = "Status", algorithm = alg$algorithm, algorithm_name = alg$name, algorithm_par = alg$par, noise_type = type)})
  list(algorithms = algorithms, noise_type = type, results = results)})
save(banknote_noise_type, file = file.path("praca_magisterska_wyniki/zbiorcze", "banknote_noise_types.RData"))

spam_noise_type <- lapply(noise_types, function(type){
  results <- lapply(algorithms, function(alg){
    experiment_1(data = spam_standarize, data_name = "spam_standarize", label = "type", algorithm = alg$algorithm, algorithm_name = alg$name, algorithm_par = alg$par, noise_type = type)})
  list(algorithms = algorithms, noise_type =type, results = results)})
save(spam_noise_type, file = file.path("praca_magisterska_wyniki/zbiorcze", "spam_noise_types.RData"))

vehicle_noise_type <- lapply(noise_types, function(type){
  results <- lapply(algorithms, function(alg){
    experiment_1(data = vehicle_standarize, data_name = "vehicle_standarize", label = "Class", algorithm = alg$algorithm, algorithm_name = alg$name, algorithm_par = alg$par, noise_type = type)})
  list(algorithms = algorithms, noise_type = type, results = results)})
save(vehicle_noise_type, file = file.path("praca_magisterska_wyniki/zbiorcze", "vehicle_noise_types.RData"))
