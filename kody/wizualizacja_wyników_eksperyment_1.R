library(dplyr)
library(tidyr)
library(xtable)
library(ggplot2)

ALL_algorithms <- list(
  list(algorithm = ENN, name = "ENN", par = list()),
  list(algorithm = AENN, name = "AENN", par = list()),
  list(algorithm = BBNR, name = "BBNR", par = list()),
  list(algorithm = ENG, name = "ENG", par = list()),
  list(algorithm = PRISM, name = "PRISM", par = list()),
  list(algorithm = C45robustFilter, name = "C45robustFilter", par = list()),
  list(algorithm = CVCF, name = "CVCF", par = list()),
  list(algorithm = C45iteratedVotingFilter, name = "C45iteratedVotingFilter", par = list()),
  list(algorithm = IPF, name = "IPF", par = list()),
  list(algorithm = EF, name = "EF", par = list(nfolds = 8)),
  list(algorithm = dynamicCF, name = "dynamicCF", par = list()),
  list(algorithm = HARF, name = "HARF", par = list()),
  list(algorithm = ORBoostFilter, name = "ORBoostFilter", par = list(N = 15)),
  list(algorithm = hybridRepairFilter, name = "hybridRepairFilter_remove", par = list(noiseAction = "remove")),
  list(algorithm = hybridRepairFilter, name = "hybridRepairFilter_repair", par = list(noiseAction = "repair")),
  list(algorithm = hybridRepairFilter, name = "hybridRepairFilter_hybrid", par = list(noiseAction = "hybrid")),
  list(algorithm = EWF, name = "EWF", par = list(noiseAction = "hybrid")),
  list(algorithm = GE, name = "GE", par = list()))

ALL_banknote_standarize <- lapply(ALL_algorithms, function(alg){
  experiment_1(data = banknote_standarize, data_name = "banknote_standarize", label = "Status", algorithm = alg$algorithm, algorithm_name = alg$name, algorithm_par = alg$par)})
banknote_standarize_0.2_NCAR <- list(
  algorithms = ALL_algorithms,
  results = ALL_banknote_standarize)
save(banknote_standarize_0.2_NCAR, file = file.path("praca_magisterska_wyniki/zbiorcze", "banknote_standarize_0.2_NCAR.RData"))

ALL_banknote_no_standarize <- lapply(ALL_algorithms, function(alg){
  experiment_1(data = banknote_no_standarize, data_name = "banknote_no_standarize", label = "Status", algorithm = alg$algorithm, algorithm_name = alg$name, algorithm_par = alg$par)})
banknote_no_standarize_0.2_NCAR <- list(
  algorithms = ALL_algorithms,
  results = ALL_banknote_no_standarize)
save(banknote_no_standarize_0.2_NCAR, file = file.path("praca_magisterska_wyniki/zbiorcze", "banknote_no_standarize_0.2_NCAR.RData"))

ALL_spam_standarize <- lapply(ALL_algorithms, function(alg){
  experiment_1(data = spam_standarize, data_name = "spam_standarize", label = "type", algorithm = alg$algorithm, algorithm_name = alg$name, algorithm_par = alg$par)})
spam_standarize_0.2_NCAR <- list(
  algorithms = ALL_algorithms,
  results = ALL_spam_standarize)
save(spam_standarize_0.2_NCAR, file = file.path("praca_magisterska_wyniki/zbiorcze", "spam_standarize_0.2_NCAR.RData"))

ALL_spam_no_standarize <- lapply(ALL_algorithms, function(alg){
  experiment_1(data = spam_no_standarize, data_name = "spam_no_standarize", label = "type", algorithm = alg$algorithm, algorithm_name = alg$name, algorithm_par = alg$par)})
spam_no_standarize_0.2_NCAR <- list(
  algorithms = ALL_algorithms,
  results = ALL_spam_no_standarize)
save(spam_no_standarize_0.2_NCAR, file = file.path("praca_magisterska_wyniki/zbiorcze", "spam_no_standarize_0.2_NCAR.RData"))

ALL_vehicle_standarize <- lapply(ALL_algorithms, function(alg){
  experiment_1(data = vehicle_standarize, data_name = "vehicle_standarize", label = "Class", algorithm = alg$algorithm, algorithm_name = alg$name, algorithm_par = alg$par)})
vehicle_standarize_0.2_NCAR <- list(
  algorithms = ALL_algorithms,
  results = ALL_vehicle_standarize)
save(vehicle_standarize_0.2_NCAR, file = file.path("praca_magisterska_wyniki/zbiorcze", "vehicle_standarize_0.2_NCAR.RData"))

ALL_vehicle_no_standarize <- lapply(ALL_algorithms, function(alg){
  experiment_1(data = vehicle_no_standarize, data_name = "vehicle_no_standarize", label = "Class", algorithm = alg$algorithm, algorithm_name = alg$name, algorithm_par = alg$par)})
vehicle_no_standarize_0.2_NCAR <- list(
  algorithms = ALL_algorithms,
  results = ALL_vehicle_no_standarize)
save(vehicle_no_standarize_0.2_NCAR, file = file.path("praca_magisterska_wyniki/zbiorcze", "vehicle_no_standarize_0.2_NCAR.RData"))

boxplot_fun <- function(data, name){
  results <- bind_rows(data$results)
  select_and_correct <- c("hybridRepairFilter_repair", "hybridRepairFilter_hybrid", "EWF", "GE")
  results <- results %>%
    mutate(
      Algorithm_Group = if_else(
        Algorithm %in% select_and_correct,
        "Select-and-Correct",
        "Sample Selection"))
  order <- results %>%
    distinct(Algorithm, Algorithm_Group) %>%
    arrange(factor(Algorithm_Group, levels = c("Sample Selection", "Select-and-Correct")),
            Algorithm) %>%
    pull(Algorithm)
  
  results$Algorithm <- factor(results$Algorithm, levels = order)
  results <- results %>%
    select(Algorithm, Algorithm_Group, Accuracy, Precision, Recall, Jaccard) %>%
    pivot_longer(
      cols = c(Accuracy, Precision, Recall, Jaccard),
      names_to = "Metric",
      values_to = "Value")
  results$Metric <- factor(results$Metric, levels = c("Accuracy", "Precision", "Recall", "Jaccard"))
  plot <- ggplot(results, aes(x = Algorithm, y = Value, fill = Algorithm_Group)) +
    geom_boxplot(outlier.size = 0.5, outlier.alpha = 1, alpha = 0.7, linewidth = 0.5) +
    facet_wrap(~ Metric, scales = "free_y", ncol = 1) +
    theme_light() +
    labs(x = NULL, y = NULL, fill = "Metoda:") +
    theme(
      axis.text.x = element_text(size = 11, colour = "black", angle = 45, hjust = 1), 
      axis.text.y = element_text(size = 11, colour = "black"),
      strip.text = element_text(size = 12, face = "bold", colour = "black"),
      legend.position = "bottom",
      legend.text = element_text(size = 11),
      legend.title = element_text(size = 12, face = "bold")) +
    scale_fill_manual(values = c("Sample Selection" = "#008080", "Select-and-Correct" = "#FF69B4"))
  ggsave(name, plot = plot, width = 8, height = 10, units = "in", device = "pdf")}

boxplot_fun(banknote_no_standarize_0.2_NCAR, "D:/2 stopień/4 semestr/praca dyplomowa/kody/eksperyment_1/banknote_no_standarize.pdf")
boxplot_fun(banknote_standarize_0.2_NCAR, "D:/2 stopień/4 semestr/praca dyplomowa/kody/eksperyment_1/banknote_standarize.pdf")
boxplot_fun(spam_no_standarize_0.2_NCAR, "D:/2 stopień/4 semestr/praca dyplomowa/kody/eksperyment_1/spam_no_standarize.pdf")
boxplot_fun(spam_standarize_0.2_NCAR, "D:/2 stopień/4 semestr/praca dyplomowa/kody/eksperyment_1/spam_standarize.pdf")
boxplot_fun(vehicle_no_standarize_0.2_NCAR, "D:/2 stopień/4 semestr/praca dyplomowa/kody/eksperyment_1/vehicle_no_standarize.pdf")
boxplot_fun(vehicle_standarize_0.2_NCAR, "D:/2 stopień/4 semestr/praca dyplomowa/kody/eksperyment_1/vehicle_standarize.pdf")

table_fun <- function(data, name){
  
  algorithm_labels <- c(
    "AENN" = "AENN", "BBNR" = "BBNR", "C45iteratedVotingFilter" = "C45iVF", "C45robustFilter" = "C45rF",
    "CVCF" = "CVCF", "dynamicCF" = "dCF", "EF" = "EF", "ENG" = "ENG", "PRISM" = "PRISM", "ENN" = "ENN",
    "EWF" = "EWF", "GE" = "GE", "HARF" = "HARF","hybridRepairFilter_remove" = "hRF - remove", "hybridRepairFilter_hybrid" = "hRF - hybrid", 
    "hybridRepairFilter_repair" = "hRF - repair", "IPF" = "IPF", "ORBoostFilter" = "ORBF")
  results <- bind_rows(data$results)
  select_and_correct <- c("hybridRepairFilter_repair", "hybridRepairFilter_hybrid", "EWF", "GE")
  results <- results %>%
    mutate(
      Algorithm_label = recode(Algorithm, !!!algorithm_labels),
      Algorithm_Group = if_else(
        Algorithm %in% select_and_correct,
        "Select-and-Correct",
        "Sample Selection"),
      predicted_true_ratio = Predicted_noise_number / True_noise_number)
  
  mean_sd <- results %>%
    group_by(Algorithm_Group, Algorithm_label) %>%
    summarise(
      Accuracy_mean = mean(Accuracy, na.rm = TRUE), Accuracy_sd = sd(Accuracy, na.rm = TRUE),
      Precision_mean = mean(Precision, na.rm = TRUE), Precision_sd = sd(Precision, na.rm = TRUE),
      Recall_mean = mean(Recall, na.rm = TRUE), Recall_sd = sd(Recall, na.rm = TRUE),
      Jaccard_mean = mean(Jaccard, na.rm = TRUE), Jaccard_sd = sd(Jaccard, na.rm = TRUE),
      Time_mean = mean(Time, na.rm = TRUE), Time_sd = sd(Time, na.rm = TRUE),
      Ratio_mean = mean(predicted_true_ratio, na.rm = TRUE), Ratio_sd = sd(predicted_true_ratio, na.rm = TRUE),
      .groups = "drop")
  best_Accuracy <- max(mean_sd$Accuracy_mean, na.rm = TRUE)
  best_Precision <- max(mean_sd$Precision_mean, na.rm = TRUE)
  best_Recall <- max(mean_sd$Recall_mean, na.rm = TRUE)
  best_Jaccard <- max(mean_sd$Jaccard_mean, na.rm = TRUE)
  best_Ratio  <- mean_sd$Ratio_mean[which.min(abs(mean_sd$Ratio_mean - 1))]
  
  no_zero <- function(value){
    text <- sprintf("%.3f", value)
    ifelse(abs(value) < 1, sub("0\\.", ".", text), text)}
  
  mean_pm_sd <- function(mean_value, sd_value, best_value){
    text <- paste0(sprintf("%.3f", mean_value), " $\\pm$ ", no_zero(sd_value))
    ifelse(abs(mean_value - best_value) < 1e-7, paste0("\\textbf{", text, "}"), text)}
  
  table <- mean_sd %>%
    mutate(
      Accuracy = mean_pm_sd(Accuracy_mean, Accuracy_sd, best_Accuracy),
      Precision = mean_pm_sd(Precision_mean, Precision_sd, best_Precision),
      Recall = mean_pm_sd(Recall_mean, Recall_sd, best_Recall),
      Jaccard = mean_pm_sd(Jaccard_mean, Jaccard_sd, best_Jaccard),
      Time = paste0(sprintf("%.3f", Time_mean), " $\\pm$ ", no_zero(Time_sd)),
      'Detection ratio' = mean_pm_sd(Ratio_mean, Ratio_sd, best_Ratio)) %>%
    arrange(
      factor(Algorithm_Group, levels = c("Sample Selection", "Select-and-Correct"))) %>%
    select(Algorithm_label, Accuracy, Precision, Recall, Jaccard, Time, 'Detection ratio') %>%
    rename(Algorytm = Algorithm_label, Czas = Time)
  table_latex <- xtable(table, caption = NULL, label = NULL, align = c("l", "l", "c", "c", "c", "c", "c", "c"))
  print(table_latex, type = "latex", file = name, include.rownames = FALSE, sanitize.text.function = identity, floating = FALSE)}

table_fun(banknote_standarize_0.2_NCAR, "D:/2 stopień/4 semestr/praca dyplomowa/kody/eksperyment_1/banknote_standarize.tex")
table_fun(spam_standarize_0.2_NCAR, "D:/2 stopień/4 semestr/praca dyplomowa/kody/eksperyment_1/spam_standarize.tex")
table_fun(vehicle_standarize_0.2_NCAR, "D:/2 stopień/4 semestr/praca dyplomowa/kody/eksperyment_1/vehicle_standarize.tex")
