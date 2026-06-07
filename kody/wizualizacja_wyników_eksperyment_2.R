library(dplyr)
library(tidyr)
library(xtable)
library(ggplot2)
library(patchwork)

algorithms <- list(
  list(algorithm = ENN, name = "ENN", par = list()),
  list(algorithm = ENG, name = "ENG", par = list()),
  list(algorithm = PRISM, name = "PRISM", par = list()),
  list(algorithm = CVCF, name = "CVCF", par = list()),
  list(algorithm = GE, name = "GE", par = list()),
  list(algorithm = hybridRepairFilter, name = "hybridRepairFilter_remove", par = list(noiseAction = "remove")),
  list(algorithm = ORBoostFilter, name = "ORBoostFilter", par = list(N = 15)),
  list(algorithm = hybridRepairFilter, name = "hybridRepairFilter_repair", par = list(noiseAction = "repair")),
  list(algorithm = EF, name = "EF", par = list(nfolds = 8)))

noise_levels <- c(0.1, 0.2, 0.3, 0.4)
noise_types <- c("NCAR", "NAR", "NNAR")

banknote_noise_levels <- lapply(noise_levels, function(level){
  results <- lapply(algorithms, function(alg){
  experiment_1(data = banknote_standarize, data_name = "banknote_standarize", label = "Status", algorithm = alg$algorithm, algorithm_name = alg$name, algorithm_par = alg$par, noise_level = level)})
  list(algorithms = algorithms, noise_level = level, results = results)})
save(banknote_noise_levels, file = file.path("praca_magisterska_wyniki/zbiorcze", "banknote_noise_levels.RData"))

spam_noise_levels <- lapply(noise_levels, function(level){
  results <- lapply(algorithms, function(alg){
    experiment_1(data = spam_standarize, data_name = "spam_standarize", label = "type", algorithm = alg$algorithm, algorithm_name = alg$name, algorithm_par = alg$par, noise_level = level)})
  list(algorithms = algorithms, noise_level = level, results = results)})
save(spam_noise_levels, file = file.path("praca_magisterska_wyniki/zbiorcze", "spam_noise_levels.RData"))

vehicle_noise_levels <- lapply(noise_levels, function(level){
  results <- lapply(algorithms, function(alg){
    experiment_1(data = vehicle_standarize, data_name = "vehicle_standarize", label = "Class", algorithm = alg$algorithm, algorithm_name = alg$name, algorithm_par = alg$par, noise_level = level)})
  list(algorithms = algorithms, noise_level = level, results = results)})
save(vehicle_noise_levels, file = file.path("praca_magisterska_wyniki/zbiorcze", "vehicle_noise_levels.RData"))


banknote_noise_types <- lapply(noise_types, function(type){
  results <- lapply(algorithms, function(alg){
    experiment_1(data = banknote_standarize, data_name = "banknote_standarize", label = "Status", algorithm = alg$algorithm, algorithm_name = alg$name, algorithm_par = alg$par, noise_type = type)})
  list(algorithms = algorithms, noise_type = type, results = results)})
save(banknote_noise_types, file = file.path("praca_magisterska_wyniki/zbiorcze", "banknote_noise_types.RData"))

spam_noise_types <- lapply(noise_types, function(type){
  results <- lapply(algorithms, function(alg){
    experiment_1(data = spam_standarize, data_name = "spam_standarize", label = "type", algorithm = alg$algorithm, algorithm_name = alg$name, algorithm_par = alg$par, noise_type = type)})
  list(algorithms = algorithms, noise_type =type, results = results)})
save(spam_noise_types, file = file.path("praca_magisterska_wyniki/zbiorcze", "spam_noise_types.RData"))

vehicle_noise_types <- lapply(noise_types, function(type){
  results <- lapply(algorithms, function(alg){
    experiment_1(data = vehicle_standarize, data_name = "vehicle_standarize", label = "Class", algorithm = alg$algorithm, algorithm_name = alg$name, algorithm_par = alg$par, noise_type = type)})
  list(algorithms = algorithms, noise_type = type, results = results)})
save(vehicle_noise_types, file = file.path("praca_magisterska_wyniki/zbiorcze", "vehicle_noise_types.RData"))


rank_table_fun <- function(data, name, metric, rank_by){
  
  algorithm_labels <- c(
    "CVCF" = "CVCF", "EF" = "EF", "ENG" = "ENG", "PRISM" = "PRISM", "ENN" = "ENN",
    "GE" = "GE", "hybridRepairFilter_remove" = "hRF - remove", 
    "hybridRepairFilter_repair" = "hRF - repair",  "ORBoostFilter" = "ORBF")
  
  if (rank_by == "level"){rank_column <- "Noise_level"}
  else if (rank_by == "type"){rank_column <- "Noise_type"}
  
  results <- bind_rows(lapply(data, function(x){bind_rows(x$results)}))
  results <- results %>%
    mutate(
      Algorithm_label = recode(Algorithm, !!!algorithm_labels))
  
  mean_results <- results %>%
    group_by(.data[[rank_column]], Algorithm_label) %>%
    summarise(
      mean_value = mean(.data[[metric]], na.rm = TRUE),
      .groups = "drop")
  
  table <- mean_results %>%
    group_by(.data[[rank_column]]) %>%
    mutate(rank = rank(-mean_value, ties.method = "first"),
           cell = paste0(Algorithm_label, " (", sprintf(paste0("%.3f"), mean_value), ")")) %>%
    ungroup() %>%
    select(rank, all_of(rank_column), cell) %>%
    pivot_wider(names_from = all_of(rank_column), values_from = cell) %>%
    arrange(rank) 
  
  table_latex <- xtable(table, caption = NULL, label = NULL)
  print(table_latex, type = "latex", file = name, include.rownames = FALSE, sanitize.text.function = identity, floating = FALSE)}


rank_table_fun(banknote_noise_levels, "D:/2 stopień/4 semestr/praca dyplomowa/kody/eksperyment_2/banknote_noise_levels_accuracy.tex", "Accuracy", "level")
rank_table_fun(banknote_noise_levels, "D:/2 stopień/4 semestr/praca dyplomowa/kody/eksperyment_2/banknote_noise_levels_precision.tex", "Precision", "level")
rank_table_fun(banknote_noise_levels, "D:/2 stopień/4 semestr/praca dyplomowa/kody/eksperyment_2/banknote_noise_levels_recall.tex", "Recall", "level")
rank_table_fun(banknote_noise_levels, "D:/2 stopień/4 semestr/praca dyplomowa/kody/eksperyment_2/banknote_noise_levels_jaccard.tex", "Jaccard", "level")

rank_table_fun(spam_noise_levels, "D:/2 stopień/4 semestr/praca dyplomowa/kody/eksperyment_2/spam_noise_levels_accuracy.tex", "Accuracy", "level")
rank_table_fun(spam_noise_levels, "D:/2 stopień/4 semestr/praca dyplomowa/kody/eksperyment_2/spam_noise_levels_precision.tex", "Precision", "level")
rank_table_fun(spam_noise_levels, "D:/2 stopień/4 semestr/praca dyplomowa/kody/eksperyment_2/spam_noise_levels_recall.tex", "Recall", "level")
rank_table_fun(spam_noise_levels, "D:/2 stopień/4 semestr/praca dyplomowa/kody/eksperyment_2/spam_noise_levels_jaccard.tex", "Jaccard", "level")

rank_table_fun(vehicle_noise_levels, "D:/2 stopień/4 semestr/praca dyplomowa/kody/eksperyment_2/vehicle_noise_levels_accuracy.tex", "Accuracy", "level")
rank_table_fun(vehicle_noise_levels, "D:/2 stopień/4 semestr/praca dyplomowa/kody/eksperyment_2/vehicle_noise_levels_precision.tex", "Precision", "level")
rank_table_fun(vehicle_noise_levels, "D:/2 stopień/4 semestr/praca dyplomowa/kody/eksperyment_2/vehicle_noise_levels_recall.tex", "Recall", "level")
rank_table_fun(vehicle_noise_levels, "D:/2 stopień/4 semestr/praca dyplomowa/kody/eksperyment_2/vehicle_noise_levels_jaccard.tex", "Jaccard", "level")


rank_table_fun(banknote_noise_types, "D:/2 stopień/4 semestr/praca dyplomowa/kody/eksperyment_2/banknote_noise_types_accuracy.tex", "Accuracy", "type")
rank_table_fun(banknote_noise_types, "D:/2 stopień/4 semestr/praca dyplomowa/kody/eksperyment_2/banknote_noise_types_precision.tex", "Precision", "type")
rank_table_fun(banknote_noise_types, "D:/2 stopień/4 semestr/praca dyplomowa/kody/eksperyment_2/banknote_noise_types_recall.tex", "Recall", "type")
rank_table_fun(banknote_noise_types, "D:/2 stopień/4 semestr/praca dyplomowa/kody/eksperyment_2/banknote_noise_types_jaccard.tex", "Jaccard", "type")

rank_table_fun(spam_noise_types, "D:/2 stopień/4 semestr/praca dyplomowa/kody/eksperyment_2/spam_noise_types_accuracy.tex", "Accuracy", "type")
rank_table_fun(spam_noise_types, "D:/2 stopień/4 semestr/praca dyplomowa/kody/eksperyment_2/spam_noise_types_precision.tex", "Precision", "type")
rank_table_fun(spam_noise_types, "D:/2 stopień/4 semestr/praca dyplomowa/kody/eksperyment_2/spam_noise_types_recall.tex", "Recall", "type")
rank_table_fun(spam_noise_types, "D:/2 stopień/4 semestr/praca dyplomowa/kody/eksperyment_2/spam_noise_types_jaccard.tex", "Jaccard", "type")

rank_table_fun(vehicle_noise_types, "D:/2 stopień/4 semestr/praca dyplomowa/kody/eksperyment_2/vehicle_noise_types_accuracy.tex", "Accuracy", "type")
rank_table_fun(vehicle_noise_types, "D:/2 stopień/4 semestr/praca dyplomowa/kody/eksperyment_2/vehicle_noise_types_precision.tex", "Precision", "type")
rank_table_fun(vehicle_noise_types, "D:/2 stopień/4 semestr/praca dyplomowa/kody/eksperyment_2/vehicle_noise_types_recall.tex", "Recall", "type")
rank_table_fun(vehicle_noise_types, "D:/2 stopień/4 semestr/praca dyplomowa/kody/eksperyment_2/vehicle_noise_types_jaccard.tex", "Jaccard", "type")

noise_level_plot <- function(data, name, metrics = c("Accuracy", "Precision", "Recall")){
  algorithm_labels <- c(
    "CVCF" = "CVCF", "EF" = "EF", "ENG" = "ENG", "PRISM" = "PRISM", "ENN" = "ENN",
    "GE" = "GE", "hybridRepairFilter_remove" = "hRF - remove", 
    "hybridRepairFilter_repair" = "hRF - repair",  "ORBoostFilter" = "ORBF")
  similarity <- c("ENG", "ENN","PRISM", "GE")
  ensemble <- c("CVCF", "EF", "hRF - remove", "hRF - repair", "ORBF")
  
  results <- bind_rows(lapply(data, function(x){bind_rows(x$results)}))
  
  results <- results %>%
    mutate(
      Algorithm_label = recode(Algorithm, !!!algorithm_labels),
      Algorithm_group = case_when(Algorithm_label %in% similarity ~ "Similarity",
                                  Algorithm_label %in% ensemble ~ "Ensemble"),
      Algorithm_group = factor(Algorithm_group, levels = c("Similarity", "Ensemble")),
      Algorithm_label = factor(Algorithm_label, levels = c(similarity, ensemble)))
  
  mean_results <- results %>%
    group_by(Noise_level, Algorithm_label, Algorithm_group) %>%
    summarise(
      across(all_of(metrics), mean, na.rm = TRUE),
      .groups = "drop") %>%
    pivot_longer(cols = all_of(metrics), names_to = "Metric", values_to = "Value") %>%
    mutate(Metric = factor(Metric, levels = metrics))
  
  plot_similarity <- ggplot(filter(mean_results, Algorithm_group == "Similarity"), aes(x = Noise_level, y = Value, color = Algorithm_label, group = Algorithm_label)) +
    geom_line(linewidth = 0.75) + 
    geom_point(size = 1.5) +
    facet_grid(Metric ~ Algorithm_group) +
    coord_cartesian(ylim = c(0, 1)) +
    scale_color_manual(name = "Similarity:", values = c("ENG" = "#008080", "ENN" = "#FF69B4", "PRISM" = "#000080", "GE" = "#FFA500"), guide = guide_legend(nrow = 1, byrow = TRUE)) +
    labs(x = "Poziom szumu", y = NULL) +
    theme_light() +
    theme(
      axis.text.x = element_text(size = 9, colour = "black"), 
      axis.text.y = element_text(size = 9, colour = "black"),
      strip.text = element_text(size = 12, face = "bold", colour = "black"),
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.text = element_text(size = 11),
      legend.title = element_text(size = 12, face = "bold"))
  
  plot_ensemble <- ggplot(filter(mean_results, Algorithm_group == "Ensemble"), aes(x = Noise_level, y = Value, color = Algorithm_label, group = Algorithm_label)) +
    geom_line(linewidth = 0.6) + 
    geom_point(size = 1.5) +
    facet_grid(Metric ~ Algorithm_group) +
    coord_cartesian(ylim = c(0, 1)) +
    scale_color_manual(name = "Ensemble:", values = c("CVCF" = "#008080", "EF" = "#FF69B4", "hRF - remove" = "#000080", "hRF - repair" = "#FFA500", "ORBF" = "#9ACD32"), guide = guide_legend(nrow = 1, byrow = TRUE)) +
    labs(x = "Poziom szumu", y = NULL) +
    theme_light() +
    theme(
      axis.text.x = element_text(size = 9, colour = "black"), 
      axis.text.y = element_text(size = 9, colour = "black"),
      strip.text = element_text(size = 12, face = "bold", colour = "black"),
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.text = element_text(size = 11),
      legend.title = element_text(size = 12, face = "bold"))
  plot <- plot_similarity + plot_ensemble +
    plot_layout(ncol = 2)
  ggsave(name, plot = plot, width = 12, height = 10, units = "in", device = "pdf")}


noise_level_plot(banknote_noise_levels, "D:/2 stopień/4 semestr/praca dyplomowa/kody/eksperyment_2/banknote_noise_levels.pdf")
noise_level_plot(spam_noise_levels, "D:/2 stopień/4 semestr/praca dyplomowa/kody/eksperyment_2/spam_noise_levels.pdf")
noise_level_plot(vehicle_noise_levels, "D:/2 stopień/4 semestr/praca dyplomowa/kody/eksperyment_2/vehicle_noise_levels.pdf")




noise_type_plot <- function(data, name, metrics = c("Accuracy", "Precision", "Recall")){
  algorithm_labels <- c(
    "CVCF" = "CVCF", "EF" = "EF", "ENG" = "ENG", "PRISM" = "PRISM", "ENN" = "ENN",
    "GE" = "GE", "hybridRepairFilter_remove" = "hRF - remove", 
    "hybridRepairFilter_repair" = "hRF - repair",  "ORBoostFilter" = "ORBF")
  similarity <- c("ENG", "ENN","PRISM", "GE")
  ensemble <- c("CVCF", "EF", "hRF - remove", "hRF - repair", "ORBF")
  
  results <- bind_rows(lapply(data, function(x){bind_rows(x$results)}))
  
  results <- results %>%
    mutate(
      Algorithm_label = recode(Algorithm, !!!algorithm_labels),
      Algorithm_group = case_when(Algorithm_label %in% similarity ~ "Similarity",
                                  Algorithm_label %in% ensemble ~ "Ensemble"),
      Algorithm_group = factor(Algorithm_group, levels = c("Similarity", "Ensemble")),
      Algorithm_label = factor(Algorithm_label, levels = c(similarity, ensemble)),
      Noise_type = as.factor(Noise_type))
  
  mean_results <- results %>%
    group_by(Noise_type, Algorithm_label, Algorithm_group) %>%
    summarise(
      across(all_of(metrics), mean, na.rm = TRUE),
      .groups = "drop") %>%
    pivot_longer(cols = all_of(metrics), names_to = "Metric", values_to = "Value") %>%
    mutate(Metric = factor(Metric, levels = metrics))
  
  plot <- ggplot(mean_results, aes(x = Algorithm_label, y = Value, fill = Noise_type)) +
    geom_col(position = position_dodge(width = 0.6), width = 0.5, color = "black", linewidth = 0.2) + 
    facet_grid(Metric ~ Algorithm_group, scales = "free_x", space = "free_x") +
    coord_cartesian(ylim = c(0, 1)) + 
    labs(x = NULL, y = NULL, fill = "Rodzaj szumu:") +
    theme_light() +
    theme(
      axis.text.x = element_text(size = 9, colour = "black", angle = 45, hjust = 1), 
      axis.text.y = element_text(size = 9, colour = "black"),
      strip.text = element_text(size = 12, face = "bold", colour = "black"),
      legend.position = "bottom",
      legend.text = element_text(size = 11),
      legend.title = element_text(size = 12, face = "bold")) +
    scale_fill_manual(values = c("NAR" = "#9ACD32","NCAR" = "#008080", "NNAR" = "#FF69B4"))
  ggsave(name, plot = plot, width = 8, height = 7, units = "in", device = "pdf")}

noise_type_plot(banknote_noise_types, "D:/2 stopień/4 semestr/praca dyplomowa/kody/eksperyment_2/banknote_noise_types.pdf")
noise_type_plot(spam_noise_types, "D:/2 stopień/4 semestr/praca dyplomowa/kody/eksperyment_2/spam_noise_types.pdf")
noise_type_plot(vehicle_noise_types, "D:/2 stopień/4 semestr/praca dyplomowa/kody/eksperyment_2/vehicle_noise_types.pdf")

