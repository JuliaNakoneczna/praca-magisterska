library(dplyr)
library(tidyr)
library(xtable)
library(ggplot2)
library(patchwork)

algorithms <- list(
  list(algorithm = ENG, name = "ENG", par = list()),
  list(algorithm = GE, name = "GE", par = list()),
  list(algorithm = hybridRepairFilter, name = "hybridRepairFilter_repair", par = list(noiseAction = "repair")),
  list(algorithm = EF, name = "EF", par = list(nfolds = 8)))

classifiers_knn <- list(
  list(classifier = "knn", name = "knn1", par = list(k = 1)),
  list(classifier = "knn", name = "knn3",  par = list(k = 3)),
  list(classifier = "knn", name = "knn5",  par = list(k = 5)),
  list(classifier = "knn", name = "knn7", par = list(k = 7)),
  list(classifier = "knn", name = "knn9", par = list(k = 9)))

classifiers_ALL <- list(
  list(classifier = "tree", name = "drzewo decyzyjne", par = list()),
  list(classifier = "knn", name = "knn5", par = list(k = 5)),
  list(classifier = "logregression", name = "regresja logistyczna", par = list()))

# Metoda k-nn dla różnych wartości parametru k

banknote_knn <- lapply(classifiers_knn, function(clasif){
  results <- lapply(algorithms, function(alg){
    experiment_3(data = banknote_standarize, data_name = "banknote_standarize", label = "Status", algorithm = alg$algorithm, algorithm_name = alg$name, algorithm_par = alg$par, 
                 classifier = clasif$classifier, classifier_par = clasif$par, classifier_name = clasif$name)})
  list(algorithms = algorithms, classifier = clasif, results = results)})
save(banknote_knn, file = file.path("praca_magisterska_wyniki_eksperyment3/zbiorcze", "banknote_knn.RData"))

spam_knn <- lapply(classifiers_knn, function(clasif){
  results <- lapply(algorithms, function(alg){
    experiment_3(data = spam_standarize, data_name = "spam_standarize", label = "type", algorithm = alg$algorithm, algorithm_name = alg$name, algorithm_par = alg$par, 
                 classifier = clasif$classifier, classifier_par = clasif$par, classifier_name = clasif$name)})
  list(algorithms = algorithms, classifier = clasif, results = results)})
save(spam_knn, file = file.path("praca_magisterska_wyniki_eksperyment3/zbiorcze", "spam_knn.RData"))

vehicle_knn <- lapply(classifiers_knn, function(clasif){
  results <- lapply(algorithms, function(alg){
    experiment_3(data = vehicle_standarize, data_name = "vehicle_standarize", label = "Class", algorithm = alg$algorithm, algorithm_name = alg$name, algorithm_par = alg$par, 
                 classifier = clasif$classifier, classifier_par = clasif$par, classifier_name = clasif$name)})
  list(algorithms = algorithms, classifier = clasif, results = results)})
save(vehicle_knn, file = file.path("praca_magisterska_wyniki_eksperyment3/zbiorcze", "vehicle_knn.RData"))

knn_plot <- function(data, name, metrics = c("Accuracy", "Precision", "Recall", "F1_score")){
  algorithm_labels <- c(
    "EF" = "EF", "ENG" = "ENG", 
    "GE" = "GE", "hybridRepairFilter_repair" = "hRF - repair")

  results <- bind_rows(lapply(data, function(x) x$results))
  
  results <- results %>%
    mutate(
      Algorithm_label = recode(Algorithm, !!!algorithm_labels),
      Group = case_when(Variant == "Original" ~ "Original", Variant == "Noise" ~ "Noise",
                        Variant == "Clean" ~ as.character(Algorithm_label)))

  mean_results <- results %>%
    group_by(Classifier, Group) %>%
    summarise(
      across(all_of(metrics), mean, na.rm = TRUE),
      .groups = "drop") %>%
    pivot_longer(cols = all_of(metrics), names_to = "Metric", values_to = "Value") %>%
    mutate(Classifier = factor(Classifier, levels = c("knn1", "knn3", "knn5", "knn7", "knn9")),
           Group = factor(Group, levels = c("Original", "Noise", setdiff(unique(Group), c("Original", "Noise")))))
  
  plot <- ggplot(mean_results, aes(x = Classifier, y = Value, color = Group, group = Group)) +
    geom_line(aes(linewidth = Group)) + 
    geom_point(size = 1.5) +
    facet_grid(Metric~.) +
    coord_cartesian(ylim = c(0.5, 0.8)) +
    scale_color_manual(name = "Wariant:", values = c("Original" = "#9ACD32","Noise" = "#FF0000" ,"ENG" = "#008080", "EF" = "#FF69B4", "hRF - repair" = "#000080", "GE" = "#FFA500")) +
    scale_linewidth_manual(name = "Wariant:", values = c("Original" = 0.9,"Noise" = 0.9,"ENG" = 0.7, "EF" = 0.7, "hRF - repair" = 0.7, "GE" = 0.7)) +
    labs(x = "Klasyfikator knn w zależności od liczby sąsiadów", y = NULL) +
    theme_light() +
    theme(
      axis.text.x = element_text(size = 9, colour = "black"), 
      axis.text.y = element_text(size = 9, colour = "black"),
      strip.text = element_text(size = 12, face = "bold", colour = "black"),
      legend.position = "bottom",
      legend.text = element_text(size = 11),
      legend.title = element_text(size = 12, face = "bold"))
  ggsave(name, plot = plot, width = 12, height = 10, units = "in", device = "pdf")}


knn_plot(banknote_knn, "D:/2 stopień/4 semestr/praca dyplomowa/kody/eksperyment_3/banknote_knn.pdf")
knn_plot(spam_knn, "D:/2 stopień/4 semestr/praca dyplomowa/kody/eksperyment_3/spam_knn.pdf")
knn_plot(vehicle_knn, "D:/2 stopień/4 semestr/praca dyplomowa/kody/eksperyment_3/vehicle_knn.pdf")

# Metoda k-nn (k=5), drzewo decyzyjne, regresja logistyczna

# szum NCAR

banknote_classifiers_ALL <- lapply(classifiers_ALL, function(clasif){
  results <- lapply(algorithms, function(alg){
    experiment_3(data = banknote_standarize, data_name = "banknote_standarize", label = "Status", algorithm = alg$algorithm, algorithm_name = alg$name, algorithm_par = alg$par, 
                 classifier = clasif$classifier, classifier_par = clasif$par, classifier_name = clasif$name)})
  list(algorithms = algorithms, classifier = clasif, results = results)})
save(banknote_classifiers_ALL, file = file.path("praca_magisterska_wyniki_eksperyment3/zbiorcze", "banknote_classifiers_ALL.RData"))

spam_classifiers_ALL <- lapply(classifiers_ALL, function(clasif){
  results <- lapply(algorithms, function(alg){
    experiment_3(data = spam_standarize, data_name = "spam_standarize", label = "type", algorithm = alg$algorithm, algorithm_name = alg$name, algorithm_par = alg$par, 
                 classifier = clasif$classifier, classifier_par = clasif$par, classifier_name = clasif$name)})
  list(algorithms = algorithms, classifier = clasif, results = results)})
save(spam_classifiers_ALL, file = file.path("praca_magisterska_wyniki_eksperyment3/zbiorcze", "spam_classifiers_ALL.RData"))

vehicle_classifiers_ALL <- lapply(classifiers_ALL, function(clasif){
  results <- lapply(algorithms, function(alg){
    experiment_3(data = vehicle_standarize, data_name = "vehicle_standarize", label = "Class", algorithm = alg$algorithm, algorithm_name = alg$name, algorithm_par = alg$par,
                 classifier = clasif$classifier, classifier_par = clasif$par, classifier_name = clasif$name)})
  list(algorithms = algorithms, classifier = clasif, results = results)})
save(vehicle_classifiers_ALL, file = file.path("praca_magisterska_wyniki_eksperyment3/zbiorcze", "vehicle_classifiers_ALL.RData"))

# szum NAR

banknote_classifiers_ALL_NAR <- lapply(classifiers_ALL, function(clasif){
  results <- lapply(algorithms, function(alg){
    experiment_3(data = banknote_standarize, data_name = "banknote_standarize", label = "Status", algorithm = alg$algorithm, algorithm_name = alg$name, algorithm_par = alg$par, noise_type = "NAR", 
                 classifier = clasif$classifier, classifier_par = clasif$par, classifier_name = clasif$name)})
  list(algorithms = algorithms, classifier = clasif, results = results)})
save(banknote_classifiers_ALL_NAR, file = file.path("praca_magisterska_wyniki_eksperyment3/zbiorcze", "banknote_classifiers_ALL_NAR.RData"))

spam_classifiers_ALL_NAR <- lapply(classifiers_ALL, function(clasif){
  results <- lapply(algorithms, function(alg){
    experiment_3(data = spam_standarize, data_name = "spam_standarize", label = "type", algorithm = alg$algorithm, algorithm_name = alg$name, algorithm_par = alg$par, noise_type = "NAR", 
                 classifier = clasif$classifier, classifier_par = clasif$par, classifier_name = clasif$name)})
  list(algorithms = algorithms, classifier = clasif, results = results)})
save(spam_classifiers_ALL_NAR, file = file.path("praca_magisterska_wyniki_eksperyment3/zbiorcze", "spam_classifiers_ALL_NAR.RData"))

vehicle_classifiers_ALL_NAR <- lapply(classifiers_ALL, function(clasif){
  results <- lapply(algorithms, function(alg){
    experiment_3(data = vehicle_standarize, data_name = "vehicle_standarize", label = "Class", algorithm = alg$algorithm, algorithm_name = alg$name, algorithm_par = alg$par, noise_type = "NAR",
                 classifier = clasif$classifier, classifier_par = clasif$par, classifier_name = clasif$name)})
  list(algorithms = algorithms, classifier = clasif, results = results)})
save(vehicle_classifiers_ALL_NAR, file = file.path("praca_magisterska_wyniki_eksperyment3/zbiorcze", "vehicle_classifiers_ALL_NAR.RData"))



classifier_boxplot <- function(data, name, metrics = c("Accuracy", "Precision", "Recall", "F1_score")){
  algorithm_labels <- c(
    "EF" = "EF", "ENG" = "ENG", 
    "GE" = "GE", "hybridRepairFilter_repair" = "hRF - repair")

  
  results <- bind_rows(data$results)
    
  results <- results %>%
    mutate(
      Algorithm_label = recode(Algorithm, !!!algorithm_labels),
      Group = case_when(Variant == "Original" ~ "Original", Variant == "Noise" ~ "Noise",
                        Variant == "Clean" ~ as.character(Algorithm_label))) %>%
    distinct(Group, Accuracy, Precision, Recall, F1_score, .keep_all = TRUE) %>%
    mutate(Group = factor(Group, levels = c("Original", "Noise", setdiff(unique(Group), c("Original", "Noise"))))) %>%
    select(Group, Accuracy, Precision, Recall, F1_score) %>%
    pivot_longer(
      cols = c(Accuracy, Precision, Recall, F1_score),
      names_to = "Metric",
      values_to = "Value")
    results$Metric <- factor(results$Metric, levels = c("Accuracy", "Precision", "Recall", "F1_score"))
  
  plot <- ggplot(results, aes(x = Group, y = Value, fill = Group)) +
    geom_boxplot(outlier.size = 0.8, outlier.alpha = 1, alpha = 0.7, linewidth = 0.5) +
    facet_grid(Metric~.) +
    coord_cartesian(ylim = c(0.55, 0.88)) +
    theme_light() +
    labs(x = NULL, y = NULL, fill = "Wariant:") +
    theme(
      axis.text.x = element_text(size = 11, colour = "black", angle = 45, hjust = 1), 
      axis.text.y = element_text(size = 11, colour = "black"),
      strip.text = element_text(size = 12, face = "bold", colour = "black"),
      legend.position = "bottom",
      legend.text = element_text(size = 11),
      legend.title = element_text(size = 12, face = "bold")) +
    scale_fill_manual(values = c("Original" = "#9ACD32","Noise" = "#FF0000" ,"ENG" = "#008080", "EF" = "#FF69B4", "hRF - repair" = "#000080", "GE" = "#FFA500"))
  ggsave(name, plot = plot, width = 7, height = 7, units = "in", device = "pdf")}

classifier_boxplot(banknote_classifiers_ALL[[1]], "D:/2 stopień/4 semestr/praca dyplomowa/kody/eksperyment_3/banknote_ALL_tree.pdf")
classifier_boxplot(banknote_classifiers_ALL[[2]], "D:/2 stopień/4 semestr/praca dyplomowa/kody/eksperyment_3/banknote_ALL_knn.pdf")
classifier_boxplot(banknote_classifiers_ALL[[3]], "D:/2 stopień/4 semestr/praca dyplomowa/kody/eksperyment_3/banknote_ALL_reg.pdf")

classifier_boxplot(spam_classifiers_ALL[[1]], "D:/2 stopień/4 semestr/praca dyplomowa/kody/eksperyment_3/spam_ALL_tree.pdf")
classifier_boxplot(spam_classifiers_ALL[[2]], "D:/2 stopień/4 semestr/praca dyplomowa/kody/eksperyment_3/spam_ALL_knn.pdf")
classifier_boxplot(spam_classifiers_ALL[[3]], "D:/2 stopień/4 semestr/praca dyplomowa/kody/eksperyment_3/spam_ALL_reg.pdf")

classifier_boxplot(vehicle_classifiers_ALL[[1]], "D:/2 stopień/4 semestr/praca dyplomowa/kody/eksperyment_3/vehicle_ALL_tree.pdf")
classifier_boxplot(vehicle_classifiers_ALL[[2]], "D:/2 stopień/4 semestr/praca dyplomowa/kody/eksperyment_3/vehicle_ALL_knn.pdf")
classifier_boxplot(vehicle_classifiers_ALL[[3]], "D:/2 stopień/4 semestr/praca dyplomowa/kody/eksperyment_3/vehicle_ALL_reg.pdf")




classifier_barplot <- function(data, name, metrics = c("Accuracy", "Precision", "Recall", "F1_score")){
  algorithm_labels <- c(
    "EF" = "EF", "ENG" = "ENG", 
    "GE" = "GE", "hybridRepairFilter_repair" = "hRF - repair")
  
  results <- bind_rows(lapply(data, function(x) x$results))
  
  results <- results %>%
    mutate(
      Algorithm_label = recode(Algorithm, !!!algorithm_labels),

      Group = case_when(Variant == "Original" ~ "Original", Variant == "Noise" ~ "Noise",
                        Variant == "Clean" ~ as.character(Algorithm_label))) %>%
    distinct(Classifier, Group, Accuracy, Precision, Recall, F1_score, .keep_all = TRUE) %>%
    group_by(Classifier, Group) %>%
    summarise(across(all_of(metrics), mean, na.rm = TRUE),
              .groups = "drop") %>%
    pivot_longer(cols = all_of(metrics), names_to = "Metric", values_to = "Value") %>%
    mutate(Group = factor(Group, levels = c("Original", "Noise", setdiff(unique(Group), c("Original", "Noise")))),
           Metric = factor(Metric, levels = metrics),
           Classifier = factor(Classifier, levels = c("drzewo decyzyjne", "knn5", "regresja logistyczna"), 
                               labels = c("Drzewo decyzyjne", "5-NN", "Regresja logistyczna")))
      

  plot <- ggplot(results, aes(x = Group, y = Value, fill = Group)) +
    geom_col(position = position_dodge(width = 0.6), width = 0.5, color = "black", linewidth = 0.2) + 
    facet_grid(Metric ~ Classifier) +
    coord_cartesian(ylim = c(0.5, 1)) + 
    labs(x = NULL, y = NULL, fill = "Wariant:") +
    theme_light() +
    theme(
      axis.text.x = element_text(size = 9, colour = "black", angle = 45, hjust = 1), 
      axis.text.y = element_text(size = 9, colour = "black"),
      strip.text = element_text(size = 12, face = "bold", colour = "black"),
      legend.position = "bottom",
      legend.text = element_text(size = 11),
      legend.title = element_text(size = 12, face = "bold")) +
    scale_fill_manual(values = c("Original" = "#9ACD32","Noise" = "#FF0000" ,"ENG" = "#008080", "EF" = "#FF69B4", "hRF - repair" = "#000080", "GE" = "#FFA500"))
  ggsave(name, plot = plot, width = 8, height = 10, units = "in", device = "pdf")}

classifier_barplot(banknote_classifiers_ALL, "D:/2 stopień/4 semestr/praca dyplomowa/kody/eksperyment_3/banknote_classifiers_ALL.pdf")
classifier_barplot(banknote_classifiers_ALL_NAR, "D:/2 stopień/4 semestr/praca dyplomowa/kody/eksperyment_3/banknote_classifiers_ALL_NAR.pdf")
classifier_barplot(spam_classifiers_ALL, "D:/2 stopień/4 semestr/praca dyplomowa/kody/eksperyment_3/spam_classifiers_ALL.pdf")
classifier_barplot(spam_classifiers_ALL_NAR, "D:/2 stopień/4 semestr/praca dyplomowa/kody/eksperyment_3/spam_classifiers_ALL_NAR.pdf")
classifier_barplot(vehicle_classifiers_ALL, "D:/2 stopień/4 semestr/praca dyplomowa/kody/eksperyment_3/vehicle_classifiers_ALL.pdf")
classifier_barplot(vehicle_classifiers_ALL_NAR, "D:/2 stopień/4 semestr/praca dyplomowa/kody/eksperyment_3/vehicle_classifiers_ALL_NAR.pdf")


classifier_table_fun <- function(data, name){
  algorithm_labels <- c(
    "EF" = "EF", "ENG" = "ENG", 
    "GE" = "GE", "hybridRepairFilter_repair" = "hRF - repair")
  
  results <- bind_rows(lapply(data, function(x) x$results))
  
  results <- results %>%
    mutate(
      Algorithm_label = recode(Algorithm, !!!algorithm_labels),
      Variant = case_when(Variant == "Original" ~ "Original", Variant == "Noise" ~ "Noise",
                        Variant == "Clean" ~ as.character(Algorithm_label)),
      Classifier = factor(Classifier, levels = c("drzewo decyzyjne", "knn5", "regresja logistyczna"), 
                          labels = c("Drzewo decyzyjne", "5-NN", "Regresja logistyczna")))
  
  mean_sd <- results %>%
    group_by(Classifier, Variant) %>%
    summarise(
      Accuracy_mean = mean(Accuracy, na.rm = TRUE), Accuracy_sd = sd(Accuracy, na.rm = TRUE),
      Precision_mean = mean(Precision, na.rm = TRUE), Precision_sd = sd(Precision, na.rm = TRUE),
      Recall_mean = mean(Recall, na.rm = TRUE), Recall_sd = sd(Recall, na.rm = TRUE),
      F1_score_mean = mean(F1_score, na.rm = TRUE), F1_score_sd = sd(F1_score, na.rm = TRUE),
      .groups = "drop")
  
  no_zero <- function(value){
    text <- sprintf("%.3f", value)
    ifelse(abs(value) < 1, sub("0\\.", ".", text), text)}
  
  mean_pm_sd <- function(mean_value, sd_value, best_value, Variant){
    text <- paste0(sprintf("%.3f", mean_value), " $\\pm$ ", no_zero(sd_value))
    case_when(
      is.na(mean_value) ~ text,
      Variant == "Original" ~ paste0("\\textit{", text, "}"),
      abs(mean_value - best_value) < 1e-7 ~ paste0("\\textbf{", text, "}"),
      TRUE ~ text)}
  
  table <- mean_sd %>%
    group_by(Classifier) %>%
    mutate(
      best_Accuracy = max(Accuracy_mean[!Variant %in% c("Original", "Noise")], na.rm = TRUE),
      best_Precision = max(Precision_mean[!Variant %in% c("Original", "Noise")], na.rm = TRUE),
      best_Recall = max(Recall_mean[!Variant %in% c("Original", "Noise")], na.rm = TRUE),
      best_F1_score = max(F1_score_mean[!Variant %in% c("Original", "Noise")], na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(
      Accuracy = mean_pm_sd(Accuracy_mean, Accuracy_sd, best_Accuracy, Variant),
      Precision = mean_pm_sd(Precision_mean, Precision_sd, best_Precision, Variant),
      Recall = mean_pm_sd(Recall_mean, Recall_sd, best_Recall, Variant),
      'F1-score' = mean_pm_sd(F1_score_mean, F1_score_sd, best_F1_score, Variant),
      Wariant = factor(Variant, levels = c("Original", "Noise", "EF", "ENG", "GE", "hRF - repair"))) %>%
    arrange(Classifier, Wariant)
      
  add_claffisier_names <- list(pos = list(-1, 0, 6, 12, 18), 
                               command = c("\\hline\n",
                                        "\\hline\n\\multicolumn{5}{l}{\\textbf{Drzewo decyzyjne}} \\\\\n\\hline\n",
                                        "\\hline\n\\multicolumn{5}{l}{\\textbf{k-najbliższych sąsiadów (k = 5)}} \\\\\n\\hline\n",
                                        "\\hline\n\\multicolumn{5}{l}{\\textbf{Regresja logistyczna}} \\\\\n\\hline\n",
                                        "\\hline\n"))
      
  table <- table %>% 
    select(Wariant, Accuracy, Precision, Recall, 'F1-score')
  table_latex <- xtable(table, caption = NULL, label = NULL, align = c("l", "l", "c", "c", "c", "c"))
  print(table_latex, type = "latex", file = name, include.rownames = FALSE, sanitize.text.function = identity, floating = FALSE,
        add.to.row = add_claffisier_names, hline.after = NULL)}


classifier_table_fun(banknote_classifiers_ALL, "D:/2 stopień/4 semestr/praca dyplomowa/kody/eksperyment_3/banknote_classifier_ALL.tex")
classifier_table_fun(banknote_classifiers_ALL_NAR, "D:/2 stopień/4 semestr/praca dyplomowa/kody/eksperyment_3/banknote_classifier_ALL_NAR.tex")
classifier_table_fun(spam_classifiers_ALL, "D:/2 stopień/4 semestr/praca dyplomowa/kody/eksperyment_3/spam_classifier_ALL.tex")
classifier_table_fun(spam_classifiers_ALL_NAR, "D:/2 stopień/4 semestr/praca dyplomowa/kody/eksperyment_3/spam_classifier_ALL_NAR.tex")
classifier_table_fun(vehicle_classifiers_ALL, "D:/2 stopień/4 semestr/praca dyplomowa/kody/eksperyment_3/vehicle_classifier_ALL.tex")
classifier_table_fun(vehicle_classifiers_ALL_NAR, "D:/2 stopień/4 semestr/praca dyplomowa/kody/eksperyment_3/vehicle_classifier_ALL_NAR.tex")