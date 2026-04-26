library(dplyr)
library(tidyr)
library(xtable)
library(ggplot2)

boxplot_fun <- function(data, name){
  results <- bind_rows(data$results)
  select_and_correct <- c("hybridRepairFilter_repair", "hybridRepairFilter_hybrid", "EWF", "GE")
  results <- results %>%
    mutate(
      Algorithm_Group = if_else(
        Algorithm %in% select_and_correct,
        "Select-and-Correct",
        "Sample Selection"))
  
  results$Algorithm <- factor(results$Algorithm, levels = rev(sort(unique(results$Algorithm))))
  results <- results %>%
    select(Algorithm, Algorithm_Group, Accuracy, Precision, Recall, Jaccard) %>%
    pivot_longer(
      cols = c(Accuracy, Precision, Recall, Jaccard),
      names_to = "Metric",
      values_to = "Value")
  results$Metric <- factor(results$Metric, levels = c("Accuracy", "Precision", "Recall", "Jaccard"))
  plot <- ggplot(results, aes(x = Algorithm, y = Value, fill = Algorithm_Group)) +
    geom_boxplot(outlier.size = 0.5, outlier.alpha = 1, alpha = 0.7, linewidth = 0.5) +
    facet_wrap(~ Metric, scales = "free_x", nrow = 1) +
    theme_light() +
    coord_flip() + 
    labs(x = NULL, y = NULL, fill = "Metoda:") +
    theme(
      axis.text.x = element_text(size = 9, colour = "black"), 
      axis.text.y = element_text(size = 9, colour = "black"),
      legend.position = "bottom",
      legend.title = element_text(face = "bold")) +
    scale_fill_manual(values = c("Sample Selection" = "#4682B4", "Select-and-Correct" = "#D8BFD8"))
  ggsave(name, plot = plot, width = 12, height = 6, units = "in", device = "pdf")}

boxplot_fun(banknote_no_standarize_0.2_NCAR, "D:/2 stopień/4 semestr/praca dyplomowa/kody/eksperyment_1/banknote_no_standarize.pdf")
boxplot_fun(banknote_standarize_0.2_NCAR, "D:/2 stopień/4 semestr/praca dyplomowa/kody/eksperyment_1/banknote_standarize.pdf")
boxplot_fun(spam_no_standarize_0.2_NCAR, "D:/2 stopień/4 semestr/praca dyplomowa/kody/eksperyment_1/spam_no_standarize.pdf")
boxplot_fun(spam_standarize_0.2_NCAR, "D:/2 stopień/4 semestr/praca dyplomowa/kody/eksperyment_1/spam_standarize.pdf")
boxplot_fun(vehicle_no_standarize_0.2_NCAR, "D:/2 stopień/4 semestr/praca dyplomowa/kody/eksperyment_1/vehicle_no_standarize.pdf")
boxplot_fun(vehicle_standarize_0.2_NCAR, "D:/2 stopień/4 semestr/praca dyplomowa/kody/eksperyment_1/vehicle_standarize.pdf")



table_fun <- function(data, name){
  
  algorithm_labels <- c(
    "AENN" = "AENN", "BBNR" = "BBNR", "C45iteratedVotingFilter" = "C45iVF", "C45robustFilter" = "C45rF",
    "CVCF" = "CVCF", "dynamicCF" = "dCF", "EF" = "EF", "ENG" = "ENG", "ENN" = "ENN",
    "EWF" = "EWF", "GE" = "GE", "HARF" = "HARF", "hybridRepairFilter_hybrid" = "hRF - hybrid", 
    "hybridRepairFilter_repair" = "hRF - repair", "IPF" = "IPF", "ORBoostFilter" = "ORBF")
  results <- bind_rows(data$results)
  results <- results %>%
    mutate(
      Algorithm_label = recode(Algorithm, !!!algorithm_labels),
      predicted_true_ratio = Predicted_noise_number / True_noise_number)
  mean_pm_sd <- function(x){
    sprintf(paste0("%.3f $\\pm$ %.3f"),
            mean(x, na.rm = TRUE),
            sd(x, na.rm = TRUE))}
  table <- results %>%
    group_by(Algorithm_label) %>%
    summarise(
      Accuracy = mean_pm_sd(Accuracy),
      Precision = mean_pm_sd(Precision),
      Recall = mean_pm_sd(Recall),
      Jaccard = mean_pm_sd(Jaccard),
      Time = mean_pm_sd(Time),
      'Detection ratio' = mean_pm_sd(predicted_true_ratio),
      .groups = "drop") %>%
    arrange(tolower(Algorithm_label)) %>%
    rename(Algorithm = Algorithm_label)
  table_latex <- xtable(table, caption = NULL, label = NULL, align = c("l", "l", "c", "c", "c", "c", "c", "c"))
  print(table_latex, type = "latex", file = name, include.rownames = FALSE, sanitize.text.function = identity, floating = FALSE)}

table_fun(banknote_standarize_0.2_NCAR, "D:/2 stopień/4 semestr/praca dyplomowa/kody/eksperyment_1/banknote_standarize.tex")
table_fun(spam_standarize_0.2_NCAR, "D:/2 stopień/4 semestr/praca dyplomowa/kody/eksperyment_1/spam_standarize.tex")
table_fun(vehicle_standarize_0.2_NCAR, "D:/2 stopień/4 semestr/praca dyplomowa/kody/eksperyment_1/vehicle_standarize.tex")
