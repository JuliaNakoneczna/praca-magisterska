library(mclust)
library(kernlab)
library(mlbench)
library(caret)
set.seed(2026)

data(banknote)
banknote_no_standarize <- banknote
banknote_preprocess <- preProcess(banknote %>% select(-Status), method = c("center", "scale"))
banknote_standarize <- predict(banknote_preprocess, banknote)
save(list = "banknote_standarize", file = "banknote_standarize.Rdata")

data(spam)
wybrane_indeksy <- createDataPartition(y = spam$type, p = 0.1, list = FALSE)
spam_subset <- spam[wybrane_indeksy, ]
spam_no_standarize <- spam_subset
spam_log <- spam_subset %>% mutate(across(-type, ~ log(.x + 0.1)))
spam_preprocess <- preProcess(spam_log %>% select(-type), method = c("center", "scale"))
spam_standarize <- predict(spam_preprocess, spam_log)
save(list = "spam_standarize", file = "spam_standarize.Rdata")

data(Vehicle)
vehicle_no_standarize <- Vehicle
vehicle_preprocess <- preProcess(Vehicle %>% select(-Class), method = c("center", "scale"))
vehicle_standarize <- predict(vehicle_preprocess, Vehicle)
save(list = "vehicle_standarize", file = "vehicle_standarize.Rdata")