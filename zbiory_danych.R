library(mclust)
dane_banknoty <- banknote


library(kernlab)
library(caret)
set.seed(12345)
dane_spam_all <- spam
wybrane_indeksy <- createDataPartition(y = spam$type, p = 0.1, list = FALSE)
dane_spam <- dane_spam_all[wybrane_indeksy, ]

library(mlbench)
dane_vehicle <- Vehicle