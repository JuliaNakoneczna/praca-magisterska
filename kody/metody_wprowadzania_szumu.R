library(noisemodel)
library(randomForest)
library(mclust)

dane_banknoty <- banknote
Y <- dane_banknoty[ ,1]
X <- dane_banknoty[, 2:6]
# NCAR (Noisy Completely at Random) - szum symetryczny
# Funkcja sym_uni_ln wybiera losowo (level * 100%) obserwacji (niezależnie od ich klasy), które są uznawane za szum.
# Etykiety wybranych obserwacji są losowo zmieniane na inne etykiety ze zbioru danych.

szum_NCAR <- sym_uni_ln(x = X, y = Y, level = 0.2)
szum_NCAR$idnoise


# NAR (Noisy at Random) - szum asymetryczny
# Funkcja asy_uni_ln wybiera dla każdej klasy (level * 100%) obserwacji, które są uznawane za szum.
# Etykiety wybranych obserwacji są zmieniane zgodnie z poziomem szumu określonym dla poszczególnych klas.
# Przed wywołaniem funkcji konieczne jest wyznaczenie poziomu szumu dla każdej z klas w zbiorze danych.
# Poziomy szumu dla poszczególnych klas wyznaczymy na podstawie błędów klasyfikacji modelu Las losowy.
# Wybór modelu Las losowy pozwala na automatyczne oszacowanie błędów klasyfikacji za pomocą metody out-of-bag. 
# W celu uzyskania zadanego poziomu szumu wektor błędów zostanie odpowiednio przeskalowany.

wektor_szumu_NAR <- function(x, y, noise_level, epsilon = 1e-4){
   y <- as.factor(y)
   model <- randomForest(x = x, y = y, ntree = 100)
   bledy_klasy <- model$confusion[, "class.error"] + epsilon
   proporcja_klasy <- table(y) / length(y)
   wsp_skalujacy <- noise_level / sum(bledy_klasy * proporcja_klasy)
   poziomy_szumu <- pmin(bledy_klasy * wsp_skalujacy, 1)
   return(poziomy_szumu)}
 
szum_NAR <- asy_uni_ln(x = X, y = Y, level = wektor_szumu_NAR(x = X, y = Y, noise_level = 0.2))
szum_NAR$idnoise

# NNAR (Noisy Not at Random) - szum zależny od cech
# Funkcja nei_bor_ln wprowadza szum etykiet w zależności od położenia obserwacji względem granicy.
# Dla każdej obserwacji obliczany jest stosunek odległości do najbliższego sąsiada z tej samej klasy oraz odległości do najbliższego sąsiada z innej klasy.
# Następnie wartości te są sortowane malejąco, a pierwszy (level * 100%) obserwacji jest uznawany za szum.
# Dla obserwacji uznanych za szum, nowa etykieta wybierana jest klasa dominująca wśród k najbliższych sąsiadów należących do innych klas (domyślnie k = 1).

szum_NNAR <- nei_bor_ln(x = X, y = Y, level = 0.2)
szum_NNAR$idnoise