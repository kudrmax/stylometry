###########################################################
# 0. Устанавливаем и подключаем нужные пакеты
###########################################################

# Выполнить ОДИН РАЗ (если пакеты ещё не установлены):
# install.packages("quanteda")
# install.packages("readr")
# install.packages("stringdist")

library(quanteda)    # для текстового анализа
library(readr)       # для чтения txt-файлов
library(stringdist)  # для "fuzzy" расстояний между строками

###########################################################
# 1. Читаем два текста из файлов
###########################################################

# ВАЖНО: файлы "онегин.txt" и "набоков.txt" должны лежать
# в рабочей директории R-проекта
text_onegin  <- read_file("онегин.txt")
text_nabokov <- read_file("набоков.txt")

# Можно проверить, что тексты разные:
identical(text_onegin, text_nabokov)  # должно быть FALSE

###########################################################
# 2. Собираем их в один объект и токенизируем (разбиваем на слова)
###########################################################

texts <- c(Onegin = text_onegin,
           Nabokov = text_nabokov)

# Разбиваем на слова, убираем пунктуацию и цифры
toks <- tokens(
  texts,
  remove_punct   = TRUE,
  remove_numbers = TRUE
)

# Приводим всё к нижнему регистру
toks <- tokens_tolower(toks)

###########################################################
# 3. Строим N-граммы.
#    Здесь возьмём биграммы (2 слова) как "словосочетания"
###########################################################

# Можешь поменять n = 3 для триграмм, если захочешь.
toks_2 <- tokens_ngrams(toks, n = 2)

# Посмотрим для интереса первые биграммы:
toks_2
# Здесь ты увидишь отдельные биграммы для Onegin и Nabokov

###########################################################
# 4. Строим матрицу частот биграмм по каждому тексту
###########################################################

dfm_2 <- dfm(toks_2)
dfm_2

# Разделим на два объекта: отдельно для Онегина и для Набокова
dfm_onegin  <- dfm_2["Onegin", ]
dfm_nabokov <- dfm_2["Nabokov", ]

###########################################################
# 5. Считаем частоты биграмм и выбираем самые частые
###########################################################

# Вектор частот биграмм (сколько раз каждая биграмма встречается)
freq_onegin  <- colSums(dfm_onegin)
freq_nabokov <- colSums(dfm_nabokov)

# Отсортируем по убыванию частоты
freq_onegin_sorted  <- sort(freq_onegin,  decreasing = TRUE)
freq_nabokov_sorted <- sort(freq_nabokov, decreasing = TRUE)

# Выберем, например, топ-200 биграмм в каждом тексте
# (можно сделать 100, 300 и т.д., в зависимости от скорости)
top_n <- 500

cand_onegin_all  <- names(freq_onegin_sorted)[ 1:min(top_n, length(freq_onegin_sorted)) ]
cand_nabokov_all <- names(freq_nabokov_sorted)[1:min(top_n, length(freq_nabokov_sorted)) ]

length(cand_onegin_all)
length(cand_onegin_all)

stop_ru <- stopwords("ru")


is_useful_ngram <- function(ngram) {
  parts <- strsplit(ngram, "_")[[1]]
  # не все слова — стоп-слова
  not_all_stop <- !all(parts %in% stop_ru)
  # есть хотя бы одно слово длиной > 2
  has_long <- any(nchar(parts) > 2)
  not_all_stop && has_long
}

cand_onegin  <- cand_onegin_all[  sapply(cand_onegin_all,  is_useful_ngram) ]
cand_nabokov <- cand_nabokov_all[ sapply(cand_nabokov_all, is_useful_ngram) ]

###########################################################
# 6. Считаем "fuzzy" расстояния между биграммами двух текстов
#    Используем расстояние Левенштейна (сколько правок надо,
#    чтобы превратить одну строку в другую).
###########################################################

# Матрица расстояний: строки - пушкинские биграммы,
# столбцы - набоковские биграммы
dist_mat <- stringdistmatrix(cand_onegin, cand_nabokov, method = "lv")

# Посмотрим размер матрицы
dim(dist_mat)  # примерно 200 x 200

###########################################################
# 7. Находим пары биграмм, которые "похожи"
#    Например, расстояние Левенштейна <= 2
###########################################################

# Ищем индексы пар с небольшим расстоянием
threshold <- 2  # порог "похожести" (можешь поиграть: 1, 2, 3)

idx <- which(dist_mat <= threshold, arr.ind = TRUE)

# Если таких пар нет, idx будет пустым
nrow(idx)

# Собираем таблицу с похожими парами
onegin_ngrams_sim  <- cand_onegin[ idx[, 1] ]
nabokov_ngrams_sim <- cand_nabokov[ idx[, 2] ]
dist_values        <- dist_mat[ idx ]

###########################################################
# 8. Делаем аккуратный data.frame с результатами
###########################################################

# Добавим частоты каждой биграммы в своём тексте
freq_onegin_top  <- freq_onegin[ cand_onegin ]
freq_nabokov_top <- freq_nabokov[ cand_nabokov ]

results <- data.frame(
  onegin_bigram   = onegin_ngrams_sim,
  nabokov_bigram  = nabokov_ngrams_sim,
  distance_lv     = dist_values,
  freq_onegin     = freq_onegin_top[ onegin_ngrams_sim ],
  freq_nabokov    = freq_nabokov_top[ nabokov_ngrams_sim ],
  stringsAsFactors = FALSE
)

# Уберём точные совпадения (если хотим только "почти" похожие,
# а не полностью одинаковые биграммы)
results <- subset(results, distance_lv > 0)

# Отсортируем: сначала самые похожие, потом самые частые
results <- results[order(results$distance_lv,
                         -(results$freq_onegin + results$freq_nabokov)), ]

###########################################################
# 9. Смотрим верхушку списка
###########################################################

results
