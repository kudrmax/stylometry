###########################################################
# 0. Пакеты
###########################################################

# install.packages("readr")
# install.packages("stringr")
# install.packages("dplyr")
# install.packages("reticulate")

library(readr)
library(stringr)
library(dplyr)
library(reticulate)

###########################################################
# 1. Настройка Python и модели sentence-transformers
###########################################################

# Если нужно указать путь к конкретному Python, раскомментируй и пропиши путь:
# use_python("C:/Users/.../python.exe", required = TRUE)

# Импортируем Python-модуль sentence_transformers
st <- import("sentence_transformers")

# Берём мультиязычную модель, которая умеет по-русски
model <- st$SentenceTransformer("sentence-transformers/paraphrase-multilingual-MiniLM-L12-v2")

###########################################################
# 2. Читаем два текста (Онегин и Университетская поэма)
###########################################################

text_onegin  <- read_file("онегин.txt")
text_nabokov <- read_file("набоков.txt")

###########################################################
# 3. Разбиваем тексты на предложения
###########################################################

split_into_sentences <- function(text) {
  # Разбиваем по . ! ? (очень простой вариант)
  s <- str_split(text, "(?<=[\\.\\!\\?])\\s+", simplify = FALSE)[[1]]
  # Убираем лишние пробелы
  s <- str_trim(s)
  # Убираем пустые строки и слишком короткие
  s <- s[nchar(s) > 10]
  return(s)
}

sent_onegin  <- split_into_sentences(text_onegin)
sent_nabokov <- split_into_sentences(text_nabokov)

length(sent_onegin)
length(sent_nabokov)

###########################################################
# 4. Строим эмбеддинги (векторное представление) предложений
###########################################################

# Получаем векторы из модели (Python возвращает numpy-массив)
emb_onegin_py  <- model$encode(sent_onegin)
emb_nabokov_py <- model$encode(sent_nabokov)

# Переводим в R-матрицы
emb_onegin  <- py_to_r(emb_onegin_py)
emb_nabokov <- py_to_r(emb_nabokov_py)

# Убедимся, что это матрицы: n_предложений x n_размерность
dim(emb_onegin)
dim(emb_nabokov)

###########################################################
# 5. Считаем косинусное сходство между предложениями
###########################################################

# Функция нормировки строк матрицы (делим на длину вектора)
normalize_rows <- function(m) {
  norms <- sqrt(rowSums(m^2))
  m / norms
}

emb_onegin_norm  <- normalize_rows(emb_onegin)
emb_nabokov_norm <- normalize_rows(emb_nabokov)

# Матрица косинусных сходств:
# строки — предложения Онегина,
# столбцы — предложения Набокова
sim_mat <- emb_onegin_norm %*% t(emb_nabokov_norm)

dim(sim_mat)

###########################################################
# 6. Для каждого предложения Онегина находим
#    самое похожее предложение у Набокова
###########################################################

# Для каждой строки находим индекс максимального сходства
max_idx   <- apply(sim_mat, 1, which.max)
max_sim   <- apply(sim_mat, 1, max)

# Порог "достаточного" сходства (можно варьировать: 0.6, 0.7, 0.8)
threshold <- 0.7

# Фильтруем только те пары, у которых сходство >= threshold
keep <- which(max_sim >= threshold)

length(keep)  # сколько найдено хороших пар

onegin_sentences_sim  <- sent_onegin[keep]
nabokov_sentences_sim <- sent_nabokov[max_idx[keep]]
sim_values            <- max_sim[keep]

###########################################################
# 7. Собираем результат в табличку
###########################################################

results_semantic <- data.frame(
  onegin_sentence   = onegin_sentences_sim,
  nabokov_sentence  = nabokov_sentences_sim,
  cosine_similarity = sim_values,
  stringsAsFactors  = FALSE
)

# Сортируем по убыванию схожести
results_semantic <- results_semantic %>%
  arrange(desc(cosine_similarity))

###########################################################
# 8. Смотрим верхние пары
###########################################################

head(results_semantic, 20)

