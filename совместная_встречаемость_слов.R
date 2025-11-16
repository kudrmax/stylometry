# === 0. Установка и подключение пакетов ===
# Если чего-то нет - раскомментируй install.packages(...)
# install.packages("udpipe")
# install.packages("readr")
# install.packages("tibble")
# install.packages("dplyr")
# install.packages("igraph")
# install.packages("ggraph")

library(udpipe)
library(readr)
library(tibble)
library(dplyr)
library(igraph)
library(ggraph)

path <- "набоков.txt"

# === 1. Читаем два txt-файла с поэмами ===
# Задай правильные пути к файлам
poem_txt <- read_lines(path, locale = locale(encoding = "UTF-8"))

# Склеиваем строки каждой поэмы в один текст
poem <- paste(poem_txt, collapse = " ")

# Начальный датасет формата doc_id / text
texts <- tibble(
  doc_id = c("poem"),  # можно назвать по авторам/названиям
  text   = c(poem)
)

# === 2. Скачиваем и загружаем модель UDPipe для русского ===
# ЭТО делается один раз, потом можешь закомментировать download и просто load_model()

rus_model_info <- udpipe_download_model(language = "russian")
rus_model <- udpipe_load_model(rus_model_info$file_model)

# Если модель уже скачана, можешь вместо двух строк выше использовать что-то вроде:
# rus_model <- udpipe_load_model("russian-gsd-ud-2.5-191206.udpipe")

# === 3. Аннотация текстов (токенизация, лемматизация, POS-теги) ===
anno <- udpipe_annotate(
  object = rus_model,
  x      = texts$text,
  doc_id = texts$doc_id
)

caesar_pos3 <- as.data.frame(anno)  # имя как в твоём примере, можешь переименовать

# Посмотреть структуру (по желанию)
# str(caesar_pos3)
# head(caesar_pos3)

# === 4. Берём только существительные ===
caesar_subset <- subset(caesar_pos3, upos == "NOUN")

# === 5. Считаем совместную встречаемость лемм в пределах одного предложения ===
cooc_raw <- cooccurrence(
  x     = caesar_subset,
  term  = "lemma",                     # что считаем как «слово»
  group = c("doc_id", "sentence_id")   # «окно» совместного появления = предложение в документе
)

# Переводим в tibble и фильтруем по порогу cooc,
# чтобы граф не превратился в «ёжика». Порог подбери сам.
cooc <- cooc_raw |>
  as_tibble() |>
  filter(cooc == 2) |>          # можешь поставить 3, 5, 10 — как визуально лучше
  arrange(desc(cooc))

# Посмотреть таблицу (по желанию)
# print(≈)

# Если хочешь ограничиться, например, 50 самыми частыми парами:
cooc_top <- cooc |>
  slice_head(n = 50)

# === 6. Строим граф совместной встречаемости ===
wordnetwork <- graph_from_data_frame(cooc_top)

set.seed(123)  # для воспроизводимого расположения вершин

library(igraph)
library(ggraph)

wordnetwork <- graph_from_data_frame(cooc)
ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(width = cooc), alpha = 0.8, edge_colour = "grey90", show.legend=FALSE) +
  geom_node_label(aes(label = name), col = "#1f78b4", size = 4) +
  theme_void() +
  labs(title = "Совместная встречаемость существительных", subtitle = "De Bello Gallico 1-7")

