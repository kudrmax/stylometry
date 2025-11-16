# -----------------------------
# 0. Пакеты
# -----------------------------
library(tidyverse)
library(tidytext)
library(udpipe)
library(rulexicon)
library(ggplot2)

# -----------------------------
# 1. Посмотреть на лексикон (не обязательно, но полезно)
# -----------------------------
set.seed(0211)
chen_skiena <- hash_sentiment_chen_skiena
sample_n(chen_skiena, 10)

# -----------------------------
# 2. Читаем текст поэмы из файла
# -----------------------------
# убедись, что файл "онегин.txt" лежит в рабочей директории
# getwd() покажет, какая она сейчас
poem_raw <- readLines("онегин.txt", encoding = "UTF-8")
poem_text <- paste(poem_raw, collapse = " ")

# -----------------------------
# 3. Скачиваем и грузим модель udpipe (делается один раз)
# -----------------------------
model_info <- udpipe_download_model(language = "russian")
udmodel <- udpipe_load_model(file = model_info$file_model)

# -----------------------------
# 4. Аннотируем текст (токенизация, лемматизация и т.п.)
# -----------------------------
anno <- udpipe_annotate(
  udmodel,
  x      = poem_text,
  doc_id = "poem_1"
)

poem_tbl <- as_tibble(anno)
poem_tbl

# -----------------------------
# 5. Приводим к формату, как в лекции (аналог liza_tbl)
#    - убираем пунктуацию
#    - берём леммы
#    - переименовываем в token
#    - делим на куски по 100 слов
# -----------------------------
poem_tbl_clean <- poem_tbl |>
  filter(upos != "PUNCT") |>
  select(lemma) |>
  rename(token = lemma) |>
  mutate(chunk = round(((row_number() + 50) / 100), 0))

poem_tbl_clean

# -----------------------------
# 6. Подключаем лексикон и считаем тональность
#    Здесь используем AFINN для русского
# -----------------------------
lex <- hash_sentiment_afinn_ru

# соединяем по лемме (token)
poem_sent <- poem_tbl_clean |>
  inner_join(lex, by = "token")

poem_sent

# суммарная тональность по каждому куску
poem_chunk_sent <- poem_sent |>
  group_by(chunk) |>
  summarise(sum = sum(score), .groups = "drop") |>
  mutate(tone = if_else(sum >= 0, "pos", "neg")) |>
  arrange(chunk)

poem_chunk_sent

# -----------------------------
# 7. Рисуем график тональности по ходу поэмы
# -----------------------------
ggplot(poem_chunk_sent, aes(x = chunk, y = sum, fill = tone)) +
  geom_col(show.legend = FALSE) +
  labs(
    title = "Эмоциональная тональность «Евгения Онегина»",
    x = "повествовательное время (куски по 100 слов)",
    y = "суммарная тональность"
  ) +
  scale_fill_manual(values = c("pos" = "#78c679", "neg" = "#e34a33")) +
  theme_minimal(base_size = 14)
