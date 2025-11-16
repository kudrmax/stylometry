library(quanteda)
library(readr)

# читаем файлы целиком одной строкой каждый
text_onegin  <- read_file("онегин.txt")     # Пушкин
text_nabokov <- read_file("набоков.txt")    # Университетская поэма

substr(text_onegin, 1, 200)   # покажет первые 200 символов

texts <- c(Onegin = text_onegin,
           Nabokov = text_nabokov)

toks <- tokens(
  texts,
  remove_punct = TRUE,   # убрать пунктуацию
  remove_numbers = TRUE  # убрать цифры
)

# привести к нижнему регистру
toks <- tokens_tolower(toks)

toks_3 <- tokens_ngrams(toks, n = 4)

dfm_onegin <- dfm(toks_3['Onegin'])
dfm_uni    <- dfm(toks_3['Nabokov'])

ngrams_onegin  <- featnames(dfm_onegin)
ngrams_nabokov <- featnames(dfm_uni)

common_ngrams <- intersect(ngrams_onegin, ngrams_nabokov)
length(common_ngrams)   # сколько общих
head(common_ngrams, 20) # первые 20 общих фраз

##################################################

# Берём только общие столбцы
dfm_onegin_common <- dfm_onegin[, common_ngrams]
dfm_uni_common    <- dfm_uni[,    common_ngrams]

# Частоты по каждому тексту
freq_onegin <- as.numeric(dfm_onegin_common[1, ])
freq_uni    <- as.numeric(dfm_uni_common[1, ])

names(freq_onegin) <- common_ngrams
names(freq_uni)    <- common_ngrams

# Например, топ-10 общих n-грамм у Пушкина
sort(freq_onegin, decreasing = TRUE)[1:30]

# И у Набокова
sort(freq_uni, decreasing = TRUE)[1:30]


