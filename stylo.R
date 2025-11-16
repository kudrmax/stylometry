library(stylo)


results <- stylo(
  gui = TRUE,
  corpus.dir   = "texts",
  encoding     = "UTF-8",
  ngram.size   = 1,
  mfw          = 500,
  analysis.type    = "CA",
  distance.measure = "delta",
  sampling     = "no.sampling"
)

results$distance.table