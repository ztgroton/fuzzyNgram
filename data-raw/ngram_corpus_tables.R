## code to prepare `ngram_corpus_tables` dataset goes here

ngram_corpus_tables <- list(
  
  list(name = 'dataset', create_order = 1), 
  
  list(name = 'column', create_order = 2), 
  
  list(name = 'record', create_order = 3), 
  
  list(name = 'ngram_data', create_order = 4), 
  
  list(name = 'ngram', create_order = 5)
  
)

ngram_corpus_tables <- purrr::transpose(ngram_corpus_tables)
ngram_corpus_tables$name <- purrr::map_chr(ngram_corpus_tables$name, function(x){as.character(x)})
ngram_corpus_tables$create_order <- purrr::map_int(ngram_corpus_tables$create_order, function(x){as.integer(x)})
ngram_corpus_tables <- as.data.frame(ngram_corpus_tables)

usethis::use_data(ngram_corpus_tables, overwrite = TRUE)
