
require(tidyverse)
require(data.table)

# * valid_char ---- 
valid_char <- function(x, mode) {
  
  # Validate Input
  if (missing(x)) {stop("`x` missing in call to `valid_char`", call. = FALSE)}
  
  if (missing(mode)) {mode <- 'quiet'}
  if (!isTRUE(mode == 'quiet') && !isTRUE(mode == 'loud')) {stop("`mode` must equal 'quiet' or 'loud' in call to `valid_char`", call. = FALSE)}
  
  res <- isTRUE(is.character(x))
  if (isTRUE(mode == 'loud') && !isTRUE(res)) {stop("`x` must be character vector in call to `valid_char`", call. = FALSE)}
  
  res <- res && isTRUE(length(x) > 0)
  if (isTRUE(mode == 'loud') && !isTRUE(res)) {stop("`x` must have non-zero length in call to `valid_char`", call. = FALSE)}
  
  res <- res && !isTRUE(any(map_lgl(x, is.null)))
  if (isTRUE(mode == 'loud') && !isTRUE(res)) {stop("`x` elements cannot be NULL in call to `valid_char`", call. = FALSE)}
  
  res <- res && !isTRUE(any(map_lgl(x, is.na)))
  if (isTRUE(mode == 'loud') && !isTRUE(res)) {stop("`x` elements cannot be NA in call to `valid_char`", call. = FALSE)}
  
  return(res)
  
}

# * valid_numeric ---- 
valid_numeric <- function(x, mode) {
  
  # Validate Input
  if (missing(x)) {stop("`x` missing in call to `valid_numeric`", call. = FALSE)}
  
  if (missing(mode)) {mode <- 'quiet'}
  if (!isTRUE(mode == 'quiet') && !isTRUE(mode == 'loud')) {stop("`mode` must equal 'quiet' or 'loud' in call to `valid_numeric`", call. = FALSE)}
  
  res <- isTRUE(is.numeric(x))
  if (isTRUE(mode == 'loud') && !isTRUE(res)) {stop("`x` must be numeric vector in call to `valid_numeric`", call. = FALSE)}
  
  res <- res && isTRUE(length(x) > 0)
  if (isTRUE(mode == 'loud') && !isTRUE(res)) {stop("`x` must have non-zero length in call to `valid_numeric`", call. = FALSE)}
  
  res <- res && !isTRUE(any(map_lgl(x, is.null)))
  if (isTRUE(mode == 'loud') && !isTRUE(res)) {stop("`x` elements cannot be NULL in call to `valid_numeric`", call. = FALSE)}
  
  res <- res && !isTRUE(any(map_lgl(x, is.na)))
  if (isTRUE(mode == 'loud') && !isTRUE(res)) {stop("`x` elements cannot be NA in call to `valid_numeric`", call. = FALSE)}
  
  return(res)
  
}

# * valid_integer ---- 
valid_integer <- function(x, mode) {
  
  # Validate Input
  if (missing(x)) {stop("`x` missing in call to `valid_integer`", call. = FALSE)}
  
  if (missing(mode)) {mode <- 'quiet'}
  if (!isTRUE(mode == 'quiet') && !isTRUE(mode == 'loud')) {stop("`mode` must equal 'quiet' or 'loud' in call to `valid_integer`", call. = FALSE)}
  
  res <- isTRUE(valid_numeric(x))
  if (isTRUE(mode == 'loud') && !isTRUE(res)) {stop("`x` must be VALID NUMERIC vector in call to `valid_integer`", call. = FALSE)}
  
  res <- res && isTRUE(all(x > 0))
  if (isTRUE(mode == 'loud') && !isTRUE(res)) {stop("`x` elements must be strictly greater than zero in call to `valid_integer`", call. = FALSE)}
  
  res <- res && isTRUE(all(x %% 1 == 0))
  if (isTRUE(mode == 'loud') && !isTRUE(res)) {stop("`x` elements must be whole integers in call to `valid_integer`", call. = FALSE)}
  
  return(res)
  
}

# * token_tidy ----
token_tidy <- function(str) {
  
  # Validate Input
  if (!isTRUE(valid_char(str))) {stop("`str` must be a VALID CHAR vector in call to `token_tidy`", call. = FALSE)}
  
  # Convert all contiguous whitespace to single-space / trim whitespace from start and end of string
  res <- str_trim(gsub("\\s+", " ", str))
  return(res)
  
}

# * token_split ---- 
token_split <- function(str, split) {
  
  # Validate Input
  if (missing(split)) {split <- ' '}
  
  if (!isTRUE(valid_char(str))) {stop("`str` must be a VALID CHAR vector in call to `token_split`", call. = FALSE)}
  if (!isTRUE(valid_char(split)) || !isTRUE(length(split) == 1)) {stop("`split` must be a VALID CHAR vector of length 1 in call to `token_split`", call. = FALSE)}
  
  # Create dictionary from `str`, mapping into unique integers
  dict <- data.table(string_val = str, str_index = 1:length(str))
  
  # Return `data.table` of tokens
  tokens <- map2(dict$string_val, dict$str_index, function(x, y) {
    z <- unlist(strsplit(x, split))
    return(data.table(str_index = y, token_index = 1:length(z), tokens = z))
  }) %>% rbindlist()
  
  return(list(dict = dict, tokens = tokens))
  
}


# token_ngram ---- 
token_ngram <- function(token, n, split) {
  
  # Validate Input
  if (missing(token)) {stop("`token` missing in call to `token_ngram`", call. = FALSE)}
  if (missing(n)) {n <- 3}
  if (missing(split)) {split <- ' '}
  
  if (!isTRUE(valid_char(token)) || !isTRUE(length(token) == 1)) {stop("`token` must be a VALID CHAR vector of length 1 in call to `token_ngram`", call. = FALSE)}
  if (!isTRUE(valid_char(split)) || !isTRUE(length(split) == 1)) {stop("`split` must be a VALID CHAR vector of length 1 in call to `token_ngram`", call. = FALSE)}
  if (isTRUE(any(grepl(split, token)))) {stop("`split` cannot be a substring of `token` in call to `token_ngram`", call. = FALSE)}
  
  if (!isTRUE(valid_integer(n)) || !isTRUE(length(n) == 1)) {stop("`n` must be a VALID INTEGER vector of length 1 in call to `token_ngram`", call. = FALSE)}
  
  # Generate Start/End Indicies for NGRAM generation
  indicies <- list()
  
  for (m in 1:n) {
    for (i in 1:(n-m+1)) {
      indicies[[length(indicies)+1]] <- c(i,i+m-1)
    }
  }
  
  # Use indicies to generate NGRAMs
  ngram <- map_chr(indicies, function(x) {substr(x = token, start = x[1], stop = x[2])})
  
  # Return `data.table` containing NGRAMs and their length
  return(data.table(length = map_dbl(ngram, nchar), ngram = ngram))
  
}


# token_tf ---- 
token_tf <- function(t, d) {
  
  # Validate Inputs
  if (missing(t)) {stop("`t` missing in call to `token_tf`", call. = FALSE)}
  if (missing(d)) {stop("`d` missing in call to `token_tf`", call. = FALSE)}
  
  if (!isTRUE(valid_char(t)) || !isTRUE(length(t) == 1)) {stop("`t` must be a VALID CHAR vector of length 1 in call to `token_tf`", call. = FALSE)}
  if (!isTRUE(valid_char(d)) || !isTRUE(length(d) == 1)) {stop("`d` must be a VALID CHAR vector in call to `token_tf`", call. = FALSE)}
  
  # Process document(s)
  d <- token_tidy(d)
  token_data <- token_split(d)
  
  
}

# token_idf ---- 
token_idf <- function(t, D) {
  
  # Validate Inputs
  if (missing(t)) {stop("`t` missing in call to `token_idf`", call. = FALSE)}
  if (missing(D)) {stop("`D` missing in call to `token_idf`", call. = FALSE)}
  
  if (!isTRUE(valid_char(t)) || !isTRUE(length(t) == 1)) {stop("`t` must be a VALID CHAR vector of length 1 in call to `token_idf`", call. = FALSE)}
  if (!isTRUE(valid_char(D))) {stop("`D` must be a VALID CHAR vector in call to `token_idf`", call. = FALSE)}
  
} 

# token_tf_idf ---- 
token_tf_idf <- function(t, d, D) {
  
  # Validate Inputs
  if (missing(t)) {stop("`t` missing in call to `token_tf_idf`", call. = FALSE)}
  if (missing(d)) {stop("`d` missing in call to `token_tf_idf`", call. = FALSE)}
  if (missing(D)) {stop("`D` missing in call to `token_tf_idf`", call. = FALSE)}
  
  if (!isTRUE(valid_char(t)) || !isTRUE(length(t) == 1)) {stop("`t` must be a VALID CHAR vector of length 1 in call to `token_tf_idf`", call. = FALSE)}
  if (!isTRUE(valid_char(d)) || !isTRUE(length(d) == 1)) {stop("`d` must be a VALID CHAR vector of length 1 in call to `token_tf_idf`", call. = FALSE)}
  if (!isTRUE(valid_char(D))) {stop("`D` must be a VALID CHAR vector in call to `token_tf_idf`", call. = FALSE)}
  
}
