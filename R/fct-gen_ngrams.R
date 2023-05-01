
#' gen List of N-Grams from a List of Tokens (stored as character vectors)
#'
#' @param tokens character
#' @param n integer
#' @param split character
#'
#' @return list
#' @export
#'
#' @examples
#' \dontrun{
#'  output_ngrams <- gen_ngrams(tokens = token_list, n = 3, split = ' ')
#' }
gen_ngrams <- function(tokens, n = 3, split = ' ') {

  # Validate Inputs
  if (missing(tokens)) {stop("`tokens` is missing in call to `gen_ngrams`")}
  if (missing(n)) {n <- 3}
  if (missing(split)) {split <- ' '}

  # Validate Input Expectations

  # * tokens
  if (!isTRUE(validate_input.list(obj = tokens, list_elem_type = 'character', list_non_empty = TRUE, throw_err = FALSE))) {
    stop("`x` must be a valid list with character vector elements in call to `gen_ngrams`")
  }

  # * n
  n <- as.numeric(n)
  if (!isTRUE(validate_input.numeric(obj = n, is_scalar = TRUE, is_whole = TRUE)) || !isTRUE(n > 0)) {
    stop("`n` must be positive whole number in call to `gen_ngrams`")
  }

  # * split
  if (!isTRUE(validate_input.character(obj = split, is_scalar = TRUE))) {
    stop("`split` must be length 1 character vector in call to `gen_ngrams`")
  }

  # Main Logic

  # * Generate Start/End Indices for NGRAM generation
  indicies <- list()

  for (m in 1:n) {
    for (i in 1:(n-m+1)) {
      indicies[[length(indicies)+1]] <- c(i,i+m-1)
    }
  }

  # Generate NGRAM Index Names
  index_names <- purrr::map_chr(indicies, function(x) {
    if (!isTRUE(x[1] == x[2])) {
      return(paste(x[1], x[2], sep = '-'))
    } else {
      return(as.character(x[1]))
    }
  })

  names(indicies) <- index_names

  # Generate NGRAM Length Lookup Vector
  ngram_length <- purrr::map_dbl(indicies, function(x) {
    if (!isTRUE(x[1] == x[2])) {
      return(abs(x[2]-x[1])+1)
    } else {
      return(1)
    }
  })

  names(ngram_length) <- index_names

  # * Use Indices to generate NGRAMs
  ngrams <- purrr::map(names(indicies), function(index_name) {

    # Fetch Index Values
    index_val <- indicies[[index_name]]

    # Generate sequence used for subsetting `tokens` elements
    sub_seq <- seq(index_val[1], index_val[2])

    # Use `sub_seq` to subset all character vectors in `tokens`
    res <- purrr::map(tokens, function(t) {return(t[sub_seq])})

    # Return `res`
    return(res)

  })

  names(ngrams) <- names

  #### FUTURE WORK
  ######## NEED TO CONVERT `ngrams` LIST ELEMENTS INTO DATA FRAMES
  ######## EACH LIST ELEMENT DATA FRAME MUST CONTAIN TWO COLUMNS: ORIG_ROW_NUM, TOKENS
  ######## COALESCE BY COMMON NGRAM LENGTH, THEN REMOVE DUPLICATE ROWS (ACROSS BOTH COLUMNS)
  ######## ADD ANY APPROPRIATE SUMMARY & MEASURE COLUMNS TO EACH LIST ELEMENT DATA FRAME

  # Create Lookup List of `names(indicies)` by common NGRAM length
  ngram_length_lookup <- split(names(ngram_length), ngram_length)

  # * Coalesce `ngrams` by common NGRAM length
  ngrams_by_length <- purrr::map(ngram_length_lookup, function(x) {

    # Subset `ngrams` list
    ngrams_x <- ngrams[x]

    # Coalesce `ngrams_x` into single character vector
    ngrams_x <- purrr::reduce(ngrams_x, `c`)

    # Return `ngrams_x`
    return(ngrams_x)

  })


  browser()

  # Return `ngrams`
  return(ngrams)

}
