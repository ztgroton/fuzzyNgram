
#' gen List of N-Grams from a Character Vector of Strings
#'
#' @param string character
#' @param n integer
#' @param split character
#'
#' @return list
#' @export
#'
#' @examples
#' \dontrun{
#'  output_ngrams <- gen_ngrams(string = input_char_vec, n = 3, split = ' ')
#' }
gen_ngrams <- function(string, n = 3, split = ' ') {

  # Validate Inputs
  if (missing(string)) {stop("`str` is missing in call to `gen_ngrams`")}
  if (missing(n)) {n <- 3}
  if (missing(split)) {split <- ' '}

  # Validate Input Expectations

  # * string
  if (!isTRUE(validate_input.character(string, non_empty = TRUE, throw_err = FALSE))) {
    stop("`x` must be a valid character in call to `gen_ngrams`")
  }

  # * n
  n <- as.numeric(n)
  if (!isTRUE(validate_input.numeric(n, is_scalar = TRUE, is_whole = TRUE)) || !isTRUE(n > 0)) {
    stop("`n` must be positive whole number in call to `gen_ngrams`")
  }

  # * split
  if (!isTRUE(validate_input.character(split, is_scalar = TRUE))) {
    stop("`split` must be length 1 character vector in call to `gen_ngrams`")
  }

  # Main Logic

  # * Generate Tokens
  token_list <- gen_tokens(string = string, split = split)

  # * Generate Start/End Indices for NGRAM generation
  indicies <- list()

  for (m in 1:n) {
    for (i in 1:(n-m+1)) {
      indicies[[length(indicies)+1]] <- c(i,i+m-1)
    }
  }

  # Generate NGRAM Index Names
  names(indicies) <- purrr::map_chr(indicies, function(x) {
    if (!isTRUE(x[1] == x[2])) {
      return(paste(x[1], x[2], sep = '-'))
    } else {
      return(as.character(x[1]))
    }
  })

  # * Use Indices to generate NGRAMs
  ngrams <- purrr::map(names(indicies), function(index_name) {

    # Fetch Index Values
    index_val <- indicies[[index_name]]

    # Generate sequence used for subsetting `token_list` elements
    sub_seq <- seq(index_val[1], index_val[2])

    # Use `sub_seq` to subset all character vectors in `token_list`
    res <- purrr::map(token_list, function(t) {return(t[sub_seq])})

    # Return `res`
    return(res)

  })

  names(ngrams) <- names(indicies)

  # Return `ngrams`
  return(ngrams)

}
