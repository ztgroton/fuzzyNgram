
#' Convert Character Vector of String Values into List of NGRAMs
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
#' output_ngrams <- text2ngrams(string = input_char_vec, n = 3, split = ' ')
#' }
text2ngrams <- function(string, n = 3, split = ' ') {

  # Validate Inputs
  if (missing(string)) {stop("`string` is missing in call to `text2ngrams`")}
  if (missing(n)) {n <- 3}
  if (missing(split)) {split <- ' '}

  # Validate Input Expectations

  # * string
  if (!isTRUE(validate_input.character(string, non_empty = TRUE, throw_err = FALSE))) {
    stop("`string` must be a valid character in call to `text2ngrams`")
  }

  # * n
  n <- as.numeric(n)
  if (!isTRUE(validate_input.numeric(n, is_scalar = TRUE, is_whole = TRUE)) || !isTRUE(n > 0)) {
    stop("`n` must be positive whole number in call to `text2ngrams`")
  }

  # * split
  if (!isTRUE(validate_input.character(split, is_scalar = TRUE))) {
    stop("`split` must be length 1 character vector in call to `text2ngrams`")
  }

  # Main Logic

  # * Clean-Up White Space in Input Character Vector 'string'
  clean_string <- clean_ws(string)

  # * Generate List of Tokens from Input Character Vector 'string'
  token_list <- gen_tokens(string = clean_string, split = split)

  # * Generate List of NGRAMs from Token List
  ngram_list <- gen_ngrams(tokens = token_list, n = n, split = split)

  # Return Results
  return(ngram_list)

}
