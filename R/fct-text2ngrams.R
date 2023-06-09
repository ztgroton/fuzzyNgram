
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
  cat("Cleaning Whitespace... ")
  tictoc::tic()
  clean_string <- clean_ws(string)
  tictoc::toc()

  # * Generate List of Tokens from Input Character Vector 'string'
  cat("Generating Tokens from Text... ")
  tictoc::tic()
  token_list <- gen_tokens(string = clean_string, split = split)
  tictoc::toc()

  # * Generate List of NGRAMs from Token List
  cat("Generating N-Grams from Tokens... ")
  tictoc::tic()
  ngram_list <- gen_ngrams(tokens = token_list, n = n, split = split)
  tictoc::toc()
  
  # Convert `token_list` to formatted data.frame
  #token_list <- list2df(token_list)
  
  # Compile Results 
  results <- list(
    input_text = string,
    tokens = token_list, 
    ngrams = ngram_list
  )

  # Return Results
  return(results)

} 
