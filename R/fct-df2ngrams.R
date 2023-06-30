
#' Generate N-Grams for Character Columns of DataFrame
#'
#' @param df data.frame
#' @param n numeric
#' @param split character
#'
#' @return list
#' @export
#'
#' @examples
#' \dontrun{
#'  output <- df2ngrams(df = edap_upc)
#' }
df2ngrams <- function(df, n = 1000, split = ' ') {
  
  # Validate Inputs
  if (missing(df)) {stop("`df` is missing in call to `df2ngrams`")}
  if (missing(n)) {n <- 1000}
  if (missing(split)) {split <- ' '}
  
  # Validate Input Expectations
  
  # * df
  if (!isTRUE(is.data.frame(df))) {
    stop("`df` must be a data.frame in call to `df2ngrams`")
  }
  
  # * n
  n <- as.numeric(n)
  if (!isTRUE(validate_input.numeric(n, is_scalar = TRUE, is_whole = TRUE)) || !isTRUE(n > 0)) {
    stop("`n` must be positive whole number in call to `df2ngrams`")
  }
  
  # * split
  if (!isTRUE(validate_input.character(split, is_scalar = TRUE))) {
    stop("`split` must be length 1 character vector in call to `df2ngrams`")
  }
  
  # MAIN LOGIC ----
  
  # List Column Data Types ----
  col_data_types <- purrr::map_chr(df, function(x){class(x)[[1]]})
  names(col_data_types) <- colnames(df)
  
  # List Character Columns
  char_cols <- col_data_types[col_data_types == 'character']
  char_cols <- names(char_cols)
  
  if (!isTRUE(length(char_cols) > 0)) {
    stop("`colnames(df)` must contain some character-valued columns in call to `df2ngrams`")
  }
  
  # Iterate over `char_cols` / Generate Tokens
  tokens <- purrr::map(char_cols, function(x) {
    
    cat(paste0("\nProcessing '", x, "'... \n"))
    tictoc::tic()
    res <- text2ngrams(string = df[[x]], n = n, split = split)
    tictoc::toc()
    
    return(res)
    
  })
  names(tokens) <- char_cols
  
  # Return Tokens 
  return(tokens)
  
}
