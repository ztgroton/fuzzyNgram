
#' Generate List of Tokens from a Character Vector and String Delimiter
#'
#' @param string character
#' @param split character
#'
#' @return list
#' @export
#'
#' @examples
#' \dontrun{
#' output_tokens <- gen_tokens(string = input_vec, split = ' ')
#' }
gen_tokens <- function(string, split = ' ') {

  # Validate Inputs
  if (missing(string)) {stop("`str` is missing in call to `gen_tokens`")}
  if (missing(split)) {split <- ' '}

  # Validate Input Expectations

  # * string
  if (!isTRUE(validate_input.character(string, non_empty = TRUE, throw_err = FALSE))) {
    stop("`x` must be a valid character in call to `gen_tokens`")
  }

  # * split
  if (!isTRUE(validate_input.character(split, is_scalar = TRUE))) {
    stop("`split` must be length 1 character vector in call to `gen_tokens`")
  }

  # Main Logic

  # Split `string` using `split` as fixed delimiter / Generate List of Character Vectors
  res <- stringr::str_split(
    string = string,
    pattern = stringr::coll(pattern = split)
  )

  # Name `res` according to natural data order
  names(res) <- as.character(1:length(res))

  # Return Results
  return(res)

}
