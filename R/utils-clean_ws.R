
#' Clean-Up Whitespace in Character Vectors
#'
#' @param x character
#'
#' @return character
#'
#' @examples
#' \dontrun{
#' clean_vec <- clean_ws(x = dirty_vec)
#' }
clean_ws <- function(x) {

  # Validate Inputs
  if (missing(x)) {stop("`x` is missing in call to `clean_ws`")}

  # Validate Input Expectations
  if (!isTRUE(validate_input.character(x, non_empty = TRUE, throw_err = FALSE))) {
    stop("`x` must be a valid character in call to `clean_ws`")
  }

  # Main Logic

  # * convert all contiguous whitespace to single-space / trim whitespace from start/end
  res <- stringr::str_squish(string = x)

  # Return Results
  return(res)

}
