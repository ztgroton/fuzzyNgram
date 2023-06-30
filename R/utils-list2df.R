
#' Convert List into Formatted DataFrame
#'
#' @param x list
#'
#' @return data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' output_dataframe <- list2df(x = input_list)
#' }
list2df <- function(x) {
  
  # Validate Inputs 
  if (missing(x)) {stop("`x` is missing in call to `list2df`")}
  
  # Validate Input Expectations
  
  # * `x` & `elem_type`
  if (!isTRUE(is.list(x)) || !isTRUE(length(x) > 0)) {
    stop("`x` must be a non-empty list in call to `list2df`")
  }
  
  # Convert `x` to a formatted data.frame
  res <- data.frame(
    orig_row_num = names(x),
    value = I(x), 
    stringsAsFactors = FALSE
  )
  
  # Return `res`
  return(res)
  
}
