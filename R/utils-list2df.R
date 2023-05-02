
#' Convert List into Formatted DataFrame
#'
#' @param x list
#' @param elem_type character 
#'
#' @return data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' output_dataframe <- list2df(x = input_list, elem_type = 'character')
#' }
list2df <- function(x, elem_type) {
  
  # Validate Inputs 
  if (missing(x)) {stop("`x` is missing in call to `list2df`")}
  if (missing(elem_type)) {stop("`elem_type` is missing in call to `list2df`")}
  
  # Validate Input Expectations
  
  # * `x` & `elem_type`
  if (!isTRUE(validate_input.list(obj = x, 
                                  list_elem_type = elem_type, 
                                  list_non_empty = TRUE, 
                                  elem_check_names = TRUE, 
                                  throw_err = FALSE))) {
    stop("`x` must be a valid list with expected element types in call to `list2df`")
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
