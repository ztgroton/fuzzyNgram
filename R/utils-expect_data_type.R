
#' Verify that R Object meets Data Type Constraints
#'
#' @param obj R Object
#' @param type character - desired data type / 'character', 'numeric' or 'logical'
#' @param not_empty TRUE/FALSE - specifies if 'length(obj)' must be non-zero
#' @param is_scalar TRUE/FALSE - specifies if 'length(obj)' must equal 1
#' @param allow_na TRUE/FALSE - specifies if NA elements are allowed in 'obj'
#' @param check_names TRUE/FALSE - specifies if 'names(obj)' should meet same criteria
#' @param throw_err TRUE/FALSE - specifies if function should return or throw error
#'
#' @return R Object
#' @export
#'
#' @examples
#' \dontrun{
#' expect_data_type(obj, type, not_empty, is_scalar, allow_na, check_names, throw_err)
#' }
expect_data_type <- function(obj, type, not_empty, is_scalar, allow_na, check_names, throw_err) {

  # Handle Missing Inputs and Apply Default Values
  if (missing(obj)) {stop("`obj` is missing in call to `expect_data_type`", call. = FALSE)}
  if (missing(type)) {stop("`type` is missing in call to `expect_data_type`", call. = FALSE)}
  if (missing(not_empty)) {not_empty <- FALSE}
  if (missing(is_scalar)) {is_scalar <- FALSE}
  if (missing(allow_na)) {allow_na <- FALSE}
  if (missing(check_names)) {check_names <- FALSE}
  if (missing(throw_err)) {throw_err <- TRUE}

  if (isTRUE(identical(is_scalar, TRUE))) {not_empty <- TRUE}

  valid_types <- c('character', 'numeric', 'logical')
  if (!isTRUE(type %in% valid_types)) {
    stop("`type` must be valid value in call to `expect_data_type`")
  }

  # Main Logic
  q_obj <- testthat::quasi_label(rlang::enquo(obj), arg = "object")

  # * throw_err
  if (!isTRUE(identical(throw_err, TRUE)) && !isTRUE(identical(throw_err, FALSE))) {
    stop("`throw_err` must be TRUE FALSE in call to `expect_data_type`")
  }

  # * type
  is_type <- FALSE

  if (isTRUE(identical(type, 'character'))) {is_type <- isTRUE(is.character(obj))}
  else if (isTRUE(identical(type, 'numeric'))) {is_type <- isTRUE(is.numeric(obj))}
  else if (isTRUE(identical(type, 'logical'))) {is_type <- isTRUE(is.logical(obj))}
  else {
    message <- sprintf("`type` must be one of: %s", valid_types)
    stop(message, call. = FALSE)
  }

  if (!isTRUE(is_type)) {
    if (isTRUE(throw_err)) {
      message <- sprintf("%s must be data type %s", q_obj$lab, type)
      stop(message, call. = FALSE)
    } else {return(FALSE)}
  }

  # * `not_empty`
  if (!isTRUE(is.logical(not_empty)) || !isTRUE(length(not_empty) == 1) || isTRUE(is.na(not_empty))) {
    message <- "`not_empty` must be TRUE/FALSE in call to `expect_data_type`"
    stop(message, call. = FALSE)
  }

  if (isTRUE(not_empty)) {
    is_not_empty <- isTRUE(length(obj) > 0)
    if (!isTRUE(is_not_empty)) {
      if (isTRUE(throw_err)) {
        message <- sprintf("%s must have non-zero length", q_obj$lab)
        stop(message, call. = FALSE)
      } else {return(FALSE)}
    }
  }

  # * `is_scalar`
  if (!isTRUE(is.logical(is_scalar)) || !isTRUE(length(is_scalar) == 1) || isTRUE(is.na(is_scalar))) {
    message <- "`is_scalar` must be TRUE/FALSE in call to `expect_data_type`"
    stop(message, call. = FALSE)
  }

  if (isTRUE(is_scalar)) {
    is_scalar <- isTRUE(length(obj) == 1)
    if (!isTRUE(is_scalar)) {
      if (isTRUE(throw_err)) {
        message <- sprintf("%s must have length equal to 1", q_obj$lab)
        stop(message, call. = FALSE)
      } else {return(FALSE)}
    }
  }

  # * `allow_na`
  if (!isTRUE(is.logical(allow_na)) || !isTRUE(length(allow_na) == 1) || isTRUE(is.na(allow_na))) {
    message <- "`allow_na` must be TRUE/FALSE in call to `expect_data_type`"
    stop(message, call. = FALSE)
  }

  if (!isTRUE(allow_na)) {
    is_allow_na <- !isTRUE(any(purrr::map_lgl(obj, ~ is.null(.) || is.na(.))))
    if (!isTRUE(is_allow_na)) {
      if (isTRUE(throw_err)) {
        message <- sprintf("%s cannot contain NA/NULL elements", q_obj$lab)
        stop(message, call. = FALSE)
      } else {return(FALSE)}
    }
  }

  # * `check_names`
  if (!isTRUE(is.logical(check_names)) || !isTRUE(length(check_names) == 1) || isTRUE(is.na(check_names))) {
    message <- "`check_names` must be TRUE/FALSE in call to `expect_data_type`"
    stop(message, call. = FALSE)
  }

  if (isTRUE(check_names)) {
    expect_data_type(
      obj = names(obj),
      type = type,
      not_empty = not_empty,
      is_scalar = is_scalar,
      allow_na = allow_na,
      check_names = FALSE,
      throw_err = throw_err
    )
  }

  # Return Final Result
  return(TRUE)

}
