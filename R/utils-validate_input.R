
#' S3 Generic - Validate Format of Function Inputs
#'
#' @param obj S3 Object
#' @param ... r ellipsis
#'
#' @return logical - (TRUE/FALSE)
#' @export
#'
#' @examples
#' \dontrun{
#' is_valid <- validate_input(obj = input, ...)
#' }
validate_input <- function(obj, ...) {UseMethod("validate_input", obj)}

#' S3 Method - Validate Format of Character Vector Inputs
#'
#' @param obj S3 Object
#' @param not_empty logical - (TRUE/FALSE) specifies if 'length(obj)' must be non-zero
#' @param is_scalar logical - (TRUE/FALSE) specifies if 'length(obj)' must equal 1
#' @param allow_na logical - (TRUE/FALSE) specifies if NA elements are allowed in 'obj'
#' @param check_names logical - (TRUE/FALSE) specifies if 'names(obj)' should meet same criteria as 'obj'
#' @param throw_err logical - (TRUE/FALSE) specifies if function should return or throw error
#' @param ... r ellipsis
#'
#' @return logical - (TRUE/FALSE)
#' @export
#'
#' @examples
#' \dontrun{
#' is_valid <- validate_input.character(obj = input, ...)
#' }
validate_input.character <- function(obj,
                                     not_empty = FALSE,
                                     is_scalar = FALSE,
                                     allow_na = TRUE,
                                     check_names = FALSE,
                                     throw_err = TRUE, ...) {

  expect_data_type(
    obj = obj,
    type = 'character',
    not_empty = not_empty,
    is_scalar = is_scalar,
    allow_na = allow_na,
    check_names = check_names,
    throw_err = throw_err
  )

}

#' S3 Method - Validate Format of Logical Vector Inputs
#'
#' @param obj S3 Object
#' @param not_empty logical - (TRUE/FALSE) specifies if 'length(obj)' must be non-zero
#' @param is_scalar logical - (TRUE/FALSE) specifies if 'length(obj)' must equal 1
#' @param allow_na logical - (TRUE/FALSE) specifies if NA elements are allowed in 'obj'
#' @param check_names logical - (TRUE/FALSE) specifies if 'names(obj)' should meet same criteria as 'obj'
#' @param throw_err logical - (TRUE/FALSE) specifies if function should return or throw error
#' @param ... r ellipsis
#'
#' @return logical - (TRUE/FALSE)
#' @export
#'
#' @examples
#' \dontrun{
#' is_valid <- validate_input.logical(obj = input, ...)
#' }
validate_input.logical <- function(obj,
                                   not_empty = FALSE,
                                   is_scalar = FALSE,
                                   allow_na = TRUE,
                                   check_names = FALSE,
                                   throw_err = TRUE, ...) {

  expect_data_type(
    obj = obj,
    type = 'logical',
    not_empty = not_empty,
    is_scalar = is_scalar,
    allow_na = allow_na,
    check_names = check_names,
    throw_err = throw_err
  )

}

#' S3 Method - Validate Format of Numeric Vector Inputs
#'
#' @param obj S3 Object
#' @param not_empty logical - (TRUE/FALSE) specifies if 'length(obj)' must be non-zero
#' @param is_scalar logical - (TRUE/FALSE) specifies if 'length(obj)' must equal 1
#' @param allow_na logical - (TRUE/FALSE) specifies if NA elements are allowed in 'obj'
#' @param check_names logical - (TRUE/FALSE) specifies if 'names(obj)' should meet same criteria as 'obj'
#' @param is_whole logical - (TRUE/FALSE) specifies if 'obj' must be whole number
#' @param throw_err logical - (TRUE/FALSE) specifies if function should return or throw error
#' @param ... r ellipsis
#'
#' @return logical - (TRUE/FALSE)
#' @export
#'
#' @examples
#' \dontrun{
#' is_valid <- validate_input.numeric(obj = input, ...)
#' }
validate_input.numeric <- function(obj,
                                   not_empty = FALSE,
                                   is_scalar = FALSE,
                                   allow_na = TRUE,
                                   check_names = FALSE,
                                   is_whole = FALSE,
                                   throw_err = TRUE, ...) {

  expect_data_type(
    obj = obj,
    type = 'numeric',
    not_empty = not_empty,
    is_scalar = is_scalar,
    allow_na = allow_na,
    check_names = check_names,
    throw_err = throw_err
  ) -> res

  if (isTRUE(is_whole)) {
    if (!isTRUE((obj %% 1) == 0)) {
      if (isTRUE(throw_err)) {
        message <- "`obj` must be whole number in call to `validate_input.numeric`"
        stop(message, call. = FALSE)
      } else {
        return(FALSE)
      }
    } else {
      return(isTRUE(res))
    }
  } else {
    return(isTRUE(res))
  }

}

#' S3 Method - Validate Format of List Inputs
#'
#' @param obj S3 Object
#' @param list_elem_type character
#' @param list_not_empty logical - (TRUE/FALSE)
#' @param elem_not_empty logical - (TRUE/FALSE)
#' @param elem_is_scalar logical - (TRUE/FALSE)
#' @param elem_allow_na logical - (TRUE/FALSE)
#' @param elem_check_names logical - (TRUE/FALSE)
#' @param throw_err logical - (TRUE/FALSE)
#' @param ... r ellipsis
#'
#' @return logical - (TRUE/FALSE)
#' @export
#'
#' @examples
#' \dontrun{
#' is_valid <- validate_input.list(obj = input, ...)
#' }
validate_input.list <- function(obj,
                                list_elem_type,
                                list_not_empty = TRUE,
                                elem_not_empty = FALSE,
                                elem_is_scalar = FALSE,
                                elem_allow_na = TRUE,
                                elem_check_names = FALSE,
                                throw_err = TRUE, ...) {

  # Validate Inputs
  if (missing(list_elem_type)) {stop("`list_elem_type` is missing in call to `validate_input.list`")}

  # Main Logic

  # * Validate List

  # ** `list_not_empty`
  if (!isTRUE(identical(list_not_empty, TRUE)) && !isTRUE(identical(list_not_empty, FALSE))) {
    stop("`list_not_empty` must be identical to TRUE/FALSE in call to `validate_input.list`")
  }

  if (isTRUE(list_not_empty)) {
    if (!isTRUE(length(obj) > 0)) {
      stop("`obj` must have non-zero length in call to `validate_input.list`")
    }
  }

  # * Validate List Elements

  # ** Check validity of each list element
  elem_valid <- purrr::map_lgl(obj, function(x) {

    expect_data_type(
      obj = x,
      type = list_elem_type,
      not_empty = elem_not_empty,
      is_scalar = elem_is_scalar,
      allow_na = elem_allow_na,
      check_names = elem_check_names,
      throw_err = throw_err
    )

  })

  # Reduce into single TRUE/FALSE value for entire list
  list_valid <- purrr::reduce(elem_valid, `&&`)

  # Return Result
  return(list_valid)

}
