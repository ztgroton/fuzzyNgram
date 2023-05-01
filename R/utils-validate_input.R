
#' S3 Generic - Validate Format of Function Inputs
#'
#' @param obj S3 Object
#' @param ... r ellipsis
#'
#' @return logical - (TRUE/FALSE)
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
#'
#' @examples
#' \dontrun{
#' is_valid <- validate_input.character(obj = input, ...)
#' }
validate_input.character <- function(obj, not_empty = FALSE, is_scalar = FALSE, allow_na = FALSE, check_names = FALSE, throw_err = TRUE, ...) {

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
#'
#' @examples
#' \dontrun{
#' is_valid <- validate_input.logical(obj = input, ...)
#' }
validate_input.logical <- function(obj, not_empty = FALSE, is_scalar = FALSE, allow_na = FALSE, check_names = FALSE, throw_err = TRUE, ...) {

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
#' @param throw_err logical - (TRUE/FALSE) specifies if function should return or throw error
#' @param ... r ellipsis
#'
#' @return logical - (TRUE/FALSE)
#'
#' @examples
#' \dontrun{
#' is_valid <- validate_input.numeric(obj = input, ...)
#' }
validate_input.numeric <- function(obj, not_empty = FALSE, is_scalar = FALSE, allow_na = FALSE, check_names = FALSE, throw_err = TRUE, ...) {

  expect_data_type(
    obj = obj,
    type = 'numeric',
    not_empty = not_empty,
    is_scalar = is_scalar,
    allow_na = allow_na,
    check_names = check_names,
    throw_err = throw_err
  )

}
