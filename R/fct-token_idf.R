
token_idf <- function(t, D) {

  # Validate Inputs
  if (missing(t)) {stop("`t` missing in call to `token_idf`", call. = FALSE)}
  if (missing(D)) {stop("`D` missing in call to `token_idf`", call. = FALSE)}

  if (!isTRUE(valid_char(t)) || !isTRUE(length(t) == 1)) {stop("`t` must be a VALID CHAR vector of length 1 in call to `token_idf`", call. = FALSE)}
  if (!isTRUE(valid_char(D))) {stop("`D` must be a VALID CHAR vector in call to `token_idf`", call. = FALSE)}

}
