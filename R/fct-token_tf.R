
token_tf <- function(t, d) {

  # Validate Inputs
  if (missing(t)) {stop("`t` missing in call to `token_tf`", call. = FALSE)}
  if (missing(d)) {stop("`d` missing in call to `token_tf`", call. = FALSE)}

  if (!isTRUE(valid_char(t)) || !isTRUE(length(t) == 1)) {stop("`t` must be a VALID CHAR vector of length 1 in call to `token_tf`", call. = FALSE)}
  if (!isTRUE(valid_char(d)) || !isTRUE(length(d) == 1)) {stop("`d` must be a VALID CHAR vector in call to `token_tf`", call. = FALSE)}

  # Process document(s)
  d <- token_tidy(d)
  token_data <- token_split(d)

}
