
#' Connect to Google BigQuery Database
#'
#' @param db character - Name of an existing database
#'
#' @return DBIConnection - R Object handle for the newly opened connection
#' @export
#'
#' @examples
#' \dontrun{
#' conn <- gsql_db_connect('edw')
#' }
gsql_db_connect <- function(db) {

  # Validate Inputs
  if (missing(db)) {stop("`db` is missing in call to `gsql_db_connect`", call. = FALSE)}

  # Initialize Global Settings
  project <- 'twm-edap-prod-1802'
  billing <- 'twm-edap-prod-1802'

  # Validate Input Expectations

  # * `db`
  input_valid <- isTRUE(all(
    purrr::map_lgl(
      list(db, project, billing),
      ~ isTRUE(is.character(.)) && isTRUE(length(.) == 1) && isTRUE(!is.na(.))
    )
  ))
  if (!isTRUE(input_valid)) {stop("invalid inputs in call to `gsql_db_connect`", call. = FALSE)}

  # Setup Connection
  conn <- DBI::dbConnect(
    bigrquery::bigquery(),
    dataset = db,
    project = project,
    billing = billing
  )

  # Return Connection
  return(conn)

}
