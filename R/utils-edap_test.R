
#' Query UPC-to-Item Mappings from Google BigQuery 'EDAP - edw'
#'
#' @return data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' output <- get_edap_upc()
#' }
get_edap_upc <- function() {
  
  # Setup DB Connection
  conn_edw <- dbTools::bqsql_db_connect('edw')
  
  # Fetch SQL Query
  qry <- readr::read_file(system.file('sql/edw/dql/edap_upc.sql', package = 'fuzzyNgram'))
  
  # Execute Query / Fetch Results
  results <- DBI::dbGetQuery(conn_edw, qry)
  
  # Close DB Connection
  DBI::dbDisconnect(conn_edw)
  rm(conn_edw)
  
  # Return Results
  return(results)
  
} 

#' Query Item Details from Google BigQuery 'EDAP - edw'
#'
#' @return data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' output <- get_edap_item_code()
#' }
get_edap_item_code <- function() {
  
  # Setup DB Connection
  conn_edw <- dbTools::bqsql_db_connect('edw')
  
  # Fetch SQL Query
  qry <- readr::read_file(system.file('sql/edw/dql/edap_item_code.sql', package = 'fuzzyNgram'))
  
  # Execute Query / Fetch Results
  results <- DBI::dbGetQuery(conn_edw, qry)
  
  # Close DB Connection
  DBI::dbDisconnect(conn_edw)
  rm(conn_edw)
  
  # Return Results
  return(results)
  
}

#' Get Test UPC Data from Excel
#'
#' @return data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' output <- get_prod_upc()
#' }
get_prod_upc <- function() {
  
  # Read Data from Excel
  data <- readxl::read_excel(
    "~/AD_HOC/ONE_TIME/20230619_DLavnik_UPCFile/Prod_UPC_w_Map_Detailed.xlsx", 
    sheet = "prod_upc_table"
  )
  
  # Return Data
  return(data)
  
}
