
#' Clean Data Text Columns
#' 
#' @importFrom rlang .data
#' 
#' @param data data.frame
#'
#' @return data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' ngram_data_clean(data = edap_item_code)
#' }
ngram_data_clean <- function(data) {
  
  # Validate Inputs 
  if (missing(data)) {stop("`data` is missing in call to `ngram_data_clean`")}
  
  # Validate Input Expectations 
  
  # * data 
  if (!isTRUE(is.data.frame(data))) {
    stop("`data` must be data.frame in call to `ngram_data_clean`")
  }
  
  # MAIN LOGIC 
  
  # Get Data Types for Columns 
  col_types <- purrr::map_chr(data, function(x) {return(class(x)[[1]])})
  names(col_types) <- colnames(data)
  
  # Get Character Columns 
  char_cols <- col_types[col_types == 'character']
  char_cols <- names(char_cols)
  
  # Iterate over Character Columns 
  for (char_col in char_cols) {
    
    cat(paste0("Processing Column '", char_col, "'... "))
    tictoc::tic()
    
    data[[char_col]] <- gsub("[^[:alnum:][:blank:]?&/\\-]", "", data[[char_col]])
    data[[char_col]] <- gsub("\\s+"," ", tolower(trimws(data[[char_col]])))
    
    tictoc::toc()
    
  }
  
  # Return Cleaned Data
  return(data)
  
}

#' Upsert Data into Dataset Table
#'
#' @param data data.frame
#' @param as_lib logical
#'
#' @return data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' dataset <- insert_ngram_dataset(data = edap_upc)
#' }
insert_ngram_dataset <- function(data, as_lib) {
  
  # Validate Inputs 
  if (missing(data)) {stop("`data` is missing in call to `insert_ngram_dataset`")}
  if (missing(as_lib)) {as_lib <- FALSE}
  
  # Validate Input Expectations 
  
  # * data 
  if (!isTRUE(is.data.frame(data)) || !isTRUE(nrow(data) > 0)) {
    stop("`data` must be data.frame in call to `insert_ngram_dataset`")
  }
  
  # * as_lib 
  if (!isTRUE(identical(as_lib, TRUE)) && !isTRUE(identical(as_lib, FALSE))) {
    stop("`as_lib` must equal TRUE/FALSE in call to `insert_ngram_dataset`")
  }
  
  # MAIN LOGIC 
  
  # Capture Input Variable Name
  dataset_name <- deparse(substitute(data)) 
  
  # Conditionally generate `dataset_name` using input name w/ Timestamp
  if (isFALSE(as_lib)) {
    
    time_str <- Sys.time()
    time_str <- gsub('-', '_', time_str, fixed = TRUE)
    time_str <- gsub(':', '_', time_str, fixed = TRUE)
    time_str <- gsub(' ', '_', time_str, fixed = TRUE)
    dataset_name <- paste(paste0('[', dataset_name, ']'), time_str, sep = '_')
    
  } 
  
  # Setup DB Connection 
  conn <- dbTools::psql_db_connect('twm_ngram_dict')
  
  # Initialize Empty Results 
  result <- NULL
  
  # Upsert into `dataset` table
  tryCatch({
    
    DBI::dbBegin(conn)
    
    # Upsert `dataset_name`, return `dataset_id` for later use
    dataset_id <- dbx::dbxUpsert(
      conn = conn, 
      table = DBI::Id(schema = 'corpus', table = 'dataset'), 
      records = data.frame(name = dataset_name), 
      where_cols = c('name'),
      returning = c('id', 'name')
    ) 
    
    if (!isTRUE(nrow(dataset_id) == 1)) {
      stop("`dataset_id` must have single row in call to `insert_ngram_dataset`")
    }
    
    DBI::dbCommit(conn)
    
    # Store Upserted Dataset Values to be Returned
    result <- dataset_id %>% 
      dplyr::rename(
        dataset_id = .data$id, 
        dataset_name = .data$name
      )
    
  }, error = function(e) {
    
    # Reset Result to NULL
    result <<- NULL
    
    # Rollback Transaction / Error Message
    DBI::dbRollback(conn)
    message(e)
    
  })
  
  # Close DB Connection 
  DBI::dbDisconnect(conn)
  rm(conn)
  
  # Return List of Mappings
  invisible(result)
  
}

#' Upsert Data into Column Table
#'
#' @param dataset data.frame
#' @param column_names character
#'
#' @return data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' column <- insert_ngram_column(dataset, column_names)
#' }
insert_ngram_column <- function(dataset, column_names) {
  
  # Validate Inputs 
  if (missing(dataset)) {stop("`dataset` is missing in call to `insert_ngram_column`")}
  if (missing(column_names)) {stop("`column_names` is missing in call to `insert_ngram_column`")}
  
  # Validate Input Expectations 
  
  # * dataset 
  if (
    !isTRUE(is.data.frame(dataset)) || 
    !isTRUE(nrow(dataset) == 1) || 
    !isTRUE(identical(colnames(dataset), c('dataset_id', 'dataset_name')))
  ) {
    stop("`dataset` must be valid data.frame in call to `insert_ngram_column`")
  }
  
  # * column_names 
  if (!isTRUE(is.character(column_names)) || !isTRUE(all(!is.na(column_names)))) {
    stop("`column_names` must be valid character vector in call to `insert_ngram_column`")
  }
  
  # MAIN LOGIC 
  
  # Setup DB Connection 
  conn <- dbTools::psql_db_connect('twm_ngram_dict')
  
  # Initialize Empty Results 
  result <- NULL
  
  tryCatch({
    
    # Compile DataFrame to Upsert
    upsert_data <- data.frame(
      dataset_id = rep(dataset$dataset_id, length(column_names)), 
      name = column_names, 
      stringsAsFactors = FALSE
    )
    
    DBI::dbBegin(conn)
    
    column_id <- dbx::dbxUpsert(
      conn = conn, 
      table = DBI::Id(schema = 'corpus', table = 'column'), 
      records = upsert_data, 
      where_cols = c('dataset_id', 'name'),
      returning = c('id', 'dataset_id', 'name')
    ) 
    
    DBI::dbCommit(conn)
    
    result <- column_id %>% 
      dplyr::rename(
        column_name = .data$name, 
        column_id = .data$id
      )
    
  }, error = function(e) {
    
    result <<- NULL
    
    # Rollback Transaction / Error Message
    DBI::dbRollback(conn)
    message(e)
    
  })
  
  # Close DB Connection 
  DBI::dbDisconnect(conn)
  rm(conn)
  
  # Return List of Mappings
  invisible(result)
  
}

#' Upsert Data into Record Table
#'
#' @param dataset_id numeric/integer
#' @param row_count numeric/integer
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' insert_ngram_record(dataset_id, row_count)
#' }
insert_ngram_record <- function(dataset_id, row_count) {
  
  # Validate Inputs 
  if (missing(dataset_id)) {stop("`dataset_id` is missing in call to `insert_ngram_record`")}
  if (missing(row_count)) {stop("`row_count` is missing in call to `insert_ngram_record`")}
  
  # Validate Input Expectations 
  
  # * dataset_id 
  if (
    !isTRUE(is.numeric(dataset_id) || is.integer(dataset_id)) || 
    !isTRUE(length(dataset_id) == 1) ||
    !isTRUE(!is.na(dataset_id) && !is.null(dataset_id))
  ) {
    stop("`dataset_id` must be valid data.frame in call to `insert_ngram_record`")
  }
  
  # * row_count 
  if (
    !isTRUE(is.numeric(row_count) || is.integer(row_count)) || 
    !isTRUE(length(row_count) == 1) ||
    !isTRUE(row_count > 0) ||
    !isTRUE(!is.na(row_count) && !is.null(row_count))
  ) {
    stop("`row_count` must be valid data.frame in call to `insert_ngram_record`")
  }
  
  # MAIN LOGIC 
  
  # Setup DB Connection 
  conn <- dbTools::psql_db_connect('twm_ngram_dict')
  
  # Generate Row-Level Records in Database
  tryCatch({
    
    DBI::dbBegin(conn)
    
    DBI::dbExecute(
      conn = conn, 
      glue::glue_sql(
        "
        INSERT INTO corpus.record 
        SELECT 
        {dataset_id} as dataset_id, 
        t.record_id 
        FROM 
        (
          SELECT * 
          FROM generate_series(1, {row_count}) as record_id
        ) t
        "
        , .con = conn
      )
    )
    
    DBI::dbCommit(conn)
    
  }, error = function(e) {
    
    # Rollback Transaction / Error Message
    DBI::dbRollback(conn)
    message(e)
    
  })
  
  # Close DB Connection 
  DBI::dbDisconnect(conn)
  rm(conn)
  
  # Return List of Mappings
  invisible(NULL)
  
}

#' Generate Temporary Table of N-Grams, Return Table Name
#'
#' @param table character
#'
#' @return character
#' @export
#'
#' @examples 
#' \dontrun{
#'  temp_table_name <- gen_ngram_data(table = 'tbl_gfdasgfdv')
#' }
gen_ngram_data <- function(table) {
  
  # Validate Inputs 
  if (missing(table)) {stop("`table` is missing in call to `gen_ngram_data`")}
  
  # Validate Input Expectations 
  
  # * table 
  if (!isTRUE(is.character(table)) || !isTRUE(length(table) == 1) || isTRUE(is.na(table))) {
    stop("`table` must be valid length 1 character in call to `gen_ngram_data`")
  }
  
  # MAIN LOGIC 
  
  # Setup DB Connection 
  conn <- dbTools::psql_db_connect('twm_ngram_dict')
  
  # Validate that 'table' exists in 'public' schema
  table_id <- DBI::Id(schema = 'public', table = table)
  
  if (!isTRUE(DBI::dbExistsTable(conn, table_id))) {
    stop("`table` does not exist as a table in public schema in call to `gen_ngram_data`")
  }
  
  # Initialize Empty Results
  result <- NULL
  
  tryCatch({
    
    DBI::dbBegin(conn)
    
    result <- DBI::dbGetQuery(
      conn = conn, 
      glue::glue_sql(
        "select * from public.gen_table_ngram({table}::TEXT)"
        , .con = conn
      )
    )
    
    if (!isTRUE(nrow(result) == 1)) {
      stop("`nrow(result)` must equal 1 in call to `gen_ngram_data`")
    } else if (!isTRUE(identical(colnames(result), c('gen_table_ngram')))) {
      stop("`colnames(result)` is invalid in call to `gen_ngram_data`")
    }
    
    result <- as.character(result$gen_table_ngram)
    
    DBI::dbCommit(conn)
    
  }, error = function(e) {
    
    result <<- NULL
    
    DBI::dbRollback(conn)
    message(e)
    
  })
  
  # Close DB Connection 
  DBI::dbDisconnect(conn)
  rm(conn)
  
  # Return Result
  return(result)
  
}

#' Upsert N-Gram Values and Return ID Mappings
#'
#' @param table character
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#'  upsert_ngram_data(table = table)
#' }
upsert_ngram_data <- function(table) {
  
  # Validate Inputs 
  if (missing(table)) {stop("`table` is missing in call to `upsert_ngram_data`")}
  
  # Validate Input Expectations 
  
  # * table 
  if (!isTRUE(is.character(table)) || !isTRUE(length(table) == 1) || isTRUE(is.na(table))) {
    stop("`table` must be valid length 1 character in call to `upsert_ngram_data`")
  }
  
  # MAIN LOGIC 
  
  # Setup DB Connection 
  conn <- dbTools::psql_db_connect('twm_ngram_dict')
  
  tryCatch({
    
    DBI::dbBegin(conn)
    
    DBI::dbExecute(
      conn = conn, 
      glue::glue_sql(
        "CALL public.upsert_tmp_ngram_data({table}::TEXT)"
        , .con = conn
      )
    )
    
    DBI::dbCommit(conn)
    
  }, error = function(e) {
    
    DBI::dbRollback(conn)
    message(e)
    
  })
  
  # Close DB Connection 
  DBI::dbDisconnect(conn)
  rm(conn)
  
  # Return NULL
  invisible(NULL)
  
} 

#' Upsert N-Gram Values and Return ID Mappings
#'
#' @param table character
#' @param dataset integer/numeric
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#'  upsert_ngram(table = table, dataset = 1)
#' }
upsert_ngram <- function(table, dataset) {
  
  # Validate Inputs 
  if (missing(table)) {stop("`table` is missing in call to `upsert_ngram`")}
  if (missing(dataset)) {stop("`dataset` is missing in call to `upsert_ngram`")}
  
  # Validate Input Expectations 
  
  # * table 
  if (!isTRUE(is.character(table)) || !isTRUE(length(table) == 1) || isTRUE(is.na(table))) {
    stop("`table` must be valid length 1 character in call to `upsert_ngram`")
  }
  
  # * dataset 
  if (
    !isTRUE(is.numeric(dataset) || is.integer(dataset)) || 
    !isTRUE(length(dataset) == 1) || isTRUE(is.na(dataset))
  ) {
    stop("`dataset` must be valid integer/numeric in call to `upsert_ngram`")
  }
  
  # MAIN LOGIC 
  
  # Setup DB Connection 
  conn <- dbTools::psql_db_connect('twm_ngram_dict')
  
  #public.upsert_tmp_ngram_data(ngram_table TEXT, schema TEXT)
  
  tryCatch({
    
    DBI::dbBegin(conn)
    
    DBI::dbExecute(
      conn = conn, 
      glue::glue_sql(
        "CALL public.upsert_ngram({table}::TEXT, {dataset}::INTEGER)"
        , .con = conn
      )
    )
    
    DBI::dbCommit(conn)
    
  }, error = function(e) {
    
    DBI::dbRollback(conn)
    message(e)
    
  })
  
  # Close DB Connection 
  DBI::dbDisconnect(conn)
  rm(conn)
  
  # Return NULL
  invisible(NULL)
  
}

#' Process N-Gram Data
#'
#' @param data data.frame 
#' @param as_lib logical
#'
#' @return data.frame
#' @export
#'
#' @examples 
#' \dontrun{
#' test <- process_ngram_data(data = edap_upc)
#' }
process_ngram_data <- function(data, as_lib) {
  
  # Validate Inputs 
  if (missing(data)) {stop("`data` is missing in call to `process_ngram_data`")}
  if (missing(as_lib)) {as_lib <- FALSE}
  
  # Validate Input Expectations 
  
  # * data 
  if (!isTRUE(is.data.frame(data)) || !isTRUE(nrow(data) > 0)) {
    stop("`data` must be data.frame in call to `process_ngram_data`")
  }
  
  # * as_lib 
  if (!isTRUE(identical(as_lib, TRUE)) && !isTRUE(identical(as_lib, FALSE))) {
    stop("`as_lib` must equal TRUE/FALSE in call to `process_ngram_data`")
  }
  
  # MAIN LOGIC 
  
  # Upload Data to Temporary Table 
  cat(paste0("Uploading Data to PSQL Temp Table... "))
  tictoc::tic()
  tmp_data_table_name <- upload_tmp_tbl(data = data)
  tictoc::toc()
  
  # Initialize Empty Results
  result <- NULL
  
  # Initialize Temporary Table ID 
  tmp_data_tbl <- DBI::Id(schema = 'public', table = tmp_data_table_name)
  
  # Insert Into Dataset
  cat(paste0("Updating 'dataset' Table... "))
  tictoc::tic()
  dataset_info <- insert_ngram_dataset(data = data, as_lib = as_lib)
  tictoc::toc()
  
  # Insert Into Columns
  cat(paste0("Updating 'column' Table... "))
  tictoc::tic()
  column_info <- insert_ngram_column(
    dataset = dataset_info, 
    column_names = colnames(data)
  )
  tictoc::toc()
  
  # Insert Into Records
  cat(paste0("Updating 'record' Table... "))
  tictoc::tic()
  record_info <- insert_ngram_record(
    dataset_id = dataset_info$dataset_id, 
    row_count = nrow(data)
  )
  tictoc::toc() 
  
  # Generate N-Gram Level Data from Uploaded Input Data
  cat(paste0("Generating N-Gram Data... "))
  tictoc::tic()
  ngram_data_table_name <- gen_ngram_data(table = tmp_data_table_name)
  tictoc::toc()
  
  # Upsert Distinct N-Gram Values into 'ngram_data' table
  cat(paste0("Upserting Distinct N-Gram Values... "))
  tictoc::tic()
  upsert_ngram_data(table = ngram_data_table_name)
  tictoc::toc()
  
  # Upsert into 'ngram' table
  cat(paste0("Upserting N-Gram Data... "))
  tictoc::tic()
  upsert_ngram(
    table = ngram_data_table_name,
    dataset = dataset_info$dataset_id
  )
  tictoc::toc()
  
  # Delete Temporary Tables 
  drop_tmp_tbl(tbl = tmp_data_table_name)
  drop_tmp_tbl(tbl = ngram_data_table_name)
  
  # Return NULL
  invisible(NULL)
  
}
