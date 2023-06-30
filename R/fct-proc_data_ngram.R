
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
#' @param schema character
#'
#' @return data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' dataset <- insert_ngram_dataset(data = edap_upc, schema = 'document')
#' }
insert_ngram_dataset <- function(data, schema) {
  
  # Validate Inputs 
  if (missing(data)) {stop("`data` is missing in call to `insert_ngram_dataset`")}
  if (missing(schema)) {schema <- 'document'} 
  
  # Validate Input Expectations 
  
  # * data 
  if (!isTRUE(is.data.frame(data)) || !isTRUE(nrow(data) > 0)) {
    stop("`data` must be data.frame in call to `insert_ngram_dataset`")
  }
  
  # * schema 
  if (!isTRUE(identical(schema, 'corpus')) && !isTRUE(identical(schema, 'document'))) {
    stop("`schema` must equal 'corpus' or 'document' in call to `insert_ngram_dataset`")
  }
  
  # MAIN LOGIC 
  
  # Capture Input Variable Name
  dataset_name <- deparse(substitute(data))
  
  # Setup DB Connection 
  conn <- dbTools::psql_db_connect('twm_ngram_dict')
  
  # Initialize Empty Results 
  result <- NULL
  
  # Upsert into `dataset` table
  tryCatch({
    
    # Generate `dataset_name` using input name w/ Timestamp
    time_str <- Sys.time()
    time_str <- gsub('-', '_', time_str, fixed = TRUE)
    time_str <- gsub(':', '_', time_str, fixed = TRUE)
    time_str <- gsub(' ', '_', time_str, fixed = TRUE)
    
    dataset_name <- paste(paste0('[', dataset_name, ']'), time_str, sep = '_')
    
    DBI::dbBegin(conn)
    
    # Upsert `dataset_name`, return `dataset_id` for later use
    dataset_id <- dbx::dbxUpsert(
      conn = conn, 
      table = DBI::Id(schema = schema, table = 'dataset'), 
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
#' @param schema character
#'
#' @return data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' column <- insert_ngram_column(dataset, column_names)
#' }
insert_ngram_column <- function(dataset, column_names, schema) {
  
  # Validate Inputs 
  if (missing(dataset)) {stop("`dataset` is missing in call to `insert_ngram_column`")}
  if (missing(column_names)) {stop("`column_names` is missing in call to `insert_ngram_column`")}
  if (missing(schema)) {schema <- 'document'} 
  
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
  
  # * schema 
  if (!isTRUE(identical(schema, 'corpus')) && !isTRUE(identical(schema, 'document'))) {
    stop("`schema` must equal 'corpus' or 'document' in call to `insert_ngram_column`")
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
      table = DBI::Id(schema = schema, table = 'column'), 
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
#' @param schema character
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' insert_ngram_record(dataset_id, row_count)
#' }
insert_ngram_record <- function(dataset_id, row_count, schema) {
  
  # Validate Inputs 
  if (missing(dataset_id)) {stop("`dataset_id` is missing in call to `insert_ngram_record`")}
  if (missing(row_count)) {stop("`row_count` is missing in call to `insert_ngram_record`")}
  if (missing(schema)) {schema <- 'document'} 
  
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
  
  # * schema 
  if (!isTRUE(identical(schema, 'corpus')) && !isTRUE(identical(schema, 'document'))) {
    stop("`schema` must equal 'corpus' or 'document' in call to `insert_ngram_record`")
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
        INSERT INTO {`schema`}.record 
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


#' Process N-Gram Data
#'
#' @param data data.frame 
#' @param schema character
#'
#' @return NULL
#' @export
#'
#' @examples 
#' \dontrun{
#'  process_ngram_data(data = edap_upc, schema = 'corpus')
#' }
process_ngram_data <- function(data, schema) {
  
  # Validate Inputs 
  if (missing(data)) {stop("`data` is missing in call to `process_ngram_data`")}
  if (missing(schema)) {schema <- 'document'} 
  
  # Validate Input Expectations 
  
  # * data 
  if (!isTRUE(is.data.frame(data)) || !isTRUE(nrow(data) > 0)) {
    stop("`data` must be data.frame in call to `process_ngram_data`")
  }
  
  # * schema 
  if (!isTRUE(identical(schema, 'corpus')) && !isTRUE(identical(schema, 'document'))) {
    stop("`schema` must equal 'corpus' or 'document' in call to `process_ngram_data`")
  }
  
  # MAIN LOGIC 
  
  # Upload Data to Temporary Table 
  cat(paste0("Uploading Data to PSQL Temp Table... "))
  tictoc::tic()
  tmp_data_table_name <- upload_tmp_tbl(data = data)
  tictoc::toc()
  
  # Initialize Temporary Table ID 
  tmp_data_tbl <- DBI::Id(schema = 'public', table = tmp_data_table_name)
  
  # Insert Into Dataset
  cat(paste0("Updating 'dataset' Table... "))
  tictoc::tic()
  dataset_info <- insert_ngram_dataset(data = data, schema = schema)
  tictoc::toc()
  
  # Insert Into Columns
  cat(paste0("Updating 'column' Table... "))
  tictoc::tic()
  column_info <- insert_ngram_column(
    dataset = dataset_info, 
    column_names = colnames(data), 
    schema = schema
  )
  tictoc::toc()
  
  # Insert Into Records
  cat(paste0("Updating 'record' Table... "))
  tictoc::tic()
  record_info <- insert_ngram_record(
    dataset_id = dataset_info$dataset_id, 
    row_count = nrow(data), 
    schema = schema
  )
  tictoc::toc()
  
  # Return Success 
  invisible(TRUE)
  
}

#' Process Data
#' 
#' @importFrom rlang .data
#' 
#' @param data data.frame
#' @param schema character
#'
#' @return list
#' @export
#'
#' @examples
#' \dontrun{
#' proc_data(data = edap_item_code)
#' }
proc_data <- function(data, schema) {
  
  # Validate Inputs 
  if (missing(data)) {stop("`data` is missing in call to `proc_data`")}
  if (missing(schema)) {schema <- 'document'} 
  
  # Validate Input Expectations 
  
  # * data 
  if (!isTRUE(is.data.frame(data)) || !isTRUE(nrow(data) > 0)) {
    stop("`data` must be data.frame in call to `proc_data`")
  }
  
  # * schema 
  if (!isTRUE(identical(schema, 'corpus')) && !isTRUE(identical(schema, 'document'))) {
    stop("`schema` must equal 'corpus' or 'document' in call to `proc_data`")
  }
  
  # Capture Input Variable Name
  dataset_name <- deparse(substitute(data))
  
  # Setup DB Connection 
  conn <- dbTools::psql_db_connect('twm_ngram_dict')
  
  # Initialize Empty Results 
  result <- NULL
  
  # MAIN LOGIC 
  
  tryCatch({
    
    # Begin Transaction 
    DBI::dbBegin(conn)
    
    # Generate `dataset_name` using input name w/ Timestamp
    time_str <- Sys.time()
    time_str <- gsub('-', '_', time_str, fixed = TRUE)
    time_str <- gsub(':', '_', time_str, fixed = TRUE)
    time_str <- gsub(' ', '_', time_str, fixed = TRUE)
    
    dataset_name <- paste(paste0('[', dataset_name, ']'), time_str, sep = '_')
    
    # Upsert `dataset_name`, return `dataset_id` for later use
    dataset_id <- dbx::dbxUpsert(
      conn = conn, 
      table = DBI::Id(schema = schema, table = 'dataset'), 
      records = data.frame(name = dataset_name), 
      where_cols = c('name'),
      returning = c('id')
    ) 
    
    if (!isTRUE(nrow(dataset_id) == 1)) {
      stop("`dataset_id` must have single row in call to `proc_data`")
    }
    
    # Store (`dataset_name`, `dataset_id`) for later use, using updated table contents
    dataset_id <- dataset_id %>% dplyr::pull(.data$id)
    
    # Use `dataset_id` along with column names to upsert(?) update `columns`
    data_colnames <- data.frame(
      dataset_id = dataset_id, 
      name = colnames(data), 
      stringsAsFactors = FALSE
    )
    
    column_id <- dbx::dbxUpsert(
      conn = conn, 
      table = DBI::Id(schema = schema, table = 'columns'), 
      records = data_colnames, 
      where_cols = c('dataset_id', 'name'),
      returning = c('id')
    ) 
    
    if (!isTRUE(nrow(column_id) > 0)) {
      stop("`column_id` must have non-zero row count in call to `proc_data`")
    }
    
    # Store mapping of (dataset_id, column_name)->id for later use, using updated table contents
    column_id <- column_id %>% dplyr::pull(.data$id)
    
    data_column_map <- DBI::dbGetQuery(
      conn = conn, 
      glue::glue_sql(
        "select dataset_id, name as column_name, id 
        from {`schema`}.columns 
        where id IN ({column_id*})
        ", 
        .con = conn
      )
    )
    
    if (!isTRUE(nrow(data_column_map) > 0)) {
      stop("`data_column_map` must have non-zero row count in call to `proc_data`")
    }
    
    # Use `dataset_id` along with dense (1:nrow) to upsert(?) update `records`
    data_records <- data.frame(
      dataset_id = dataset_id, 
      id = 1:nrow(data), 
      stringsAsFactors = FALSE
    )
    
    # Generate contents (dataset_id, record_id), using updated table contents
    dbx::dbxUpsert(
      conn = conn, 
      table = DBI::Id(schema = schema, table = 'records'), 
      records = data_records, 
      where_cols = c('dataset_id', 'id')
    ) 
    
    # Commit Transaction
    DBI::dbCommit(conn)
    
    # Store Mappings in `result`
    result <- list(
      dataset = list(dataset_id = dataset_id, dataset_name = dataset_name), 
      column = data_column_map, 
      records = data_records
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

#' Process NGRAMs
#' 
#' @importFrom rlang .data
#' 
#' @param data data.frame 
#' @param schema character
#' @param data_map list
#' @param data_token list
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' proc_ngram(data = edap_item_code)
#' }
proc_ngram <- function(data, schema, data_map, data_token) {
  
  # Validate Inputs 
  if (missing(data)) {stop("`data` is missing in call to `proc_ngram`")}
  if (missing(schema)) {schema <- 'document'} 
  if (missing(data_map)) {data_map <- NULL}
  if (missing(data_token)) {data_token <- NULL}
  
  # Validate Input Expectations 
  
  # * data 
  if (!isTRUE(is.data.frame(data)) || !isTRUE(nrow(data) > 0)) {
    stop("`data` must be data.frame in call to `proc_ngram`")
  }
  
  # * schema 
  if (!isTRUE(identical(schema, 'corpus')) && !isTRUE(identical(schema, 'document'))) {
    stop("`schema` must equal 'corpus' or 'document' in call to `proc_ngram`")
  }
  
  # Cleaning Input Text Columns
  cat(paste0("Cleaning Input Text Columns... \n"))
  tictoc::tic()
  data <- ngram_data_clean(data = data)
  tictoc::toc()
  
  # Upsert Contents of Specific Tables
  cat(paste0("\nUpsert Contents of Specific Tables... "))
  tictoc::tic()
  if (isTRUE(is.null(data_map))) {data_map <- proc_data(data = data, schema = schema)}
  tictoc::toc()
  
  # Generate Tokens from Input Data 
  # Use `df2ngrams` to generate gathered n-gram data from original data 
  cat(paste0("Generating Data Tokens... "))
  tictoc::tic()
  if (isTRUE(is.null(data_token))) {data_token <- df2ngrams(df = data)}
  tictoc::toc()
  
  # Use distinct ngram values to upsert(?) update `ngram_data`
  data_ngram <- purrr::map(names(data_token), function(x) {
    data_token[[x]] %>% purrr::pluck('ngrams') %>% 
      dplyr::mutate(column_name = x) %>% dplyr::relocate(.data$column_name)
  })
  
  data_ngram <- dplyr::bind_rows(data_ngram)
  
  # Cleaning Input Text Columns
  cat(paste0("Archive Intermediate Data Sets... "))
  tictoc::tic()
  saveRDS(data, "./inst/archive/rds/data.rds")
  saveRDS(data_map, "./inst/archive/rds/data_map.rds")
  saveRDS(data_token, "./inst/archive/rds/data_token.rds")
  saveRDS(data_ngram, "./inst/archive/rds/data_ngram.rds")
  tictoc::toc()
  
  # Setup DB Connection 
  conn <- dbTools::psql_db_connect('twm_ngram_dict')
  
  # Store mapping of (ngram_value)->id for later use, using updated table contents 
  upload_ngram_data <- data_ngram %>% 
    dplyr::select(.data$str_value, .data$size) %>% 
    dplyr::distinct(.data$str_value, .keep_all = TRUE) %>% 
    dplyr::rename(value = .data$str_value, length = .data$size)
  
  dbx::dbxUpsert(
    conn = conn, 
    table = DBI::Id(schema = schema, table = 'ngram_data'), 
    records = upload_ngram_data, 
    where_cols = c('value'),
    returning = c('id', 'value')
  ) -> ngram_data_map
  
  # Generate ID Columns
  ngram_upload <- data_ngram %>%
    dplyr::rename(
      record_id = .data$orig_row_num,
      value = .data$str_value
    ) %>%
    dplyr::left_join(
      data_map$column %>% dplyr::rename(column_id = .data$id)
      , by = 'column_name'
    ) %>%
    dplyr::select(
      .data$dataset_id,
      .data$column_id,
      .data$record_id,
      .data$value
    ) %>%
    dplyr::left_join(
      ngram_data_map %>% dplyr::rename(ngram_id = .data$id)
      , by = 'value'
    ) %>% 
    dplyr::select(-.data$value)
  
  # Upsert `ngram` table
  dbx::dbxUpsert(
    conn = conn, 
    table = DBI::Id(schema = schema, table = 'ngram'), 
    records = ngram_upload, 
    where_cols = c('dataset_id', 'column_id', 'record_id')
  )
  
  # Return NULL
  invisible(NULL)
  
}
