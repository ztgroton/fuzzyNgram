
#' Kill All DB Connections to 'twm_ngram_dict'
#'
#' @return R Object
#' @export
#'
#' @examples
#' \dontrun{
#' kill_db_connections()
#' }
kill_db_connections <- function() {
  
  # Setup DB Connection
  conn <- dbTools::psql_db_connect('postgres')
  
  # Initialize Empty Results 
  results <- NULL
  
  tryCatch({
    
    results <- DBI::dbGetQuery(
      conn = conn, 
      glue::glue_sql(
        "SELECT pg_terminate_backend(pg_stat_activity.pid)
         FROM pg_stat_activity
         WHERE pg_stat_activity.datname = 'twm_ngram_dict' 
         AND pid <> pg_backend_pid();"
        , .con = conn
      )
    )
    
  }, error = function(e) {
    results <<- NULL
    message(e)
  })
  
  # Close DB Connection 
  DBI::dbDisconnect(conn)
  rm(conn)
  
  # Return Results
  invisible(results)
  
}

#' Destroy 'TWM N-Gram Dictionary' PSQL Database
#'
#' @return TRUE/FALSE
#' @export
#'
#' @examples
#' \dontrun{
#' destroy_twm_ngram_db()
#' status <- destroy_twm_ngram_db()
#' }
destroy_twm_ngram_db <- function() {
  
  # Setup DB Connection 
  conn <- dbTools::psql_db_connect('postgres')
  
  # Initialize FALSE Status Message
  result_status <- FALSE
  
  # Conditionally Initialize New DB
  tryCatch({
    
    # Check if database already exists
    does_db_exist <- DBI::dbGetQuery(conn, "SELECT datname FROM pg_catalog.pg_database WHERE datname='twm_ngram_dict'")
    does_db_exist <- isTRUE(nrow(does_db_exist) > 0)
    
    if (isTRUE(does_db_exist)) {
      suppressMessages({DBI::dbExecute(conn, "DROP DATABASE twm_ngram_dict")})
    } else {
      message("Database 'twm_ngram_dict' does not exists - NO ACTION TAKEN")
    }
    
    result_status <- TRUE
    
  }, error = function(e) {
    result_status <<- FALSE
    message(e)
  })
  
  # Close DB Connection 
  DBI::dbDisconnect(conn)
  rm(conn)
  
  # Return Result Status
  invisible(result_status)
  
}

#' Initialize 'TWM N-Gram Dictionary' PSQL Database
#'
#' @return TRUE/FALSE
#' @export
#'
#' @examples
#' \dontrun{
#' init_twm_ngram_db()
#' status <- init_twm_ngram_db()
#' }
init_twm_ngram_db <- function() {
  
  # Setup DB Connection 
  conn <- dbTools::psql_db_connect('postgres')
  
  # Initialize FALSE Status Message
  result_status <- FALSE
  
  # Conditionally Initialize New DB
  tryCatch({
    
    # Check if database already exists
    does_db_exist <- DBI::dbGetQuery(conn, "SELECT datname FROM pg_catalog.pg_database WHERE datname='twm_ngram_dict'")
    does_db_exist <- isTRUE(nrow(does_db_exist) > 0)
    
    if (!isTRUE(does_db_exist)) {
      suppressMessages({DBI::dbExecute(conn, "CREATE DATABASE twm_ngram_dict")})
    } else {
      message("Database 'twm_ngram_dict' already exists - NO ACTION TAKEN")
    }
    
    result_status <- TRUE
    
  }, error = function(e) {
    result_status <<- FALSE
    message(e)
  })
  
  # Close DB Connection 
  DBI::dbDisconnect(conn)
  rm(conn)
  
  # Return Result Status
  invisible(result_status)
  
}

#' Initialize 'TWM N-Gram Dictionary' PSQL Schemas
#'
#' @return TRUE/FALSE
#' @export
#'
#' @examples
#' \dontrun{
#' init_twm_ngram_schema()
#' status <- init_twm_ngram_schema()
#' }
init_twm_ngram_schema <- function() {
  
  # Setup DB Connection 
  conn <- dbTools::psql_db_connect('twm_ngram_dict')
  
  # Initialize FALSE Status Message
  result_status <- FALSE
  
  # Conditionally Initialize New DB
  tryCatch({
    
    suppressMessages({DBI::dbExecute(conn, "CREATE SCHEMA IF NOT EXISTS corpus")})
    suppressMessages({DBI::dbExecute(conn, "CREATE SCHEMA IF NOT EXISTS document")})
    
    result_status <- TRUE
    
  }, error = function(e) {
    result_status <<- FALSE
    message(e)
  })
  
  # Close DB Connection 
  DBI::dbDisconnect(conn)
  rm(conn)
  
  # Return Result Status
  invisible(result_status)
  
} 

#' Initialize 'TWM N-Gram Dictionary' PSQL Tables
#' 
#' @importFrom rlang .data 
#' 
#' @return logical
#' @export
#'
#' @examples
#' \dontrun{
#' init_twm_ngram_tables()
#' status <- init_twm_ngram_tables()
#' }
init_twm_ngram_tables <- function() {
  
  # Setup DB Connection 
  conn <- dbTools::psql_db_connect('twm_ngram_dict')
  
  # Setup 'ngram_corpus_tables'
  ngram_corpus_tables <- fuzzyNgram::ngram_corpus_tables %>% dplyr::arrange(.data$create_order)
  ngram_corpus_rows <- nrow(ngram_corpus_tables)
  
  if (!isTRUE(ngram_corpus_rows > 0)) {
    stop("`nrow(fuzzyNgram::ngram_corpus_tables)` must be non-zero in call to `init_twm_ngram_tables`")
  }
  
  table_status <- vector(mode = 'logical', length = ngram_corpus_rows)
  
  # Conditionally Initialize Schema Tables
  cat(paste0("\nInitializing Tables... \n"))
  tictoc::tic()
  
  table_status <- purrr::map_lgl(1:ngram_corpus_rows, function(i) {
    
    tryCatch({
      
      # Store Table Name
      table_name <- fuzzyNgram::ngram_corpus_tables$name[i]
      cat(paste0("Processing '", table_name, "'... "))
      tictoc::tic()
      
      # Initialize SQL DDL File Path
      ddl_file_path <- system.file(paste0('sql/psql/ddl/create_table/', table_name, '.sql'), package = 'fuzzyNgram')
      
      # Fetch SQL DDL Statement
      qry_ddl <- readr::read_file(ddl_file_path)
      
      # Execute DDL Statement 
      suppressMessages({DBI::dbExecute(conn, qry_ddl)})
      tictoc::toc()
      
      return(TRUE)
      
    }, error = function(e) {
      message(e)
      return(FALSE)
    })
    
  })
  names(table_status) <- fuzzyNgram::ngram_corpus_tables$name
  tictoc::toc()
  
  # Return Result Status
  invisible(table_status)
  
} 

#' Initialize 'TWM N-Gram Dictionary' PSQL Indexes
#' 
#' @importFrom rlang .data 
#' 
#' @return logical
#' @export
#'
#' @examples
#' \dontrun{
#' init_twm_ngram_indexes()
#' status <- init_twm_ngram_indexes()
#' }
init_twm_ngram_indexes <- function() {
  
  # Setup DB Connection 
  conn <- dbTools::psql_db_connect('twm_ngram_dict')
  
  # MAIN LOGIC 
  
  # Initialize SQL DDL Folder Path 
  ddl_folder_path <- system.file(paste0('sql/psql/ddl/create_index/'), package = 'fuzzyNgram') 
  
  # List SQL DDL Folder Contents
  ddl_folder_contents <- list.files(path = ddl_folder_path, pattern = '.sql')
  
  if (!isTRUE(length(ddl_folder_contents) > 0)) {
    stop("`ddl_folder_contents` must have non-zero length in call to `init_twm_ngram_indexes`")
  }
  
  # Setup 'ngram_corpus_indexes' 
  index_status <- vector(mode = 'logical', length = length(ddl_folder_contents))
  
  # Conditionally Initialize Schema Tables
  cat(paste0("\nInitializing Indexes... \n"))
  tictoc::tic()
  
  index_status <- purrr::map_lgl(ddl_folder_contents, function(x) {
    
    tryCatch({
      
      # Store Table Name
      cat(paste0("Processing '", x, "'... "))
      tictoc::tic()
      
      # Initialize SQL DDL File Path
      ddl_file_path <- file.path(ddl_folder_path, x)
      
      # Fetch SQL DDL Statement
      qry_ddl <- readr::read_file(ddl_file_path)
      
      # Execute DDL Statement 
      suppressMessages({DBI::dbExecute(conn, qry_ddl)})
      tictoc::toc()
      
      return(TRUE)
      
    }, error = function(e) {
      message(e)
      return(FALSE)
    })
    
  })
  names(index_status) <- ddl_folder_contents
  tictoc::toc()
  
  # Return Result Status
  invisible(index_status)
  
}

#' Initialize 'TWM N-Gram Dictionary' PSQL Functions
#' 
#' @importFrom rlang .data 
#' 
#' @return logical
#' @export
#'
#' @examples
#' \dontrun{
#' init_twm_ngram_func()
#' status <- init_twm_ngram_func()
#' }
init_twm_ngram_func <- function() {
  
  # Setup DB Connection 
  conn <- dbTools::psql_db_connect('twm_ngram_dict')
  
  # MAIN LOGIC 
  
  # Initialize SQL DDL Folder Path 
  ddl_folder_path <- system.file(paste0('sql/psql/ddl/create_func/'), package = 'fuzzyNgram') 
  
  # List SQL DDL Folder Contents
  ddl_folder_contents <- list.files(path = ddl_folder_path, pattern = '.sql')
  
  if (!isTRUE(length(ddl_folder_contents) > 0)) {
    stop("`ddl_folder_contents` must have non-zero length in call to `init_twm_ngram_func`")
  }
  
  # Setup 'ngram_corpus_indexes' 
  index_status <- vector(mode = 'logical', length = length(ddl_folder_contents))
  
  # Conditionally Initialize Stored Functions
  cat(paste0("\nInitializing Functions... \n"))
  tictoc::tic()
  
  index_status <- purrr::map_lgl(ddl_folder_contents, function(x) {
    
    tryCatch({
      
      # Store Table Name
      cat(paste0("Processing '", x, "'... "))
      tictoc::tic()
      
      # Initialize SQL DDL File Path
      ddl_file_path <- file.path(ddl_folder_path, x)
      
      # Fetch SQL DDL Statement
      qry_ddl <- readr::read_file(ddl_file_path)
      
      # Execute DDL Statement 
      suppressMessages({DBI::dbExecute(conn, qry_ddl)})
      tictoc::toc()
      
      return(TRUE)
      
    }, error = function(e) {
      message(e)
      return(FALSE)
    })
    
  })
  names(index_status) <- ddl_folder_contents
  tictoc::toc()
  
  # Return Result Status
  invisible(index_status)
  
}

#' Initialize 'TWM N-Gram Dictionary' PSQL Procedures
#' 
#' @importFrom rlang .data 
#' 
#' @return logical
#' @export
#'
#' @examples
#' \dontrun{
#' init_twm_ngram_proc()
#' status <- init_twm_ngram_proc()
#' }
init_twm_ngram_proc <- function() {
  
  # Setup DB Connection 
  conn <- dbTools::psql_db_connect('twm_ngram_dict')
  
  # MAIN LOGIC 
  
  # Initialize SQL DDL Folder Path 
  ddl_folder_path <- system.file(paste0('sql/psql/ddl/create_proc/'), package = 'fuzzyNgram') 
  
  # List SQL DDL Folder Contents
  ddl_folder_contents <- list.files(path = ddl_folder_path, pattern = '.sql')
  
  if (!isTRUE(length(ddl_folder_contents) > 0)) {
    stop("`ddl_folder_contents` must have non-zero length in call to `init_twm_ngram_proc`")
  }
  
  # Setup 'ngram_corpus_indexes' 
  index_status <- vector(mode = 'logical', length = length(ddl_folder_contents))
  
  # Conditionally Initialize Stored Functions
  cat(paste0("\nInitializing Procedures... \n"))
  tictoc::tic()
  
  index_status <- purrr::map_lgl(ddl_folder_contents, function(x) {
    
    tryCatch({
      
      # Store Table Name
      cat(paste0("Processing '", x, "'... "))
      tictoc::tic()
      
      # Initialize SQL DDL File Path
      ddl_file_path <- file.path(ddl_folder_path, x)
      
      # Fetch SQL DDL Statement
      qry_ddl <- readr::read_file(ddl_file_path)
      
      # Execute DDL Statement 
      suppressMessages({DBI::dbExecute(conn, qry_ddl)})
      tictoc::toc()
      
      return(TRUE)
      
    }, error = function(e) {
      message(e)
      return(FALSE)
    })
    
  })
  names(index_status) <- ddl_folder_contents
  tictoc::toc()
  
  # Return Result Status
  invisible(index_status)
  
}

#' Generate Random Table Name
#'
#' @param use_date logical
#'
#' @return character
#' @export
#'
#' @examples
#' \dontrun{
#' output <- gen_unq_name(use_date = TRUE)
#' }
gen_unq_name <- function(use_date = TRUE) {
  
  # Validate Inputs
  if (missing(use_date)) {use_date <- TRUE}
  
  # Validate Input Expectations
  if (!isTRUE(identical(use_date, TRUE)) && !isTRUE(identical(use_date, FALSE))) {
    stop("`use_date` must be identical to TRUE/FALSE in call to `gen_unq_name`")
  }
  
  # Conditionally Generate Formatted TimeStamp String
  if (isTRUE(use_date)) {
    time_str <- as.POSIXct(Sys.time())
    time_str <- gsub(' ', '_', time_str, fixed = TRUE)
    time_str <- gsub(':', '_', time_str, fixed = TRUE)
    time_str <- gsub('-', '_', time_str, fixed = TRUE)
  }
  
  # Generate Random String of Length 5
  letter_str <- paste(
    sample(c(0:9, letters, LETTERS), 5, replace = TRUE)
    , collapse = ""
  )
  
  # Generate Final String
  if (isTRUE(use_date)) {
    final_str <- paste('tbl', letter_str, time_str, sep = '_')
  } else {
    final_str <- paste('tbl', letter_str, sep = '_')
  }
  
  # Return Final String
  return(final_str)
  
}

#' Upload R Data.Frame to PSQL Table in 'public' schema
#'
#' @param data data.frame 
#'
#' @return character
#' @export
#'
#' @examples
#' \dontrun{
#' upload_tbl_name <- upload_tmp_tbl(data = raw_data)
#' }
upload_tmp_tbl <- function(data) {
  
  # Validate Inputs
  if (missing(data)) {stop("`data` is missing in call to `upload_tmp_tbl`")}
  
  # Validate Input Expectations
  
  # * `data`
  if (!isTRUE(is.data.frame(data))) {
    stop("`data` must be data.frame in call to `upload_tmp_tbl`")
  }
  
  # MAIN LOGIC
  
  # Generate Random Table Name
  tbl_name <- gen_unq_name()
  
  # Initialize File Upload Table ID
  tbl_id <- DBI::Id(schema = 'public', table = tbl_name)
  
  # Generate Table Hash
  tbl_hash <- digest::digest(object = tbl_id, algo = 'sha256', serialize = TRUE)
  
  # Setup DB Connection
  psql_conn <- dbTools::psql_db_connect('twm_ngram_dict')
  
  tryCatch({
    
    # Begin Transaction 
    DBI::dbBegin(psql_conn)
    
    # Create Empty Table
    DBI::dbCreateTable(
      conn = psql_conn,
      name = tbl_id,
      fields = data
    )
    
    # Upload Data
    dbx::dbxInsert(
      conn = psql_conn,
      table = tbl_id,
      records = data
    )
    
    # Commit Transaction
    DBI::dbCommit(psql_conn)
    
  }, error = function(e) {
    
    # Rollback Transaction 
    DBI::dbRollback(psql_conn)
    
    # Set `tbl_name` to NULL
    tbl_name <<- NULL
    
    # Error Message
    message(sprintf("Error in %s: %s", deparse(e[["call"]]), e[["message"]]))
    
  })
  
  # Close DB Connection
  DBI::dbDisconnect(psql_conn)
  rm(psql_conn)
  
  # Return `tbl_name` 
  return(tbl_name)
  
}

#' Drop Temporary Table from Public Schema
#'
#' @param tbl character
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' drop_tmp_tbl(tbl = 'tbl_fasdvjcaf')
#' }
drop_tmp_tbl <- function(tbl) {
  
  # Validate Inputs
  if (missing(tbl)) {stop("`tbl` is missing in call to `upload_tmp_tbl`")}
  
  # Validate Input Expectations
  
  # * `tbl`
  if (!isTRUE(is.character(tbl)) || !isTRUE(length(tbl) == 1) || isTRUE(is.na(tbl))) {
    stop("`tbl` must be character vector of length 1 in call to `drop_tmp_tbl`")
  }
  
  # MAIN LOGIC
  
  # Setup DB Connection 
  conn <- dbTools::psql_db_connect('twm_ngram_dict')
  
  # Initialize Table ID
  tbl_id <- DBI::Id(schema = 'public', table = tbl)
  
  if (isTRUE(DBI::dbExistsTable(conn, tbl_id))) {
    DBI::dbExecute(conn, glue::glue_sql("DROP TABLE public.{`tbl`}", .con = conn))
  } else {
    message(paste0("Table '", tbl, "' does not exist - NO ACTION TAKEN"))
  }
  
  # Close DB Connection
  DBI::dbDisconnect(conn)
  rm(conn)
  
  # Return NULL
  invisible(NULL)
  
}

#' Rebuild DB 
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#'  clean_slate_db()
#' }
clean_slate_db <- function() {
  
  # Kill All DB Connections
  kill_db_connections()
  
  # Destroy DB (if exists)
  destroy_twm_ngram_db()
  
  # Initialize Empty DB 
  init_twm_ngram_db()
  
  # Initialize DB Schemas
  init_twm_ngram_schema()
  
  # Initialize DB Tables
  init_twm_ngram_tables()
  
  # Initialize DB Indexes
  init_twm_ngram_indexes()
  
  # Initialize DB Functions and Procedures
  init_twm_ngram_func()
  init_twm_ngram_proc()
  
  # Return NULL
  invisible(NULL)
  
}
