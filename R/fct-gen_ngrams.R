
#' gen List of N-Grams from a List of Tokens (stored as character vectors)
#' 
#' @importFrom rlang .data
#' 
#' @param tokens character
#' @param n integer
#' @param split character
#'
#' @return list
#' @export
#'
#' @examples
#' \dontrun{
#'  output_ngrams <- gen_ngrams(tokens = token_list, n = 3, split = ' ')
#' }
gen_ngrams <- function(tokens, n = 3, split = ' ') {

  # Validate Inputs
  if (missing(tokens)) {stop("`tokens` is missing in call to `gen_ngrams`")}
  if (missing(n)) {n <- 3}
  if (missing(split)) {split <- ' '}

  # Validate Input Expectations

  # * tokens
  if (!isTRUE(validate_input.list(obj = tokens, list_elem_type = 'character', list_non_empty = TRUE, throw_err = FALSE))) {
    stop("`x` must be a valid list with character vector elements in call to `gen_ngrams`")
  }

  # * n
  n <- as.numeric(n)
  if (!isTRUE(validate_input.numeric(obj = n, is_scalar = TRUE, is_whole = TRUE)) || !isTRUE(n > 0)) {
    stop("`n` must be positive whole number in call to `gen_ngrams`")
  }

  # * split
  if (!isTRUE(validate_input.character(obj = split, is_scalar = TRUE))) {
    stop("`split` must be length 1 character vector in call to `gen_ngrams`")
  }

  # Main Logic
  
  # Group `tokens` by TOKEN_COUNT
  tokens_by_count <- split(tokens, purrr::map_dbl(tokens, function(t) {length(t)}))
  
  # Save Distinct 'TOKEN_COUNT' Values for Later Use
  unq_token_counts <- as.character(unique(names(tokens_by_count)))

  # * Generate Start/End Indices for NGRAM generation
  indicies <- list()
  n <- min(as.numeric(max(as.numeric(unq_token_counts), na.rm = TRUE)), n)
  #unq_token_counts <- as.character(seq(1, n))

  for (m in 1:n) {
    for (i in 1:(n-m+1)) {
      indicies[[length(indicies)+1]] <- c(i,i+m-1)
    }
  }

  # Generate NGRAM Index Names
  index_names <- purrr::map_chr(indicies, function(x) {
    if (!isTRUE(x[1] == x[2])) {
      return(paste(x[1], x[2], sep = '-'))
    } else {
      return(as.character(x[1]))
    }
  })

  names(indicies) <- index_names
  
  # Group Indicies by Max Index Value
  indicies_by_maxval <- split(indicies, purrr::map_dbl(indicies, function(t) {max(t)}))

  # Generate NGRAM Length Lookup Vector
  ngram_length <- purrr::map_dbl(indicies, function(x) {
    if (!isTRUE(x[1] == x[2])) {
      return(abs(x[2]-x[1])+1)
    } else {
      return(1)
    }
  })

  names(ngram_length) <- index_names
  
  # Create Lookup List of `names(indicies)` by common NGRAM length
  ngram_length_lookup <- split(names(ngram_length), ngram_length)
  
  # * Use Indicies to Generate NGRAMs
  ngrams <- purrr::map(unq_token_counts, function(token_count) {
    
    # Get All Tokens with Length 'token_count'
    tokens_x <- tokens_by_count[[token_count]]
    
    # Get All Indicies with 'maxval' <= 'token_count'
    valid_maxvals <- as.numeric(unq_token_counts)
    valid_maxvals <- valid_maxvals[valid_maxvals <= as.numeric(token_count)]
    valid_maxvals <- as.character(valid_maxvals)
    
    indicies_x <- indicies_by_maxval[valid_maxvals]
    
    # Unnest 'indicies_x' into list with single level
    name_indicies_x <- purrr::reduce(purrr::map(indicies_x, function(t){names(t)}), `c`)
    val_indicies_x <- purrr::reduce(indicies_x, `c`)
    names(val_indicies_x) <- name_indicies_x
    
    indicies_x <- val_indicies_x
    
    rm(name_indicies_x)
    rm(val_indicies_x)
    
    # Apply Selected Indicies to Selected Tokens
    ngrams_x <- purrr::map(indicies_x, function(index_val) {
      
      # Generate sequence used for subsetting `tokens` elements
      sub_seq <- seq(index_val[1], index_val[2])
      
      # Use `sub_seq` to subset all character vectors in `tokens`
      res <- purrr::map(tokens_x, function(t) {return(t[sub_seq])})
      return(res)
      
    })
    
    names(ngrams_x) <- names(indicies_x)
    
    # Return `ngrams_x`
    return(ngrams_x)
    
  })
  
  names(ngrams) <- unq_token_counts
  
  # * Convert List Elements of 'ngrams' to DataFrames
  ngrams <- purrr::map(ngrams, function(x) {
    
    # Save 'names(x)' for later use
    names_x <- names(x)
    
    frames_x <- purrr::map(names(x), function(t) {
      
      x_t <- x[[t]]
      
      return(data.frame(
        orig_row_num = names(x_t),
        position = t, 
        list_value = I(x_t), 
        str_value = purrr::map_chr(I(x_t), function(x){paste(x, collapse = split)}),
        stringsAsFactors = FALSE
      ))
      
    })
    names(frames_x) <- names_x
    
    # Return 'frames_x'
    return(frames_x)
    
  })
  
  # Coalesce `ngrams`
  ngrams <- purrr::map(ngrams, function(x) {
    purrr::reduce(x, `rbind`)
  }) %>% 
    purrr::reduce(`rbind`)
  
  # Convert 'ngrams$orig_row_num' to numeric
  ngrams$orig_row_num <- as.numeric(ngrams$orig_row_num)
  
  # Ensure All Rows are Unique
  ngrams <- ngrams %>% 
    dplyr::distinct(
      .data$orig_row_num, 
      .data$position, 
      .keep_all = TRUE
    )
  
  # Add 'NGRAM Size' as New Column
  ngrams <- ngrams %>% 
    dplyr::mutate(size = purrr::map_dbl(.data$list_value, function(t){length(t)}))
  
  # Add 'Original Text' as New Column
  ngrams_max_size <- ngrams %>% 
    dplyr::group_by(.data$orig_row_num) %>% 
    dplyr::summarise(size = max(.data$size)) %>% 
    dplyr::ungroup()
  
  ngram_orig_text <- ngrams %>% 
    dplyr::semi_join(
      ngrams_max_size, 
      by = c('orig_row_num', 'size')
    ) %>% 
    dplyr::mutate(orig_text = purrr::map_chr(.data$list_value, function(t){paste0(t, collapse = split)})) %>% 
    dplyr::rename(orig_size = .data$size) %>% 
    dplyr::select(.data$orig_row_num, .data$orig_text, .data$orig_size)
  
  ngrams <- ngrams %>% 
    dplyr::left_join(ngram_orig_text, by = 'orig_row_num')
  
  # Order Rows in 'ngrams'
  ngrams <- ngrams %>% 
    dplyr::arrange(.data$orig_row_num, .data$size, .data$position)
  
  # Remove Pre-Existing Row Names from 'ngrams'
  rownames(ngrams) <- NULL

  # Return `ngrams`
  return(ngrams)

}
