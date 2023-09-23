#' Create RTF Documents Based on Multiple Specifications
#'
#' This function generates RTF documents with tables based on multiple specifications provided.
#' It splits the data, applies the specifications, and then combines the output.
#'
#' @param data A data frame, the dataset to be used in the RTF document.
#' @param split_list List, defining the columns for each split of data.
#' @param specs_list List of lists, providing specifications for each subset of data. Each element of the list corresponds to a subset defined in \code{split_list}.
#' @param just_list List of vectors, defining the justification for each column in each subset of data.
#'
#' @return A character vector, where each element represents a line in the final RTF document.
#'
#' @examples
#' \dontrun{
#' # Simple Example Data
#' data_simple <- data.frame(
#'   label = c('', 'A', 'B', '', ''),
#'   col1 = c('', '10 (20.0)', '20 (40.0)', '$NEWPAGE$', '$NEWPAGE$'),
#'   col2 = c('', '5 (10.0)', '15 (30.0)', '', '')
#' )
#'
#' # Specifying Splits and Specifications
#' split_list_simple <- list(c(1, 2), 1, 2)
#' specs_list_simple <- list(
#'   list(list(label='Category', just='l', pct=50), list(label='Value (%)', just='c', pct=50)),
#'   list(list(label='Category', just='l', pct=50), list(label='Value', just='c', pct=50))
#' )
#' just_list_simple <- list(c('l', 'c'), c('l', 'c'))
#'
#' # Creating RTF Document
#' out_rtf_txt_simple <- create_rtf_multiple_specs(
#'   data = data_simple,
#'   split_list = split_list_simple,
#'   specs_list = specs_list_simple,
#'   just_list = just_list_simple
#' )
#' }
#' @export
create_rtf_multiple_specs <- function(data,
                                      split_list,
                                      specs_list=NULL,
                                      just_list=NULL) {

  # copy $NEWPAGE$ to all columns in a row
  for (i in 1:nrow(data)) {
    if(grepl('\\$NEWPAGE\\$', paste0(data[i,], collapse=''))) {
      for (col in 1:ncol(data)) {
        data[i,col] <- '$NEWPAGE$'
      }
    }
  }

  # split data
  uniq_vals <- unique(unlist(split_list))
  data_subset_idx <- lapply(uniq_vals, function(y) sapply(split_list, function(x) y %in% x))
  data_subsets <- lapply(data_subset_idx, function(x) list(data[,x]))
  for (i in 1:length(data_subsets)) {
    data_subsets[[i]]$specs <- specs_list[[i]]
    data_subsets[[i]]$just  <- just_list[[i]]
  }
  #data_subsets

  all_outputs <- lapply(data_subsets, function(x) {
    create_rtf_table(
      data=x[[1]],
      specs = x$specs,
      col_just = x$just,
      get_pages = T
    )[[2]]
  })

  total_pages <- length(all_outputs)*length(all_outputs[[1]])

  combined_output <- NULL
  for (pg in 1:length(all_outputs[[1]])) {
    for (subset in 1:length(all_outputs)) {
      combined_output <- c(
        combined_output,
        unlist(all_outputs[[subset]][[pg]])
      )
    }
  }
  combined_output

  pg_nums_idx <- grepl('Page (\\d+) of (\\d+)$', combined_output)
  pg_nums_cum <- cumsum(pg_nums_idx)

  pg_replacements <- NULL
  for (i in which(pg_nums_idx)) {
    pg_replacements <- c(pg_replacements,
                         gsub("Page (\\d+) of (\\d+)",
                              paste0("Page ", pg_nums_cum[i], " of ", total_pages),
                              combined_output[i]))
  }
  for (i in 2:length(pg_replacements)) {
    if(!grepl('^\\\\page', pg_replacements[i])) {
      pg_replacements[i] <- paste('\\page', pg_replacements[i])
    }
  }
  combined_output[pg_nums_idx] <- pg_replacements

  header_file <- system.file("extdata", "rtf_header.txt", package="aRtf")
  rtf_header <- readLines(header_file)
  rtf_row <- rtf_header[length(rtf_header)]
  rtf_header <- rtf_header[1:length(rtf_header)-1]

  final_table <- c(
    rtf_header,
    sapply(combined_output, function(x) ifelse(grepl('^\\page', rtf_row), paste0(rtf_row, x),
                                               paste(rtf_row, x))),
    '}')

  return(final_table)
}
