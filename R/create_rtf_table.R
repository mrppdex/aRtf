#' Create RTF Table
#'
#' This function creates a table in RTF format from the given data, specifications,
#' header text, and footer text.
#'
#' @param data A data frame containing the data to be included in the table.
#' @param specs A list of specifications for the table header.
#' @param col_just A character vector specifying the justification of each column.
#' @param header_text_lines A character vector containing the lines of text to be
#' included in the table header.
#' @param footer_text_lines A character vector containing the lines of text to be
#' included in the table footer.
#' @param PAGE_WIDTH The width of the page. Default is 133.
#' @param PAGE_HEIGHT The height of the page. Default is 45.
#'
#' @details
#' The function creates a table in RTF format with the specified header, body, and footer.
#' The `data` parameter is a data frame containing the data to be included in the table body.
#' The `specs` parameter is a list of specifications for the table header.
#' The `col_just` parameter is a character vector specifying the justification of each column.
#' The `header_text_lines` parameter is a character vector containing the lines of text to be included in the table header.
#' The `footer_text_lines` parameter is a character vector containing the lines of text to be included in the table footer.
#' The `PAGE_WIDTH` parameter specifies the width of the page.
#' The `PAGE_HEIGHT` parameter specifies the height of the page.
#'
#' If $NEWPAGE$ is included in any of the table body rows, it will start a new page.
#'
#' @seealso
#' \code{\link{create_header_from_specs}} for the detailed explanation of the `specs` parameter.
#'
#' @return A character vector containing the RTF code for the table.
#'
#' @examples
#' \dontrun{
#' #' data <- data.frame(
#'   col1 = c("The quick brown fox",
#'            "$NEWPAGE$",
#'            "jumps over the lazy dog",
#'            "$NEWPAGE$",
#'            "She sells sea shells down by the sea shore\nnew line"),
#'   col2 = c("Lorem ipsum dolor sit amet",
#'            "",
#'            "consectetur adipiscing elit",
#'            "",
#'            "Sepmre fidelis")
#' )
#'
#' specs <- list(
#'   list(label='Quotes', just='c',
#'        sub=list(
#'          list(label='English', just='c'),
#'          list(label='Latin', just='l')
#'        ))
#' )
#'
#' header_text <- c(
#'   "this is my header this is my header this is my header this is my header",
#'   "Safety Population"
#' )
#'
#' footer_text <- c(
#'   "this is my footer this is my footer this is my footer this is my footer",
#'   "",
#'   "a path"
#' )
#'
#' out_rtf <- create_rtf_table(data = data, header_text_lines = header_text,
#'                             footer_text_lines = footer_text, specs = specs)
#' writeLines(out_rtf, 'tmp_tfl.rtf')
#' }
#'
#' @export
create_rtf_table <- function(
    data,
    specs=NULL,
    col_just=NULL,
    header_text_lines=NULL,
    footer_text_lines=NULL,
    PAGE_WIDTH=133,
    PAGE_HEIGHT=45) {

  data <- data.frame(lapply(data, as.character), stringsAsFactors = FALSE)

  if(is.null(specs)) {
    specs <- lapply(colnames(data), function(x) list(label=x))
  }

  # Generate and print header

  header <- create_header_from_specs(specs, width = PAGE_WIDTH)
  table_columns_header <- strsplit(header$header, '\n')[[1]]

  # Create Top Header
  new_header <- Header$new()
  for(t in header_text_lines) {
    new_header$add_line(t)
  }
  new_header$complete_header()

  # Create Footer
  new_footer <- Footer$new()
  for (t in footer_text_lines) {
    new_footer$add_line(t)
  }

  # Process Data
  text <- create_table_body(data, PAGE_WIDTH, widths = header$column_widths, positions = header$columns_offsets, just = col_just)

  # Augment text
  # when $NEWPAGE$, complete the page to the size:
  # PAGE_HEIGHT - length(header) - my_header$get_depth() - my_footer$get_depth()

  new_pages_numbers <- rev(which(grepl('\\$NEWPAGE\\$', text)))
  new_pages_numbers_2 <- c(new_pages_numbers[1:length(new_pages_numbers)-1]-new_pages_numbers[2:length(new_pages_numbers)],
                           new_pages_numbers[length(new_pages_numbers)])

  BODY_HEIGHT <- PAGE_HEIGHT- new_header$get_depth() - new_footer$get_depth() - length(table_columns_header)

  if(length(new_pages_numbers)>0) {
    for( i in 1:length(new_pages_numbers)) {
      np <- new_pages_numbers[i]
      np_2 <- new_pages_numbers_2[i]
      n_empty_lines <- BODY_HEIGHT - np_2%%BODY_HEIGHT
      empty_lines <- rep(strrep(' ', PAGE_WIDTH), n_empty_lines+1)
      new_text <- c(text[1:np-1], empty_lines, if ((np+1)<=length(text)) text[(np+1):length(text)])
      text <- new_text
    }
  }


  # Calculate number of pages
  TEXT_PG_HEIGHT <- PAGE_HEIGHT - new_header$get_depth() - new_footer$get_depth() - length(table_columns_header)
  N_pages <- ceiling(length(text)/TEXT_PG_HEIGHT)

  total_length <- length(text)
  pg_starts <- sapply(1:ceiling(total_length/BODY_HEIGHT), function(x) (x-1)*BODY_HEIGHT+1)
  pg_ends   <- if(total_length<=TEXT_PG_HEIGHT) total_length
               else c(pg_starts[2:length(pg_starts)]-1, total_length)
  pg_start_stop_df <- as.data.frame(list(start=pg_starts, stop=pg_ends))

  header_file <- system.file("extdata", "rtf_header.txt", package="aRtf")
  rtf_header <- readLines(header_file)
  rtf_row <- rtf_header[length(rtf_header)]
  rtf_header <- rtf_header[1:length(rtf_header)-1]

  final_table_txt <- c()

  for (pg in 1:nrow(pg_start_stop_df)) {
    start <- pg_start_stop_df[pg, 'start']
    stop  <- pg_start_stop_df[pg, 'stop']

    pg_txt <- trimws(paste('Page', pg, 'of', N_pages))
    top_header <- new_header$get_lines()
    top_header[1] <- substr_replace(top_header[1], pg_txt, nchar(top_header[1])-nchar(pg_txt), nchar(top_header[1]))

    title <- top_header
    if (pg>1) {
      title[1] <- paste0('\\page ', title[1])
    }

    final_table_txt <- c(final_table_txt,
                         title,
                         table_columns_header,
                         text[start:stop],
                         new_footer$get_lines())
  }

  final_table <- c(
    rtf_header,
    sapply(final_table_txt, function(x) ifelse(grepl('^\\page', rtf_row), paste0(rtf_row, x),
                                               paste(rtf_row, x))),
    '}')

  return(final_table)
}
