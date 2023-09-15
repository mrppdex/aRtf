#' Create Table Body
#'
#' This function creates the body of a table from a given data frame. The table body is created
#' based on the provided column widths, positions, and justification. If these parameters are not
#' provided, they are calculated based on the table width and the number of columns.
#'
#' @param data A data frame containing the data to be included in the table body.
#' @param table_width An integer specifying the total width of the table.
#' @param widths (Optional) A vector of integers specifying the width of each column.
#' If NULL or if its length is less than the number of columns in the data, it will be calculated.
#' @param positions (Optional) A vector of integers specifying the starting position of each column.
#' If NULL or if its length is less than the number of columns in the data, it will be calculated.
#' @param just (Optional) A vector of characters specifying the text justification for each column.
#' It can be 'l' for left-justified, 'c' for center-justified, or 'r' for right-justified.
#' If NULL or if its length is less than the number of columns in the data,
#' all columns will be left-justified.
#'
#' @return A vector of character strings, each representing a line (row) of the table body.
#'
#' @examples
#' data <- data.frame(
#'   col1 = c("The quick brown fox", "", "jumps over the lazy dog"),
#'   col2 = c("Lorem ipsum dolor sit amet", "", "consectetur adipiscing elit")
#' )
#'
#' table_width <- 40
#' widths <- c(10, 20)
#' positions <- c(1, 13)
#' just <- c('c', 'l')
#'
#' create_table_body(data, table_width, widths, positions, just)
#'
#' @seealso \code{\link{create_header_from_specs}}
#'
#' @export
create_table_body <- function(data, table_width, widths=NULL, positions=NULL, just=NULL) {

  # Process table parameters
  if (is.null(widths) | length(widths)<ncol(data)) {
    if (is.null(positions) | length(positions)<ncol(data)) {
      widths <- as.integer(rep(table_width, ncol(data))/ncol(data))
    } else {
      positions_full <- c(positions, table_width)
      widths <- positions_full[2:length(positions_full)] - positions_full[1:(length(positions_full)-1)]
    }
  }

  if(is.null(positions) | length(positions)<ncol(data)) {
    positions <- cumsum(widths)-widths[1]
  }
  if(is.null(just) | length(just)<ncol(data)) {
    just <- rep('l', ncol(data))
  }

  # Split col text
  lines <- vector("list", nrow(data))
  for (r in 1:nrow(data)) {
    lines[[r]] <- vector("list", ncol(data))
    for (c in 1:ncol(data)) {
      lines[[r]][[c]] <- split_text(data[r, c], widths[c])
    }
  }

  # Add depth info
  for (r in 1:length(lines)) {
    for (c in 1:length(lines[[1]])) {
      if (is.null(lines[[r]]$depth)) {
        lines[[r]]$depth <- length(lines[[r]][[c]])
      } else {
        lines[[r]]$depth <- max(lines[[r]]$depth,
                                length(lines[[r]][[c]]))
      }
    }
  }

  # Vector of gaps between columns
  vec_gaps <- c(0, positions[2:length(positions)] - (widths + positions)[1:length(positions)-1])

  # Create lines of text for the output
  txt_lines <- c()
  for (r in 1:length(lines)) {
    for (d in 1:(lines[[r]]$depth)) {
      format_str <- ""
      args <- list()
      for (c in 1:(length(lines[[r]])-1)) {
        space_width <- widths[c]
        #format_gap <- ifelse(c<(length(lines[[r]])-1), widths[c]-positions[c+1], 0)
        format_gap <- ifelse(c<(length(lines[[r]])-1), vec_gaps[c], 0)
        format_str <- paste0(format_str, "%s%", format_gap, "s")

        col_txt <- ifelse(length(lines[[r]][[c]]) >= d, trimws(lines[[r]][[c]][[d]], which = 'right'), ' ')
        col_txt <- justify_text(ifelse(just[c]=='c', trimws(col_txt), col_txt), space_width, just[c])
        args <- c(args, col_txt, ifelse(format_gap==0,'',' '))
      }
      txt_lines <- c(txt_lines, do.call(sprintf, c(list(format_str), args)))
    }
  }

  return(txt_lines)
}

