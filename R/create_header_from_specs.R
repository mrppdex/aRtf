#' Create Header from Specs
#'
#' This function generates a table header. The header has a specified width,
#' and the labels, widths, and the justification of label text are specified in 'specs'.
#' It returns a vector of text lines (rows) of the header, widths of text in each of the columns,
#' and the relative position of the text of the columns (relative to the start of the line).
#' 'specs' can be null; otherwise, it is a list of lists that allows creating a hierarchical
#' header structure, allowing some labels to span one or more columns.
#'
#' @param specs A list of lists that specifies the labels, justification, and subheaders
#' for the table header. If NULL, the header is crated based on the data column names. Otherwise,
#' it should be a list of lists,
#' where each list can contain the following elements:
#' \itemize{
#'   \item \code{label}: A string that specifies the label of the header or subheader.
#'   \item \code{just}: (Optional) A string that specifies the justification of the text. It can be 'l' for left-justified,
#'   'c' for center-justified, or 'r' for right-justified.
#'   \item \code{pct}: (Optional) A numeric value that specifies the width of the column as the percentage
#'   of the parent column. If not provided, the width is determined based on the content of the column.
#'   \item \code{sub}: (Optional) A list of subheaders. It is another list of lists with the same structure as
#'   'specs', which specifies the subheaders of the current header.
#' }
#'
#' Here is an example of 'specs':
#' \preformatted{
#' specs <- list(
#'   list(
#'     label = 'Main Header 1',
#'     just = 'c',
#'     sub = list(
#'       list(
#'         label = 'Subheader 1.1',
#'         just = 'c',
#'         sub = list(
#'           list(
#'             label = 'Subheader 1.1.1',
#'             just = 'c'
#'           )
#'         )
#'       ),
#'       list(
#'         label = 'Subheader 1.2',
#'         just = 'c'
#'       )
#'     )
#'   ),
#'   list(
#'     label = 'Main Header 2',
#'     just = 'c'
#'   )
#' )
#' }
#'
#' In this example, 'Main Header 1' has two subheaders: 'Subheader 1.1' and 'Subheader 1.2'.
#' 'Subheader 1.1' has one sub-subheader: 'Subheader 1.1.1'. 'Main Header 2' does not have any subheaders.
#' @param width The total width of the table.
#'
#' @return A list containing three elements:
#' \itemize{
#' \item 'header': All text lines (rows) of the header joined with '\\n'.
#' \item 'widths': The widths of text in each of the columns.
#' \item 'positions': The relative position of the text of the columns (relative to the start of the line).
#' }
#'
#' @examples
#' specs <- list(
#'   list(label='Irregular A', just='c', sub=list(
#'     list(label='Sub A1', just='c'),
#'     list(label='Sub A2', just='c', sub=list(
#'       list(label='Deep A2.1', just='c', sub=list(
#'         list(label='Deeper A2.1.1', just='c')
#'       ))
#'     ))
#'   )),
#'   list(label='Irregular B', just='c')
#' )
#' create_header_from_specs(specs, width = 80)
#'
#' @export
create_header_from_specs <- function(specs, width) {

  calculate_depth <- function(specs) {
    calculate_max_depths_per_level <- function(specs, current_level=1, max_depths=list()) {
      for (spec in specs) {
        # Calculate depth based on the label
        current_depth <- length(unlist(strsplit(spec$label, "\n")))

        # Update the max_depths list according to the instructions
        if (length(max_depths) < current_level) {
          max_depths[[current_level]] <- current_depth
        } else if (current_depth > max_depths[[current_level]]) {
          max_depths[[current_level]] <- current_depth
        }

        # If there's a sub list, recursively update the max_depths list
        if (!is.null(spec$sub)) {
          max_depths <- calculate_max_depths_per_level(spec$sub, current_level + 1, max_depths)
        }
      }

      return(max_depths)
    }

    depths_per_level <- calculate_max_depths_per_level(specs)
    return(sum(unlist(depths_per_level)))
  }

  calculate_max_depths_per_level <- function(specs, current_level=1, max_depths=list()) {
    for (spec in specs) {
      # Calculate depth based on the label
      current_depth <- length(unlist(strsplit(spec$label, "\n")))

      # Update the max_depths list according to the instructions
      if (length(max_depths) < current_level) {
        max_depths[[current_level]] <- current_depth
      } else if (current_depth > max_depths[[current_level]]) {
        max_depths[[current_level]] <- current_depth
      }

      # If there's a sub list, recursively update the max_depths list
      if (!is.null(spec$sub)) {
        max_depths <- calculate_max_depths_per_level(spec$sub, current_level + 1, max_depths)
      }
    }

    return(max_depths)
  }

  add_depth_to_specs <- function(specs, max_depths, current_level=1) {
    num_siblings <- length(specs) # number of items at the current level
    total_pct <- sum(sapply(specs, function(x) ifelse(is.null(x$pct), 0, x$pct)), na.rm=TRUE)
    num_siblings_with_pct <- sum(!sapply(specs, function(x) is.null(x$pct)))

    for (i in seq_along(specs)) {
      # Assign depth from max_depths
      specs[[i]]$depth <- max_depths[[current_level]]

      # Assign the number of siblings at the current level
      specs[[i]]$siblings <- num_siblings

      # Calculate siblingspct
      if (is.null(specs[[i]]$pct)) {
        specs[[i]]$siblingspct <- total_pct
      } else {
        specs[[i]]$siblingspct <- total_pct - specs[[i]]$pct
        if(specs[[i]]$siblingspct == 0) specs[[i]]$siblingspct <- NULL
      }

      # Assign the number of siblings with specific pct values
      specs[[i]]$siblingsspecpct <- ifelse(is.null(specs[[i]]$pct), num_siblings_with_pct, num_siblings_with_pct - 1)

      # If it's the first level, add the 'top' flag
      specs[[i]]$top <- ifelse(current_level == 1, TRUE, FALSE)

      # If there's a sub list, recursively add depth to it
      if (!is.null(specs[[i]]$sub)) {
        specs[[i]]$sub <- add_depth_to_specs(specs[[i]]$sub, max_depths, current_level + 1)
      }

      # If it's the last element in its level, add the 'last' flag
      specs[[i]]$last <- ifelse(i == length(specs), TRUE, FALSE)
    }

    return(specs)
  }

  # Calculate maximum depths per level
  depths_per_level <- calculate_max_depths_per_level(specs)

  # Add depth to the specs
  augmented_specs <- add_depth_to_specs(specs, depths_per_level)

  #max_depth <- specs[[1]]$depth  # Get the max depth from the first spec
  max_depth <- calculate_depth(specs)

  # Initialize lines list
  lines <- vector("list", max_depth)
  for (i in 1:max_depth) {
    lines[[i]] <- strrep(" ", width)
  }

  # Initialize a list to hold the column widths at the lowest level
  column_widths <- list()
  column_offsets <- list()

  print_hierarchy_augmented <- function(specs, parent_width, offset=0, current_depth=1, prev_depth=1) {

    current_offset <- offset
    level_depth <- current_depth

    total_sublevel_pcts <- c()
    total_widths <- 0
    for (spec in specs) {
      label_lines <- unlist(strsplit(spec$label, "\n"))
      label_pct <- NA
      if (!is.null(spec$pct)) {
        label_pct <- spec$pct
      } else if (!is.null(spec$siblingspct)) {
        label_pct <- ((100-sum(total_sublevel_pcts)) - spec$siblingspct)/(spec$siblings-spec$siblingsspecpct)
      } else {
        label_pct <- (100-sum(total_sublevel_pcts))/(spec$siblings - length(total_sublevel_pcts))
      }

      label_width <- ifelse(is.list(spec) && !is.null(spec$pct), round(parent_width * (spec$pct / 100)), round(label_pct*parent_width/100))
      if (spec$last) label_width <- parent_width - total_widths
      total_widths <- total_widths + label_width

      just <- ifelse(is.list(spec) && !is.null(spec$just), spec$just, 'c')

      for (i in 1:spec$depth) {
        if (length(label_lines)>=i) {
          if (just %in% c('c','r') & !spec$last) {
            lines[[current_depth]] <<- substr_replace(lines[[current_depth]], justify_text(label_lines[i], label_width, just), start=current_offset, stop=current_offset+label_width-1)
          } else {
            lines[[current_depth]] <<- substr_replace(lines[[current_depth]], justify_text(label_lines[i], label_width, just), start=current_offset+1, stop=current_offset+label_width)
          }
          if (nchar(label_lines[i])>label_width) {
            warning(paste0('Column label "', label_lines[i],'" is truncated. Increase the column width.'))
          }
          # lines[[current_depth]] <<- substr_replace(lines[[current_depth]], '>', current_offset+1, current_offset+1)
          # lines[[current_depth]] <<- substr_replace(lines[[current_depth]], '<', current_offset+label_width, current_offset+label_width)
        }
        current_depth <- current_depth + 1
      }

      # If there are no sub-specs, add the width of the current column to the column_widths list
      if (is.null(spec$sub)) {
        column_widths <<- c(column_widths, label_width)
        column_offsets <<- c(column_offsets, current_offset)
      }

      # If there are sub-specs, recursively handle them
      if (!is.null(spec$sub)) {
        current_depth <- print_hierarchy_augmented(spec$sub, label_width, current_offset, current_depth, current_depth - spec$depth )
        if (spec$last | spec$top) {
          current_depth <- prev_depth
        }
      } else {
        current_depth <- level_depth
      }

      current_offset <- current_offset + label_width
    }

    if (spec$last | spec$top) {
      return(prev_depth)
    }

    return(level_depth)
  }

  print_hierarchy_augmented(augmented_specs, width)
  return(list(header = paste(c(strrep("-", width), unlist(lines), strrep("-", width)), collapse="\n"),
              column_widths = unlist(column_widths),
              columns_offsets = unlist(column_offsets)))
}
