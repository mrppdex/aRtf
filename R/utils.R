#' Replace a Substring Within a String
#'
#' This function replaces a substring within a string.
#'
#' @param main_str The main string.
#' @param replace_str The string to replace the substring with.
#' @param start The starting index of the substring to be replaced.
#' @param stop The ending index of the substring to be replaced.
#' @return The modified string.
#' @examples
#' \dontrun{substr_replace("hello world", "new", 7, 11)}
#' # [1] "hello new"
substr_replace <- function(main_str, replace_str, start, stop) {
  prefix <- substr(main_str, 1, start - 1)
  suffix <- substr(main_str, stop + 1, nchar(main_str))
  return(paste0(prefix, replace_str, suffix))
}

#' Justify Text
#'
#' This function justifies a text to the left, center, or right.
#'
#' @param text The text to be justified.
#' @param width The width of the field.
#' @param justification The justification type: 'c' for center, 'r' for right, and 'l' for left.
#' @return The justified text.
#' @examples
#' \dontrun{justify_text("hello", 10, 'c')}
#' # [1] "   hello   "
justify_text <- function(text, width, justification) {
  if (justification == 'c') {
    return(sprintf("%*s", -width, sprintf("%*s", (nchar(text) + width) %/% 2, text)))
  } else if (justification == 'r') {
    return(sprintf("%*s", width, text))
  } else {  # Left alignment
    return(sprintf("%-*s", width, text))
  }
}

#' Split Text Into Lines
#'
#' This function splits a text into lines of a specified width.
#'
#' @param text The text to be split.
#' @param width The maximum width of a line.
#' @return A list of lines.
#' @examples
#' \dontrun{split_text('lorem bla baglk asdf asdfasfd  fdsafasdf', 15)}
#' # [[1]]
#' # [1] "lorem bla baglk "
#' #
#' # [[2]]
#' # [1] "asdf asdfasfd  "
#' #
#' # [[3]]
#' # [1] "fdsafasdf "
split_text <- function(text, width) {
  # Split the text into lines at each newline character
  text <- ifelse(is.na(text) | text=='', ' ', text)
  lines <- strsplit(text, "\n")[[1]]

  # Function to wrap a single line to the specified width
  wrap_line <- function(line, width) {
    words <- strsplit(line, " ")[[1]]
    lines <- list()
    current_line <- ""
    for (word in words) {
      while (nchar(word) > width) {
        part <- substring(word, 1, width)
        word <- substring(word, width + 1)
        current_line <- paste0(current_line, part)
        lines <- c(lines, current_line)
        current_line <- ""
      }
      if (nchar(current_line) + nchar(word) <= width) {
        current_line <- paste0(current_line, word, " ")
      } else {
        lines <- c(lines, trimws(current_line, which = 'right'))
        current_line <- paste0(word, " ")
      }
    }
    lines <- c(lines, trimws(current_line, which = 'right'))
    return(lines)
  }

  # Wrap each line to the specified width
  wrapped_lines <- unlist(lapply(lines, wrap_line, width))

  return(wrapped_lines)
}
