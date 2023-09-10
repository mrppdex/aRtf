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
    return(paste0(sprintf("%*s", width - 1, text), " "))
  } else {  # Left alignment
    return(paste0(" ", sprintf("%-*s", width - 1, text)))
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
  # Split the text into words
  words <- strsplit(text, " ")[[1]]

  # Initialize the list of lines and the current line
  lines <- list()
  current_line <- ""

  # Loop over each word
  for (word in words) {
    # If the current word is longer than the specified width, it will be split
    while (nchar(word) > width) {
      # Extract the part of the word that fits on the current line
      part <- substring(word, 1, width)
      # Update the word to contain only the remaining characters
      word <- substring(word, width + 1)
      # Add the part of the word to the current line
      current_line <- paste0(current_line, part)
      # Add the current line to the list of lines and start a new line
      lines <- c(lines, current_line)
      current_line <- ""
    }

    # If the current word fits on the current line, add it to the current line
    if (nchar(current_line) + nchar(word) <= width) {
      current_line <- paste0(current_line, word, " ")
    } else {
      # Otherwise, add the current line to the list of lines and start a new line
      lines <- c(lines, current_line)
      current_line <- paste0(word, " ")
    }
  }

  # Add the remaining part of the current line to the list of lines
  lines <- c(lines, current_line)

  return(lines)
}
