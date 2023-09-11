#' Header ggproto object
#'
#' A ggproto object for creating and managing table headers.
#'
#' @field width The total width of the header. Default is 133.
#' @field lines A character vector containing the lines of text in the header.
#' @field current_datetime A character string containing the current date and time.
#' It is updated each time a new Header object is created.
#' @field input_status A character string indicating the input status. Default is 'qa'.
#' @field output_status A character string indicating the output status. Default is 'qa'.
#'
#' @description
#' The Header object has several methods:
#'
#' \describe{
#'   \item{add_line}{Adds a line of text to the header. Takes a single argument,
#'   \code{txt}, a character string containing the text to be added.}
#'   \item{get_lines}{Returns the lines of text in the header.}
#'   \item{new}{Creates a new Header object.}
#'   \item{add_empty_line}{Adds an empty line to the header.}
#'   \item{set_data_status}{Sets the input and output status of the header. Takes two arguments,
#'   \code{instat}, a character string indicating the input status, and \code{outstat}, a character
#'   string indicating the output status.}
#'   \item{complete_header}{Completes the header by adding the current date and time,
#'   and the input and output status.}
#'   \item{get_depth}{Returns the number of lines in the header.}
#' }
#'
#' @examples
#' new_header <- Header$new()
#' new_header$add_line("this is my header this is my header this is my header this is my header")
#' new_header$add_line("Safety Population")
#' new_header$complete_header()
#' new_header$get_depth()
#' for (l in new_header$get_lines()) {
#'   cat(l, '\n')
#' }
#'
#' @import ggplot2
#' @export
Header <- ggplot2::ggproto("Header",
                  width = 133,
                  lines = c(),
                  current_datetime = toupper(format(Sys.time(), "%H:%M %d%b%Y")),
                  input_status = 'qa',
                  output_status = 'qa',
                  add_line = function(self, txt) {
                    txt_split <- sapply(split_text(txt, self$width - nchar(self$current_datetime) - 2),
                                        function(s) sprintf("%*s", -self$width, s))
                    # if (length(self$lines)==1) {
                    #   txt_split[1] <- substr_replace(txt_split[1], self$current_datetime, self$width-nchar(self$current_datetime), self$width)
                    # }
                    for (l in txt_split) {
                      self$lines <- c(self$lines, l)
                    }
                  },
                  get_lines = function(self) {
                    return(self$lines)
                  },
                  new = function(self) {
                    self$current_datetime <- toupper(format(Sys.time(), "%H:%M %d%b%Y"))
                    self$lines <-c()
                    return(self)
                  },
                  add_empty_line = function(self) {
                    self$lines <- c(self$lines, strrep(' ', self$width));
                  },
                  set_data_status = function(self, instat, outstat) {
                    self$input_status <- instat
                    self$output_status <- outstat
                  },
                  complete_header = function(self) {
                    date_line <- NULL
                    if (length(self$lines)>1) {
                      self$lines[2] <- substr_replace(self$lines[2], self$current_datetime, self$width-nchar(self$current_datetime)+1, self$width)
                    } else {
                      date_line <- sprintf("%*s", self$width, self$current_datetime)
                    }

                    status_string <- paste0(ifelse(self$input_status=='qa', 'TD', 'PD'),
                                            ifelse(self$output_status=='qa', 'TM', 'PM'))
                    last_line <- NULL
                    if (length(self$lines)>2) {
                      self$lines[3] <- substr_replace(self$lines[3], status_string, self$width-4+1, self$width)
                    } else {
                      last_line <- sprintf("%*s", self$width, status_string)
                    }

                    if(length(self$lines)==0) {
                      self$lines <- strrep(' ', self$width)
                    }
                    self$lines <- c(self$lines, date_line, last_line)
                  },
                  get_depth = function(self) {
                    return(length(self$lines))
                  }
)
