#' Footer ggproto object
#'
#' A ggproto object for creating and managing table footers.
#'
#' @field width The total width of the footer. Default is 133.
#' @field lines A character vector containing the lines of text in the footer.
#'
#' @description
#' The Footer object has several methods:
#'
#' \describe{
#'   \item{new}{Creates a new Footer object.}
#'   \item{add_line}{Adds a line of text to the footer. Takes a single argument,
#'   \code{txt}, a character string containing the text to be added.}
#'   \item{get_lines}{Returns the lines of text in the footer.}
#'   \item{add_empty_line}{Adds an empty line to the footer.}
#'   \item{get_depth}{Returns the number of lines in the footer.}
#' }
#'
#' @examples
#' new_footer <- Footer$new()
#' new_footer$add_line("this is my footer this is my footer this is my footer")
#' new_footer$add_empty_line()
#' new_footer$add_line("a path")
#' new_footer$get_depth()
#' for (l in new_footer$get_lines()) {
#'   cat(l, '\n')
#' }
#'
#' @import ggplot2
#' @export
Footer <- ggplot2::ggproto("Footer",
                           width = 133,
                           lines = c(),
                           new = function(self, width=133) {
                             self$width <- width
                             self$lines <-c(strrep('-', self$width))
                             return(self)
                           },
                           add_line = function(self, txt) {
                             txt_split <- sapply(split_text(txt, self$width),
                                                 function(s) sprintf("%*s", -self$width, s))
                             for (l in txt_split) {
                               self$lines <- c(self$lines, l)
                             }
                           },
                           get_lines = function(self) {
                             return(self$lines)
                           },
                           add_empty_line = function(self) {
                             self$lines <- c(self$lines, strrep(' ', self$width));
                           },
                           get_depth = function(self) {
                             return(length(self$lines))
                           }
)
