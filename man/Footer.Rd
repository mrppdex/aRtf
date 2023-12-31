% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/footer.R
\docType{data}
\name{Footer}
\alias{Footer}
\title{Footer ggproto object}
\format{
An object of class \code{Footer} (inherits from \code{ggproto}, \code{gg}) of length 7.
}
\usage{
Footer
}
\description{
The Footer object has several methods:

\describe{
  \item{new}{Creates a new Footer object.}
  \item{add_line}{Adds a line of text to the footer. Takes a single argument,
  \code{txt}, a character string containing the text to be added.}
  \item{get_lines}{Returns the lines of text in the footer.}
  \item{add_empty_line}{Adds an empty line to the footer.}
  \item{get_depth}{Returns the number of lines in the footer.}
}
}
\details{
A ggproto object for creating and managing table footers.
}
\section{Fields}{

\describe{
\item{\code{width}}{The total width of the footer. Default is 133.}

\item{\code{lines}}{A character vector containing the lines of text in the footer.}
}}

\examples{
new_footer <- Footer$new()
new_footer$add_line("this is my footer this is my footer this is my footer")
new_footer$add_empty_line()
new_footer$add_line("a path")
new_footer$get_depth()
for (l in new_footer$get_lines()) {
  cat(l, '\n')
}

}
\keyword{datasets}
