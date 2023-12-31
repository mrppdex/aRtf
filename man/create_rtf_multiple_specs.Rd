% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_rtf_multiple_specs.R
\name{create_rtf_multiple_specs}
\alias{create_rtf_multiple_specs}
\title{Create RTF Documents Based on Multiple Specifications}
\usage{
create_rtf_multiple_specs(
  data,
  split_list,
  specs_list = NULL,
  just_list = NULL,
  header_text_lines = NULL,
  footer_text_lines = NULL,
  input_status = "qa",
  output_status = "qa",
  PAGE_WIDTH = 133,
  PAGE_HEIGHT = 45
)
}
\arguments{
\item{data}{A data frame, the dataset to be used in the RTF document.}

\item{split_list}{List, defining the columns for each split of data.}

\item{specs_list}{List of lists, providing specifications for each subset of data. Each element of the list corresponds to a subset defined in \code{split_list}.}

\item{just_list}{List of vectors, defining the justification for each column in each subset of data.}

\item{header_text_lines}{A character vector containing the lines of text to be
included in the table header.}

\item{footer_text_lines}{A character vector containing the lines of text to be
included in the table footer.}

\item{input_status}{A string that represents the source environment of the input data.
Can be either 'qa', or 'prd'. Default is 'qa'.}

\item{output_status}{A string that represents in which environment will the output be saved.
Can be either 'qa', or 'prd'. Default is 'qa'.}

\item{PAGE_WIDTH}{The width of the page. Default is 133.}

\item{PAGE_HEIGHT}{The height of the page. Default is 45.}
}
\value{
A character vector, where each element represents a line in the final RTF document.
}
\description{
This function generates RTF documents with tables based on multiple specifications provided.
It splits the data, applies the specifications, and then combines the output.
}
\examples{
\dontrun{
# Simple Example Data
data_simple <- data.frame(
  label = c('', 'A', 'B', '', ''),
  col1 = c('', '10 (20.0)', '20 (40.0)', '$NEWPAGE$', '$NEWPAGE$'),
  col2 = c('', '5 (10.0)', '15 (30.0)', '', '')
)

# Specifying Splits and Specifications
split_list_simple <- list(c(1, 2), 1, 2)
specs_list_simple <- list(
  list(list(label='Category', just='l', pct=50), list(label='Value (\%)', just='c', pct=50)),
  list(list(label='Category', just='l', pct=50), list(label='Value', just='c', pct=50))
)
just_list_simple <- list(c('l', 'c'), c('l', 'c'))

# Creating RTF Document
out_rtf_txt_simple <- create_rtf_multiple_specs(
  data = data_simple,
  split_list = split_list_simple,
  specs_list = specs_list_simple,
  just_list = just_list_simple
)
}
}
\seealso{
\code{\link{create_rtf_table}} for the detailed explanation of the parameters.
}
