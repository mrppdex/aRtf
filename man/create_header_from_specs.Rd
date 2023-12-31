% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_header_from_specs.R
\name{create_header_from_specs}
\alias{create_header_from_specs}
\title{Create Header from Specs}
\usage{
create_header_from_specs(specs, width)
}
\arguments{
\item{specs}{A list of lists that specifies the labels, justification, and subheaders
for the table header. If NULL, the header is crated based on the data column names. Otherwise,
it should be a list of lists,
where each list can contain the following elements:
\itemize{
  \item \code{label}: A string that specifies the label of the header or subheader.
  \item \code{just}: (Optional) A string that specifies the justification of the text. It can be 'l' for left-justified,
  'c' for center-justified, or 'r' for right-justified.
  \item \code{pct}: (Optional) A numeric value that specifies the width of the column as the percentage
  of the parent column. If not provided, the width is determined based on the content of the column.
  \item \code{sub}: (Optional) A list of subheaders. It is another list of lists with the same structure as
  'specs', which specifies the subheaders of the current header.
}

Here is an example of 'specs':
\preformatted{
specs <- list(
  list(
    label = 'Main Header 1',
    just = 'c',
    sub = list(
      list(
        label = 'Subheader 1.1',
        just = 'c',
        sub = list(
          list(
            label = 'Subheader 1.1.1',
            just = 'c'
          )
        )
      ),
      list(
        label = 'Subheader 1.2',
        just = 'c'
      )
    )
  ),
  list(
    label = 'Main Header 2',
    just = 'c'
  )
)
}

In this example, 'Main Header 1' has two subheaders: 'Subheader 1.1' and 'Subheader 1.2'.
'Subheader 1.1' has one sub-subheader: 'Subheader 1.1.1'. 'Main Header 2' does not have any subheaders.}

\item{width}{The total width of the table.}
}
\value{
A list containing three elements:
\itemize{
\item 'header': All text lines (rows) of the header joined with '\\n'.
\item 'widths': The widths of text in each of the columns.
\item 'positions': The relative position of the text of the columns (relative to the start of the line).
}
}
\description{
This function generates a table header. The header has a specified width,
and the labels, widths, and the justification of label text are specified in 'specs'.
It returns a vector of text lines (rows) of the header, widths of text in each of the columns,
and the relative position of the text of the columns (relative to the start of the line).
'specs' can be null; otherwise, it is a list of lists that allows creating a hierarchical
header structure, allowing some labels to span one or more columns.
}
\examples{
specs <- list(
  list(label='Irregular A', just='c', sub=list(
    list(label='Sub A1', just='c'),
    list(label='Sub A2', just='c', sub=list(
      list(label='Deep A2.1', just='c', sub=list(
        list(label='Deeper A2.1.1', just='c')
      ))
    ))
  )),
  list(label='Irregular B', just='c')
)
create_header_from_specs(specs, width = 80)

}
