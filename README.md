# aRtf
The `aRtf` package provides functions to create formatted tables in Rich Text Format (RTF) 
from R data frames. The main function, `create_rtf_table`, takes a data frame, header and footer specifications, 
and other formatting options, and returns a character vector containing the RTF code for the table. 
This RTF code can then be written to a file and opened in any word processing software that supports RTF. 
The package also provides utility functions for specifying header and footer, and for handling special 
formatting requirements like new pages.

# Installation
`devtool::install_github("mrppdex/aRtf")`
