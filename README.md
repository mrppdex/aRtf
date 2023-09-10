# aRtf
The `aRtf` package provides functions to create formatted tables in Rich Text Format (RTF) 
from R data frames. The main function, `create_rtf_table`, takes a data frame, header and footer specifications, 
and other formatting options, and returns a character vector containing the RTF code for the table. 
This RTF code can then be written to a file and opened in any word processing software that supports RTF. 
The package also provides utility functions for specifying header and footer, and for handling special 
formatting requirements like new pages.

# Installation
`devtools::install_github("mrppdex/aRtf", build_vignettes = TRUE)`

# Example
```r
library(aRtf)

# Generate mock data
N <- 400
idx <- paste0('Parameter ',sapply(1:300, function(x) paste0(sample(LETTERS, 5, replace=T), collapse = '')))
n_treat <- sample(100:200, length(idx), replace = T)
pct_treat <- 100*n_treat/N
n_control <- sample(100:200, length(idx), replace = T)
pct_control <- 100*n_control/N
df <- as.data.frame(list(idx=idx, n_treat=n_treat, pct_treat=pct_treat, n_control=n_control, pct_control=pct_control))

# Specify the header
header <- c('Fake dataset',
            'ITT Population')

# Specify th efooter
footer <- c('Program path: c:\\')

# Specify the structure of the columns' header
specs <- list(
  list(label='', sub=list(
    list(label='', sub=list(
      list(label='Parameter Name', width=40, just='l')
    ))
  )),
  list(label='LY', just='c', sub=list(
    list(label = '----------------------------', sub=list(
      list(label='n', just='c'),
      list(label='(%)', just='l')
    ))
  )),
  list(label='Placebo', just='c', sub=list(
    list(label = '----------------------------', sub=list(
      list(label='n', just='c'),
      list(label='(%)', just='l')
    ))
  ))
)

# Generate text of the output RTF
test_rtf <- aRtf::create_rtf_table(data=df, header_text_lines = header, footer_text_lines = footer,
                             specs = specs, col_just=c('l','c','l','c','l'))

# Save to file
writeLines(test_rtf, 'out_fl.rtf')
```

# Author
pawel.piela at lilly.com
