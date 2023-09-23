test_that("Header object can be created and get_depth returns correct depth", {
  header <- Header$new()
  expect_s3_class(header, "Header")
  expect_equal(header$get_depth(), 0)
})

test_that("add_line adds line correctly", {
  header <- Header$new(40)
  header$add_line("Testing Line")
  expect_equal(header$get_lines()[1], "Testing Line                            ")
  expect_equal(header$get_depth(), 1)
})

test_that("add_empty_line adds an empty line correctly", {
  header <- Header$new()
  header$add_empty_line()
  expect_equal(header$get_lines()[1], strrep(' ', header$width))
  expect_equal(header$get_depth(), 1)
})

test_that("set_data_status sets statuses and complete_header completes the header correctly", {
  header <- Header$new()
  header$set_data_status('pd', 'pm')
  header$complete_header()
  expect_equal(substr(header$get_lines()[3], header$width-3, header$width), "PDPM")
})

test_that("complete_header adds correct datetime in the second row of the subject header", {
  header <- Header$new(50)
  header$complete_header()
  header_time_string <- trimws(header$get_lines()[2])
  header_time <- as.POSIXct(header_time_string,
                            format='%H:%M %d%b%Y')
  t_diff <- abs(as.numeric(difftime(header_time, Sys.time(),
                                    units='min')))
  expect_true(t_diff<1)
})

test_that("get_lines returns correct lines with multiple lines added", {
  header <- Header$new(40)
  header$add_line("Line 1")
  header$add_line("Line 2")
  header$add_empty_line()
  expect_equal(header$get_lines(), c(sprintf("%*s", -header$width, "Line 1"),
                                     sprintf("%*s", -header$width, "Line 2"),
                                     strrep(' ', header$width)))
})
