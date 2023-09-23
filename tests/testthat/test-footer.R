test_that("Footer object can be created", {
  footer <- Footer$new()
  expect_s3_class(footer, "Footer")
})

test_that("Footer object initializes with a line", {
  footer <- Footer$new()
  expect_length(footer$get_lines(), 1)
  expect_equal(footer$get_lines()[1], strrep('-', footer$width))
})

test_that("Footer object add_line method adds lines", {
  footer <- Footer$new()
  footer$add_line("this is my footer")
  expect_length(footer$get_lines(), 2)
  expect_equal(footer$get_lines()[2], sprintf("%*s", -footer$width, "this is my footer"))
})

test_that("Footer object add_empty_line method adds empty line", {
  footer <- Footer$new(40)
  footer$add_empty_line()
  expect_length(footer$get_lines(), 2)
  expect_equal(footer$get_lines()[2], strrep(' ', footer$width))
})

test_that("Footer object get_depth method returns correct depth", {
  footer <- Footer$new()
  footer$add_line("this is my footer")
  footer$add_empty_line()
  expect_equal(footer$get_depth(), 3)  # Including the initial line
})
