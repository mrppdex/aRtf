test_that("create_table_body works with provided widths, positions, and justifications", {
  data <- data.frame(col1=c("apple", "banana"), col2=c("car", "dog"))
  table_width <- 20
  widths <- c(9, 9)
  positions <- c(1, 11)
  just <- c('c', 'r')
  result <- create_table_body(data, table_width, widths, positions, just)
  expected <- c("   apple         car", "  banana         dog")
  expect_equal(result, expected)
})

test_that("create_table_body works with default widths, positions, and left justifications", {
  data <- data.frame(col1=c("apple", "banana"), col2=c("car", "dog"))
  result <- create_table_body(data, 20)
  expected <- c("apple     car       ",
                "banana    dog       ")
  expect_equal(result, expected)
})

test_that("create_table_body handles different numbers of rows and columns", {
  data <- data.frame(col1=c("apple"), col2=c("banana"), col3=c("car"))
  result <- create_table_body(data, 30)
  expected <- c("apple     banana    car       ")
  expect_equal(result, expected)
})

test_that("create_table_body handles different column widths", {
  data <- data.frame(col1=c("apple", "banana"), col2=c("car", "dog"))
  table_width <- 22
  widths <- c(5, 15)
  positions <- c(1, 7)
  result <- create_table_body(data, table_width, widths, positions)
  expected <- c(" apple car            ",
                " banan dog            ",
                " a                    ")
  expect_equal(result, expected)
})

test_that("create_table_body handles empty cells", {
  data <- data.frame(col1=c("apple", ""), col2=c("", "dog"))
  result <- create_table_body(data, 20)
  expected <- c("apple               ",
                "          dog       ")
  expect_equal(result, expected)
})
