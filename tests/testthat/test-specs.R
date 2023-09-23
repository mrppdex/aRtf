# For each of the specifications, we could compare the produced headers with the expected headers.

### TEST

test_that("Simple header with no sublevels is correctly created", {
  # 1. Simple header with no sublevels.
  specs1 <- list(
    list(label='Column A', just='c'),
    list(label='Column B', just='c'),
    list(label='Column C', just='c')
  )

  actual_header_whole <- create_header_from_specs(specs1, 100)$header
  actual_header_split <- strsplit(actual_header_whole, '\n')[[1]]
  expected_header <- c(
    "----------------------------------------------------------------------------------------------------",
    "            Column A                         Column B                         Column C              ",
    "----------------------------------------------------------------------------------------------------"
  )
    expect_equal(actual_header_split, expected_header)
})

test_that("Header with one sublevel is correctly created", {
  # 2. Header with one sublevel.
  specs2 <- list(
    list(label='Main A', just='c', sub=list(
      list(label='Sub A1', just='c'),
      list(label='Sub A2', just='c')
    )),
    list(label='Main B', just='c')
  )

  actual_header_whole <- create_header_from_specs(specs2, 100)$header
  actual_header_split <- strsplit(actual_header_whole, '\n')[[1]]
  expected_header <- c(
    "----------------------------------------------------------------------------------------------------",
    "                     Main A                                            Main B                       ",
    "         Sub A1                   Sub A2                                                            ",
    "----------------------------------------------------------------------------------------------------"
  )
  expect_equal(actual_header_split, expected_header)
})

test_that("Header with two sublevels is correctly created", {
  # 3. Header with two sublevels.
  specs3 <- list(
    list(label='Header A', just='c', sub=list(
      list(label='Middle A1', just='c', sub=list(
        list(label='Sub A1.1', just='c'),
        list(label='Sub A1.2', just='c')
      ))
    )),
    list(label='Header B', just='c')
  )

  actual_header_whole <- create_header_from_specs(specs3, 100)$header
  actual_header_split <- strsplit(actual_header_whole, '\n')[[1]]
  expected_header <- c(
    "----------------------------------------------------------------------------------------------------",
    "                    Header A                                          Header B                      ",
    "                    Middle A1                                                                       ",
    "        Sub A1.1                 Sub A1.2                                                           ",
    "----------------------------------------------------------------------------------------------------"
  )
  expect_equal(actual_header_split, expected_header)
})

test_that("Header with irregular nesting is correctly created", {
  # 10. Header with irregular nesting.
  specs10 <- list(
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

  actual_header_whole <- create_header_from_specs(specs10, 100)$header
  actual_header_split <- strsplit(actual_header_whole, '\n')[[1]]
  expected_header <- c(
    "----------------------------------------------------------------------------------------------------",
    "                   Irregular A                                       Irregular B                    ",
    "         Sub A1                   Sub A2                                                            ",
    "                                Deep A2.1                                                           ",
    "                              Deeper A2.1.1                                                         ",
    "----------------------------------------------------------------------------------------------------"
  )
  expect_equal(actual_header_split, expected_header)
})

test_that("Column label longer than allocated space triggers a warning", {
  specs11 <- list(
    list(label='1234567890')
  )
  expect_warning(create_header_from_specs(specs11, 9),
                 'Column label "1234567890" is truncated. Increase the column width.')
  expect_no_warning(create_header_from_specs(specs11, 10))
})
