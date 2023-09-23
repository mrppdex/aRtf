test_that("substr_replace replaces substring correctly", {
  expect_equal(substr_replace("hello world", "new", 7, 11), "hello new")
  expect_equal(substr_replace("1234567890", "abc", 3, 5), "12abc67890")
})

test_that("justify_text justifies text correctly", {
  expect_equal(justify_text("hello", 10, 'c'), "  hello   ")
  expect_equal(justify_text("hello", 10, 'r'), "     hello")
  expect_equal(justify_text("hello", 10, 'l'), "hello     ")
})

test_that("split_text splits text into lines correctly", {
  expect_equal(
    split_text('lorem bla baglk asdf asdfasfd  fdsafasdf', 15),
    c("lorem bla baglk", "asdf asdfasfd", "fdsafasdf")
  )
  expect_equal(split_text('a b c d e f g h i j', 5), c('a b c', 'd e f', 'g h i', 'j'))
  expect_equal(split_text('123456789', 3), c('123','456','789'))
})

