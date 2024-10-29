context("strTail tests")

test_that("strTail correctly returns the last two characters for each string", {
  strTail_vector <- c("bc", "fg", "56")
  using_strTail <- strTail(c("abc", "defg", " 3456"), 2)

  expect_equal(
    using_strTail, strTail_vector
  )
})

test_that("strTail correctly handles negative n_char to remove characters from the start", {
  strTail_vector <- c("bc", "efg", "3456")
  using_strTail <- strTail(c("abc", "defg", " 3456"), -1)

  expect_equal(
    using_strTail, strTail_vector
  )
})

test_that("strTail throws an error when n_char is out of bounds", {
  expect_error(strTail("abc", 10), "n_char is out of bounds for string: abc")
})

test_that("strTail handles invalid input gracefully", {
  expect_error(strTail(123, 2), "Each element must be a character string.")
})
