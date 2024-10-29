context("strHead tests")

test_that("strHead correctly returns the first two characters for each string", {
  strHead_vector <- c("ab", "de", " 3")
  using_strHead <- strHead(c("abc", "defg", " 3456"), 2)

  expect_equal(
    using_strHead, strHead_vector
  )
})

test_that("strHead correctly handles negative n_char to remove characters from the end", {
  strHead_vector <- c("a", "de", " 34")
  using_strHead <- strHead(c("abc", "defg", " 3456"), -2)

  expect_equal(
    using_strHead, strHead_vector
  )
})

test_that("strHead throws an error when n_char is out of bounds", {
  expect_error(strHead("abc", 10), "n_char is out of bounds for string: abc")
})

test_that("strHead handles invalid input gracefully", {
  expect_error(strHead(123, 2), "Each element must be a character string.")
})
