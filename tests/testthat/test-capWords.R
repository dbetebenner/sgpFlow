test_that("capWords converts all caps to mixed case correctly", {
  expect_equal(capWords("TEST"), "Test")
  expect_equal(capWords("JOHN'S"), "John's")
  expect_equal(capWords("O'NEIL"), "O'Neil")
})

test_that("capWords handles special.words correctly", {
  expect_equal(capWords("ELA TEST"), "ELA Test")
  expect_equal(capWords("SGP RESULTS"), "SGP Results")
  expect_equal(capWords("CUSD HIGH SCHOOL"), "CUSD High School")
})

test_that("capWords preserves numbers and punctuation", {
  expect_equal(capWords("TEST1 TEST2"), "Test1 Test2")
  expect_equal(capWords("2020 RESULTS"), "2020 Results")
  expect_equal(capWords("PIE-CHART DATA"), "Pie-Chart Data")
  expect_equal(capWords("(TEST) RESULTS"), "(Test) Results")
})

test_that("capWords trims and cleans spaces correctly", {
  expect_equal(capWords("  TEST  "), "Test")
  expect_equal(capWords("TEST   RESULTS"), "Test Results")
  expect_equal(capWords("TEST.RESULTS"), "Test Results")
})

test_that("capWords returns appropriate values for null or NA input", {
  expect_null(capWords(NULL))
  expect_equal(capWords(NA), NA)
  expect_equal(capWords(" "), " ")
})

