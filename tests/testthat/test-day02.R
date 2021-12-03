test_that("f02a works", {
  x <- example_data_02()
  expect_equal(f02a(x), 150)
})

test_that("f02b works", {
  x <- example_data_02()
  expect_equal(f02b(x), 900)
})
