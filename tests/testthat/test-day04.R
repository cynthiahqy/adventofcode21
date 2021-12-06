test_that("f04a works on example data", {
  d <- example_data_04("draws")
  b <- example_data_04("boards")
  expect_equal(f04a(d, b), 4512)
})

test_that("f04b works on example data", {
  d <- example_data_04("draws")
  b <- example_data_04("boards")
  expect_equal(f04b(d, b), 1924)
})
