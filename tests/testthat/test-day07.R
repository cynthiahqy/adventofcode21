test_that("f07a returns 37", {
  e <- example_data_07()
  expect_equal(f07a(e), 37)
})

test_that("f07b returns 168", {
  e <- example_data_07()
  expect_equal(f07b(e), 168)
})
