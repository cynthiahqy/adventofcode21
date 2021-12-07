test_that("f06a example after 80 days returns 5934", {
  init <- f06_init_fish(example_data_06())
  expect_equal(f06a(init, 80), 5934)
})
