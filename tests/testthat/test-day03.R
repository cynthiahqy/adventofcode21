test_that("baseR read.table handles input03 like readr", {
  x <- example_data_03()
  readr_in <- readr::read_table(file = x, col_names = c("bin"))
  base_in <- read.table(text = x, col.names = c("bin"), colClass = c("character"))
  # df won't be equal because readr returns tibbles
  # test bin column instead
  expect_equal(readr_in$bin, base_in$bin)
})

test_that("baseR bit split behaves the same as dplyr version", {
  x <- example_data_03()
  base_split <- f03_input_helper(x)
  library(magrittr)
  tidy_split <- readr::read_table(file = x, col_names = c("bin")) %>%
    tidyr::separate(., bin, sep="", into=paste0("X", c(0:5))) %>%
    dplyr::select(-X0) %>%
    dplyr::mutate(across(.fns = as.numeric)) %>%
    as.data.frame(., stringsAsFactors = FALSE)

  expect_equal(base_split, tidy_split)
})
