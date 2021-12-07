library(adventofcode21)
x <- readLines("./inst/input05.txt")

p1 <- f05(x, .inclDiag = FALSE)
p2 <- f05(x, .inclDiag = TRUE)

stopifnot(p1 == aoc_solutions$day05a)
stopifnot(p2 == aoc_solutions$day05b)
