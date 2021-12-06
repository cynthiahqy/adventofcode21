library(adventofcode21)
fil <- "./inst/input04.txt"

draws <- f04_get_draws(fil)
boards <- f04_get_boards(fil)

p1 <- f04a(draws, boards)
p2 <- f04b(draws, boards)

stopifnot(p1 == aoc_solutions$day04a)
stopifnot(p2 == aoc_solutions$day04b)
