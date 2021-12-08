library(adventofcode21)
x <- readLines("./inst/input06.txt")

init <- f06_init_fish(x)
p1 <- f06a(init, days = 80)
p2 <- f06b(init, days = 256)

stopifnot(p1 == aoc_solutions$day06a)
stopifnot(p2 == aoc_solutions$day06b)
