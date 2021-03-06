#' Day 08: Seven Segment Search
#'
#' [Seven Segment Search](https://adventofcode.com/2021/day/8)
#'
#' @name day08
#' @rdname day08
#' @details
#'
#' **Part One**
#'
#' You barely reach the safety of the cave when the whale smashes into the
#' cave mouth, collapsing it. Sensors indicate another exit to this cave at
#' a much greater depth, so you have no choice but to press on.
#'
#' As your submarine slowly makes its way through the cave system, you
#' notice that the four-digit [seven-segment
#' displays](https://en.wikipedia.org/wiki/Seven-segment_display) in your
#' submarine are malfunctioning; [they must have been
#' damaged]{title="Yes, just the four-digit seven-segment ones. Whole batch must have been faulty."}
#' during the escape. You\'ll be in a lot of trouble without them, so
#' you\'d better figure out what\'s wrong.
#'
#' Each digit of a seven-segment display is rendered by turning on or off
#' any of seven segments named `a` through `g`:
#'
#'       0:      1:      2:      3:      4:
#'      aaaa    ....    aaaa    aaaa    ....
#'     b    c  .    c  .    c  .    c  b    c
#'     b    c  .    c  .    c  .    c  b    c
#'      ....    ....    dddd    dddd    dddd
#'     e    f  .    f  e    .  .    f  .    f
#'     e    f  .    f  e    .  .    f  .    f
#'      gggg    ....    gggg    gggg    ....
#'
#'       5:      6:      7:      8:      9:
#'      aaaa    aaaa    aaaa    aaaa    aaaa
#'     b    .  b    .  .    c  b    c  b    c
#'     b    .  b    .  .    c  b    c  b    c
#'      dddd    dddd    ....    dddd    dddd
#'     .    f  e    f  .    f  e    f  .    f
#'     .    f  e    f  .    f  e    f  .    f
#'      gggg    gggg    ....    gggg    gggg
#'
#' So, to render a `1`, only segments `c` and `f` would be turned on; the
#' rest would be off. To render a `7`, only segments `a`, `c`, and `f`
#' would be turned on.
#'
#' The problem is that the signals which control the segments have been
#' mixed up on each display. The submarine is still trying to display
#' numbers by producing output on signal wires `a` through `g`, but those
#' wires are connected to segments *randomly*. Worse, the wire/segment
#' connections are mixed up separately for each four-digit display! (All of
#' the digits *within* a display use the same connections, though.)
#'
#' So, you might know that only signal wires `b` and `g` are turned on, but
#' that doesn\'t mean *segments* `b` and `g` are turned on: the only digit
#' that uses two segments is `1`, so it must mean segments `c` and `f` are
#' meant to be on. With just that information, you still can\'t tell which
#' wire (`b`/`g`) goes to which segment (`c`/`f`). For that, you\'ll need
#' to collect more information.
#'
#' For each display, you watch the changing signals for a while, make a
#' note of *all ten unique signal patterns* you see, and then write down a
#' single *four digit output value* (your puzzle input). Using the signal
#' patterns, you should be able to work out which pattern corresponds to
#' which digit.
#'
#' For example, here is what you might see in a single entry in your notes:
#'
#'     acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab |
#'     cdfeb fcadb cdfeb cdbaf
#'
#' (The entry is wrapped here to two lines so it fits; in your notes, it
#' will all be on a single line.)
#'
#' Each entry consists of ten *unique signal patterns*, a `|` delimiter,
#' and finally the *four digit output value*. Within an entry, the same
#' wire/segment connections are used (but you don\'t know what the
#' connections actually are). The unique signal patterns correspond to the
#' ten different ways the submarine tries to render a digit using the
#' current wire/segment connections. Because `7` is the only digit that
#' uses three segments, `dab` in the above example means that to render a
#' `7`, signal lines `d`, `a`, and `b` are on. Because `4` is the only
#' digit that uses four segments, `eafb` means that to render a `4`, signal
#' lines `e`, `a`, `f`, and `b` are on.
#'
#' Using this information, you should be able to work out which combination
#' of signal wires corresponds to each of the ten digits. Then, you can
#' decode the four digit output value. Unfortunately, in the above example,
#' all of the digits in the output value (`cdfeb fcadb cdfeb cdbaf`) use
#' five segments and are more difficult to deduce.
#'
#' For now, *focus on the easy digits*. Consider this larger example:
#'
#'     be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb |
#'     fdgacbe cefdb cefbgd gcbe
#'     edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec |
#'     fcgedb cgb dgebacf gc
#'     fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef |
#'     cg cg fdcagb cbg
#'     fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega |
#'     efabcd cedba gadfec cb
#'     aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga |
#'     gecf egdcabf bgf bfgea
#'     fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf |
#'     gebdcfa ecba ca fadegcb
#'     dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf |
#'     cefg dcbef fcge gbcadfe
#'     bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd |
#'     ed bcgafe cdgba cbgef
#'     egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg |
#'     gbdfcae bgc cg cgb
#'     gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc |
#'     fgae cfgab fg bagce
#'
#' Because the digits `1`, `4`, `7`, and `8` each use a unique number of
#' segments, you should be able to tell which combinations of signals
#' correspond to those digits. Counting *only digits in the output values*
#' (the part after `|` on each line), in the above example, there are `26`
#' instances of digits that use a unique number of segments (highlighted
#' above).
#'
#' *In the output values, how many times do digits `1`, `4`, `7`, or `8`
#' appear?*
#'
#' **Part Two**
#'
#' *(Use have to manually add this yourself.)*
#'
#' *(Try using `convert_clipboard_html_to_roxygen_md()`)*
#'
#' @param x some data
#' @return For Part One, `f08a(x)` returns .... For Part Two,
#'   `f08b(x)` returns ....
#' @export
#' @examples
#' f08a(example_data_08())
#' f08b()
f08a <- function(x) {
  `%>%` <- magrittr::`%>%`
  easy_segs <- c(one = 2, four = 4, seven = 3, eight = 7)

  df <- readr::read_delim(x, delim = "|", col_names = c("sign", "out"), trim_ws = TRUE)

  df %>%
    dplyr::select(out) %>%
    tidyr::separate(., out, into = paste0("out0", 1:4)) %>%
    dplyr::mutate(dplyr::across(out01:out04, nchar)) %>%
    tidyr::pivot_longer(cols = c(out01:out04)) %>%
    dplyr::filter(value %in% easy_segs) %>%
    nrow()
}


#' @rdname day08
#' @export
f08b <- function(x) {
  `%>%` <- magrittr::`%>%`
  signals <- c(paste0("sign", 1:9), "sign10")
  output <- paste0("out0", 1:4)
  dig_len <- tibble::tribble(~digit, ~n_seg,
                             1,     2,
                             2,     5,
                             3,     5,
                             4,     4,
                             5,     5,
                             6,     6,
                             7,     3,
                             8,     7,
                             9,     6,
                             0,     6)
  easy_segs <- c(one = 2, four = 4, seven = 3, eight = 7)
  easy_len <- dig_len %>% dplyr::filter(n_seg %in% easy_segs)

  df <- readr::read_table2(x, col_names = c(signals, "delim", output)) %>%
    dplyr::select(-delim) %>%
    tibble::rowid_to_column() %>%
    tidyr::pivot_longer(c(-rowid)) %>%
    dplyr::mutate(len = nchar(value)) %>%
    dplyr::left_join(x = ., y = easy_len, by = c("len" = "n_seg"))
}


f08_helper <- function(x) {

}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day08
#' @export
example_data_08 <- function(example = 1) {
  l <- list(
    a = c(


    )
  )
  l[[example]]
}
