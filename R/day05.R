#' Day 05: Hydrothermal Venture
#'
#' [Hydrothermal Venture](https://adventofcode.com/2021/day/5)
#'
#' @name day05
#' @rdname day05
#' @details
#'
#' **Part One**
#'
#' You come across a field of [hydrothermal
#' vents](https://en.wikipedia.org/wiki/Hydrothermal_vent) on the ocean
#' floor! These vents constantly produce large, opaque clouds, so it would
#' be best to avoid them if possible.
#'
#' They tend to form in *lines*; the submarine helpfully produces a list of
#' nearby [lines of vents]{title="Maybe they're Bresenham vents."} (your
#' puzzle input) for you to review. For example:
#'
#'     0,9 -> 5,9
#'     8,0 -> 0,8
#'     9,4 -> 3,4
#'     2,2 -> 2,1
#'     7,0 -> 7,4
#'     6,4 -> 2,0
#'     0,9 -> 2,9
#'     3,4 -> 1,4
#'     0,0 -> 8,8
#'     5,5 -> 8,2
#'
#' Each line of vents is given as a line segment in the format
#' `x1,y1 -> x2,y2` where `x1`,`y1` are the coordinates of one end the line
#' segment and `x2`,`y2` are the coordinates of the other end. These line
#' segments include the points at both ends. In other words:
#'
#' -   An entry like `1,1 -> 1,3` covers points `1,1`, `1,2`, and `1,3`.
#' -   An entry like `9,7 -> 7,7` covers points `9,7`, `8,7`, and `7,7`.
#'
#' For now, *only consider horizontal and vertical lines*: lines where
#' either `x1 = x2` or `y1 = y2`.
#'
#' So, the horizontal and vertical lines from the above list would produce
#' the following diagram:
#'
#'     .......1..
#'     ..1....1..
#'     ..1....1..
#'     .......1..
#'     .112111211
#'     ..........
#'     ..........
#'     ..........
#'     ..........
#'     222111....
#'
#' In this diagram, the top left corner is `0,0` and the bottom right
#' corner is `9,9`. Each position is shown as *the number of lines which
#' cover that point* or `.` if no line covers that point. The top-left pair
#' of `1`s, for example, comes from `2,2 -> 2,1`; the very bottom row is
#' formed by the overlapping lines `0,9 -> 5,9` and `0,9 -> 2,9`.
#'
#' To avoid the most dangerous areas, you need to determine *the number of
#' points where at least two lines overlap*. In the above example, this is
#' anywhere in the diagram with a `2` or larger - a total of `5` points.
#'
#' Consider only horizontal and vertical lines. *At how many points do at
#' least two lines overlap?*
#'
#' **Part Two**
#'
#' *(Use have to manually add this yourself.)*
#'
#' *(Try using `convert_clipboard_html_to_roxygen_md()`)*
#'
#' @param x some data
#' @return For Part One, `f05(x, .inclDiag = FALSE)` returns .... For Part Two,
#'   `f05(x, .inclDiag = TRUE)` returns ....
#' @export
#' @examples
#' f05(example_data_05())
f05 <- function(x, .inclDiag) {
  coords <- f05_get_line_coords(x)

  # filter for horizontal & vertical lines;
  if(!.inclDiag) {
    coords <- coords %>%
      dplyr::filter(line_type %in% c("v", "h"))
  }

  # count lines by x,y coords
  coords %>%
    dplyr::select(line_id, x, y) %>%
    dplyr::count(x, y) %>%
    dplyr::filter(n > 1) %>%
    nrow()
}


#' @rdname day05
#' @export
f05_get_line_coords <- function(x) {
  `%>%` <- magrittr::`%>%`

  # extract line start & ends
  lines <- tibble(x) %>%
    tibble::rowid_to_column(., var = "line_id") %>%
    tidyr::separate(x, into = c("start", "end"), sep = " -> ") %>%
    tidyr::pivot_longer(., c(start, end)) %>%
    tidyr::separate(., value, into = c("x", "y"), sep = ",") %>%
    dplyr::mutate(across(x:y, as.integer)) %>%
    dplyr::group_by(line_id)

  # get line coordinates
  coords <- lines %>%
    dplyr::summarise(x = list(x[1]:x[2]),
                     y = list(y[1]:y[2])) %>%
    dplyr::mutate(len_x = map_int(x, length),
                  len_y = map_int(y, length)) %>%
    dplyr::mutate(line_type = case_when(len_x == 1 ~ "v",
                                        len_y == 1 ~ "h",
                                        TRUE ~ "d")) %>%
    tidyr::unnest(., cols = c(x, y))

  return(coords)
}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day05
#' @export
example_data_05 <- function(example = 1) {
  l <- list(
    lines = c(unlist(strsplit("0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2", "\n"))
    )
  )
  l[[example]]
}
