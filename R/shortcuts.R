#' Generates a palette within 1 hue
#'
#' @inheritParams sc_within
#'
#' @export
#' @noRd

make_color_function <- function(hue, light = 2:5, sat = "", ...) {
  sc_within(
    hue = hue,
    light = light,
    sat = sat,
    ...
  )
}


#' Generates a palette of reds
#' @inheritParams sc_within
#' @export
#' @rdname sc_within
sc_red <- function(...) make_color_function("red", ...)

#' Generates a palette of oranges
#' @inheritParams sc_within
#' @export
#' @rdname sc_within
sc_orange <- function(...) make_color_function("orange", ...)

#' Generates a palette of yellows
#' @inheritParams sc_within
#' @export
#' @rdname sc_within
sc_yellow <- function(...) make_color_function("yellow", ...)

#' Generates a palette of greens
#' @inheritParams sc_within
#' @export
#' @rdname sc_within
sc_green <- function(...) make_color_function("green", ...)

#' Generates a palette of teals
#' @inheritParams sc_within
#' @export
#' @rdname sc_within
sc_teal <- function(...) make_color_function("teal", ...)

#' Generates a palette of blues
#' @inheritParams sc_within
#' @export
#' @rdname sc_within
sc_blue <- function(...) make_color_function("blue", ...)

#' Generates a palette of violets
#' @inheritParams sc_within
#' @export
#' @rdname sc_within
sc_violet <- function(...) make_color_function("violet", ...)

#' Generates a palette of pinks
#' @inheritParams sc_within
#' @export
#' @rdname sc_within
sc_pink <- function(...) make_color_function("pink", ...)

#' Generates a palette of greys
#' @inheritParams sc_within
#' @export
#' @rdname sc_within
sc_grey <- function(...) make_color_function("blue", ...)



# make_color_functions <- function() {
#
#   unique_colors <- unique(color_table$color)
#
#   for (i in seq_along(unique_colors)) {
#
#     assign(
#       paste0("delete_", unique_colors[i]),
#       function(light = 2:5, sat = "", ..., envir = current()) {
#         sc_within(
#           hue = unique_colors[i],
#           light = light,
#           sat = sat,
#           ...
#         )
#       },
#       inherits = T,
#       envir = sys.frame()
#     )
#
#     print(i)
#     print(unique_colors[i])
#   }
# }
#make_color_functions()
