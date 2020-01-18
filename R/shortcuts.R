#' @export
#' @rdname sc_within
sc_red <- function(light = 2:5, sat = "", return = NULL) {
  sc_within(
    hue = "red",
    light = light,
    sat = sat,
    return = return
  )
}


#' @export
#' @rdname sc_within
sc_orange <- function(light = 2:5, sat = "", return = NULL) {
  sc_within(
    hue = "orange",
    light = light,
    sat = sat,
    return = return
  )
}


#' @export
#' @rdname sc_within
sc_yellow <- function(light = 2:5, sat = "", return = NULL) {
  sc_within(
    hue = "yellow",
    light = light,
    sat = sat,
    return = return
  )
}


#' @export
#' @rdname sc_within
sc_green <- function(light = 2:5, sat = "", return = NULL) {
  sc_within(
    hue = "green",
    light = light,
    sat = sat,
    return = return
  )
}


#' @export
#' @rdname sc_within
sc_teal <- function(light = 2:5, sat = "", return = NULL) {
  sc_within(
    hue = "teal",
    light = light,
    sat = sat,
    return = return
  )
}


#' @export
#' @rdname sc_within
sc_blue <- function(light = 2:5, sat = "", return = NULL) {
  sc_within(
    hue = "blue",
    light = light,
    sat = sat,
    return = return
  )
}


#' @export
#' @rdname sc_within
sc_violet <- function(light = 2:5, sat = "", return = NULL) {
  sc_within(
    hue = "violet",
    light = light,
    sat = sat,
    return = return
  )
}


#' @export
#' @rdname sc_within
sc_pink <- function(light = 2:5, sat = "", return = NULL) {
  sc_within(
    hue = "pink",
    light = light,
    sat = sat,
    return = return
  )
}


#' @export
#' @rdname sc_within
sc_grey <- function(light = 2:5, sat = "", return = NULL) {
  sc_within(
    hue = "grey",
    light = light,
    sat = sat,
    return = return
  )
}


# make_color_functions <- function() {
#
#   unique_colors <- unique(color_table$color)
#
#   for (i in seq_along(unique_colors)) {
#
#     assign(
#       paste0("delete_", unique_colors[i]),
#       function(light = 2:5, sat = "", return = NULL, envir = current()) {
#         sc_within(
#           hue = unique_colors[i],
#           light = light,
#           sat = sat,
#           return = return
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
