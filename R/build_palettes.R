#' simplecolors color names
#'
#' @param color the unique color names used in the package, ex: "brightred5", "grey4", "dullblue2"
#'
#' @export
#'
#' @importFrom stats setNames
#'
#' @examples
#' sc(c("violet4", "brightteal3"))
#'
sc <- function(color = "teal5") {
  sc_names <-
    setNames(
      color_table$hex,
      color_table$color_name
    )

  unname(sc_names[color])

}



#' Title
#'
#' @param df passing a data frame with `H360`, `color`, `letter`, `sat`, `light` and `hex`
#'
#' @export
#'
#' @importFrom ggplot2 ggplot aes facet_grid geom_tile scale_y_reverse scale_fill_identity labs
#'
#' @examples
#'
#' sc_within("teal", sat = unique(color_table$sat), return = "table") %>% show_palette()
#'
show_palette <- function(df = color_table) {
  df %>%
    ggplot() +
    facet_grid(.~H360 + color + letter, scales = "free_x") +
    geom_tile(aes(x = sat, y = light, fill = hex), color = "white") +
    scale_y_reverse(breaks = 0:9) +
    scale_fill_identity() +
    labs(
      x = "Saturation",
      y = "Light"
    )
}


#' Helper function to print output
#'
#' @param df a dataframe built from color_table
#' @param return defaults to returning hex codes but can also return a table or plot of the generated palette
#'
#'
#' @importFrom dplyr select arrange pull
#' @importFrom forcats fct_reorder

specify_output <- function(df, return = NULL){
  if (missing(return) | is.null(return)) {

    df %>% pull(hex)

  } else if (return == "table") {

    df %>% select(color_name, hex)

  } else if (return == "plot") {

    df %>% show_palette()

  }
}



#' Generates a palette within 1 hue
#'
#' @param hue ex: "red", "blue", "violet"
#' @param light the lightness of the color, ex: 1:7, best at 2:6
#' @param sat the saturation of the color, ex: "bright", "muted", "dull" or "" (base)
#' @param return defaults to returning hex codes but can also return a table or plot of the generated palette
#'
#' @export
#'
#' @importFrom dplyr filter left_join mutate select arrange pull
#' @importFrom forcats fct_reorder
#'
#' @examples
#'
#' sc_within("violet", 1:6, "bright" , return = "table")
#' sc_within("violet", 2:4, c("bright", "muted"), return = "plot")
#' sc_within("red", 1:3)
#'
#'
sc_within <- function(hue,
                      light = c(2:6),
                      sat = "",
                      return = NULL) {


  l_order <-
    data.frame(
      light = light,
      l_ord = seq_along(light)
    )

  s_order <-
    data.frame(
      sat = sat,
      s_ord = seq_along(sat)
    )

  df <-
    suppressWarnings(
      color_table %>%
        filter(
          color %in% !! hue,
          light %in% !! light,
          (!! hue == "grey" | sat %in% !! sat)
      ) %>%
      left_join(l_order, by = "light") %>%
      left_join(s_order, by = "sat") %>%
      mutate(sat = fct_reorder(factor(sat), s_ord)) %>%
      arrange(l_ord, s_ord)
    )

  #df <- df[which(df$L == !!L),]
  specify_output(df, return)

}




#' Title
#'
#' @noRd
#

make_color_function <- function(hue, light = 2:6, sat = "", ...) {
  sc_within(
    hue = hue,
    light = light,
    sat = sat,
    ...
  )
}
# sc_red <- function(...) make_color_function("red", ...)
# sc_orange <- function(...) make_color_function("orange", ...)
# sc_yellow <- function(...) make_color_function("yellow", ...)
# sc_green <- function(...) make_color_function("green", ...)
# sc_teal <- function(...) make_color_function("teal", ...)
# sc_blue <- function(...) make_color_function("blue", ...)
# sc_violet <- function(...) make_color_function("violet", ...)
# sc_pink <- function(...) make_color_function("pink", ...)
# sc_grey <- function(...) make_color_function("blue", ...)
# sc_teal(1:3)
# sc_teal(1:3, sat = "dull")
# sc_teal(1:3, sat = "bright", return = "table")
# sc_teal(1:3, sat = "muted", return = "plot")

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

#' Generates a palette within across hues
#'
#' @param palette the first letter of each hue to include
#' @param light the lightness value to hold constant (1:7)
#' @param sat the saturation value to hold constant ("bright", "muted", "dull", "")
#' @param return defaults to returning hex codes but can also return a table or plot of the generated palette
#'
#' @export
#'
#' @importFrom dplyr filter left_join mutate
#' @importFrom forcats fct_inorder
#' @importFrom stringr str_detect str_extract_all
#' @importFrom stats setNames
#'
#' @examples
#' sc_across()
#' sc_across(sat = "bright", return = "plot")
#' sc_across(sat = c("bright", "muted"), return = "plot")
#' sc_across(return = "plot")
#'
#' sc_across(palette = "BO", sat = "bright", return = "plot")
#' sc_across(palette = "BO", sat = "bright", return = "table")
#' sc_across(palette = "RYCMG", light =  c(1,4), return = "plot")
#'

sc_across <- function(palette = "ROYGTBVPGy",
                      light = 3,
                      sat = "",
                      return = NULL) {

  use_colors <- str_extract_all(palette, "[A-Z][y]?") %>% unlist()

  filter_df <-
    color_table %>%
    filter(
      light == !! light,
      (sat == !! sat | (str_detect(palette, "Gy") & letter == "Gy"))
    )

  pal_names <-
    setNames(
      filter_df$hex,
      filter_df$letter
    )

  df <-
    suppressMessages(
      data.frame(
        hex = unname(pal_names[use_colors]),
        stringsAsFactors = FALSE
      ) %>%
        left_join(filter_df) %>%
        mutate(H360 = fct_inorder(factor(H360)))
    )

  specify_output(df, return)

}




#' shows the base hues for reference
#'
#' @export
#'
#' @importFrom ggplot2 labs theme element_blank
#'
#' @examples
#' show_hues()
#'
show_hues <- function(){
  sc_across(return = "plot") +
    labs(x = "also grey", y = "") +
    theme(
      axis.ticks = element_blank(),
      axis.text = element_blank()
    )

  # color_table %>%
  #   filter(S1 == 1, L1 == 0.45) %>%
  #   ggplot(aes("", "", fill = hex)) +
  #   geom_tile() +
  #   scale_fill_identity() +
  #   facet_grid(H360 + color  ~ .) +
  #   theme(
  #     strip.text.y = element_text(angle = 0),
  #     axis.ticks = element_blank()
  #   ) +
  #  labs(x = "also grey", y = "")
}



