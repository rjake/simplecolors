#' colorbuildr color names
#'
#' @param color the unique color names used in the package, ex: "brightred5", "grey4", "dullblue2"
#'
#' @export
#'
#' @importFrom stats setNames
#'
#' @examples
#' cb(c("violet4", "brightcyan3"))
#'
cb <- function(color = "grey3") {
  cb_names <-
    setNames(
      colorbuildr::color_table$hex,
      colorbuildr::color_table$color_name
    )

  unname(cb_names[color])

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
#' cb_pal_within("blue", sat = unique(color_table$sat), return = "table") %>% show_palette()
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
#' cb_pal_within("violet", 1:6, "bright" , return = "table")
#' cb_pal_within("violet", 2:4, c("bright", "muted"), return = "plot")
#' cb_pal_within("red", 1:3)
#'
#'
cb_pal_within <- function(hue,
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
      colorbuildr::color_table %>%
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

  if (missing(return)) {

    df %>% pull(hex)

  } else if (return == "table") {

    df %>% select(color_name, hex)

  } else if (return == "plot") {

    df %>% show_palette()

  }
}



#' Title
#'
#' @noRd
#

make_color_function <- function(hue, light = 2:6, sat = "", ...) {
        cb_pal_within(
          hue = hue,
          light = light,
          sat = sat,
          ...
        )
}

pal_reds <- function(...) make_color_function("red", ...)
pal_reds(light = 2, sat = "bright",return = "table")

pal_blues <- function(...) make_color_function("blue", ...)
pal_blues(sat = c("bright", "muted"), return = "plot")

# make_color_functions <- function() {
#
#   unique_colors <- unique(color_table$color)
#
#   for (i in seq_along(unique_colors)) {
#
#     assign(
#       paste0("delete_", unique_colors[i]),
#       function(light = 2:5, sat = "", ..., envir = current()) {
#         cb_pal_within(
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
#delete_red()
#rm(list = ls()[grepl("delete", ls())])
#delete_blue()
#delete_violet()







#' Generates a palette within across hues
#'
#' @param palette the first letter of each hue to include
#' @param light the lightness value to hold constant (1:7)
#' @param sat the saturation value to hold constant ("bright", "muted", "dull", "")
#' @param return defaults to returning hex codes but can also return a table or plot of the generated palette
#'
#' @export
#'
#' @importFrom dplyr filter arrange pull select
#' @importFrom stringr str_detect
#'
#' @examples
#' cb_pal_across()
#' cb_pal_across(sat = "bright", return = "plot")
#' cb_pal_across(sat = c("bright", "muted"), return = "plot")
#' cb_pal_across(return = "plot")
#'
#' cb_pal_across(palette = "BO", sat = "bright", return = "plot")
#' cb_pal_across(palette = "BO", sat = "bright", return = "table")
#' cb_pal_across(palette = "RYCMG", light =  c(1,4), return = "plot")
#'

cb_pal_across <- function(palette = "ROYLCBVMW",
                          light = 3,
                          sat = "bright",
                          return = NULL) {


    df <-
      colorbuildr::color_table %>%
      filter(
        str_detect(letter, paste0("[", palette, "]")),
        light %in% !! light,
        sat %in% !! sat
      ) %>%
      arrange(light)

    #df <- df[which(df$L == !!L),]

    if (missing(return)) {

      df %>% pull(hex)

    } else if (return == "table") {

      df %>% select(color_name, hex)

    } else if (return == "plot") {

      df %>% show_palette()

    }
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
  cb_pal_across(return = "plot") +
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
