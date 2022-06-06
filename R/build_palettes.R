#' Specify color(s) by name
#'
#' @param ... the unique color names used in the package, ex: "brightred5",
#' "grey4", "dullblue2"
#' @param with_names if TRUE vector will be a named vector and can be called as
#' 'my_colors$red' and 'my_colors$blue'
#' #' @export
#'
#' @importFrom purrr set_names
#'
#' @examples
#'
#' sc("violet4", "brightteal3")
#'
#' # You can return the names of the colors using 'with_names = TRUE'
#' sc("violet4", "brightteal3", with_names = TRUE)
#'
#' # If the colors you picked go across hues, you might want to shorten them
#' # so that 'mutedred2' and 'mutedblue2' become 'red' and 'blue'.
#' # The 'set_names' function from purrr lets you pass a function like this.
#' # As a named vector, you can use 'my_colors$red' and 'my_colors$blue'
#' sc("mutedred2", "mutedblue2", with_names = TRUE) %>%
#'   purrr::set_names(stringr::str_remove_all, "muted|\\d")
sc <- function(..., with_names = FALSE) {
  sc_names <-
    setNames(
      simplecolors::color_table$hex,
      simplecolors::color_table$color_name
    )

  color_names <- sc_names[c(...)]

  if (with_names) {
    return(color_names)
  }

  unname(color_names)
}



#' Helper function for displaying palette for sc_within or sc_across
#'
#' @param df passing a data frame with `H360`, `color`, `letter`, `sat`, `light` and `hex`
#'
#' @importFrom ggplot2 ggplot aes facet_grid geom_tile scale_y_reverse scale_fill_identity labs
#' @noRd
#' @examples
#' show_palette(head(color_table, 8*3))
show_palette <- function(df = simplecolors::color_table) {
  if (!"label" %in% names(df)) {
    df <-
      df %>%
      mutate(label = paste0(color, "\n(", letter, ")"))
  }

  df %>%
    ggplot() +
    facet_grid(.~label, scales = "free_x") +
    geom_tile(aes(x = sat, y = light, fill = hex), color = "white") +
    scale_y_reverse(breaks = 0:9) +
    scale_fill_identity() +
    labs(
      x = "Saturation",
      y = "Light"
    )
}


#' Helper function to print output
#' @param df a dataframe built from color_table
#' @param return defaults to returning hex codes but can also return a table or plot of the generated palette
#' @importFrom dplyr select arrange pull
#' @importFrom forcats fct_reorder
#' @noRd
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
#' @param light the lightness of the color, ex: 1:5
#' @param sat the saturation of the color, ex: "bright", "muted", "dull" or "" (base)
#' @param return defaults to returning hex codes but can also return a table or plot of the generated palette
#'
#' @export
#'
#' @importFrom dplyr filter left_join mutate select arrange pull
#' @importFrom forcats fct_reorder
#'
#' @family palettes
#'
#' @examples
#' sc_within("violet", 1:3)
#' sc_within("violet", 1:5, "bright" , return = "table")
#' sc_within("violet", 2:4, c("bright", "muted"), return = "plot")
sc_within <- function(hue,
                      light = c(2:5),
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
      simplecolors::color_table %>%
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

  specify_output(df, return)

}




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
#' @family palettes
#'
#' @examples
#' sc_across(palette = "BO")
#' sc_across(palette = "BO", sat = "bright", return = "table")
#' sc_across(palette = "BO", sat = "bright", return = "plot")
#' sc_across(palette = "RBTVPGy", light = 4, return = "plot")
sc_across <- function(palette = "ROYGTBVPGy",
                      light = 3,
                      sat = "",
                      return = NULL) {

  use_colors <- str_extract_all(palette, "[A-Z][y]?") %>% unlist()

  filter_df <-
    simplecolors::color_table %>%
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
        mutate(
          label = paste0(color, "\n(", letter, ")"),
          label = fct_inorder(factor(label))
        )
    )

  specify_output(df, return)

}
