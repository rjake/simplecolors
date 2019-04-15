#' Build table of colors to use
#'
#' Create a table using the HLS framework, hue values on 0-1, 0-255, and 0-360 ranges
#'
#' @importFrom dplyr mutate recode rowwise select ungroup
#' @importFrom forcats fct_reorder
#' @importFrom colorspace hex HLS
#' @importFrom grDevices col2rgb
#'
#' @return data.frame
#' @export
#'
#' @examples
#'

build_colors <- function() {

  hue_values <- # seq(0, 330, round(330 / 11, 0))
    c(0, 40, 60, 120, 180, 220, 270, 300)

  l_values <- c(.2, .3, .45, .6, .75, .9)
  s_values <- c(.25, .5, .75, 1)

    # all colors, 12 hues, 6 light, and 4 saturation values
  expand.grid(
    H360 = hue_values,
    L1 = l_values,
    S1 = s_values
  ) %>%
    # add grey scale
    rbind(expand.grid(
      H360 = 360,
      L1 = l_values,
      S1 = 0
    )) %>%
    mutate(
      light = as.integer(factor(L1, levels = rev(l_values))),
      S =
        ifelse(
          H360 == 360, 0, as.integer(factor(S1, levels = rev(s_values)))
        )
    ) %>%
  # add white & black
  rbind(c(360, 0, 0, 7, 0)) %>%
  rbind(c(360, 1, 0, 0, 0)) %>%
  mutate(
       color =
         recode(
           H360,
           "0" = "red",
          "40" = "orange",
          "60" = "yellow",
          "120" = "lime",
          "180" = "cyan",
          "220" = "blue",
          "270" = "violet",
          "300" = "magenta",
          "360" = "grey"
          ),
       letter = toupper(substr(color, 1, 1)),
       sat =
        recode(S,
          "0" = "",
          "1" = "bright",
          "2" = "",
          "3" = "muted",
          "4" = "dull"
        ),
       sat = fct_reorder(sat, S, max),
       color_sat = paste0(sat, color),
       color_name = paste0(color_sat, light)
    ) %>%
    # get hex & RGB codes
    rowwise() %>%
    mutate(
      H1 = round(H360 / 360, 2),
      hex = hex(HLS(H360, L1, S1)),
      R = col2rgb(hex)[1],
      G = col2rgb(hex)[2],
      B = col2rgb(hex)[3],
      H255 = round(H1 * 255, ifelse(color == "grey", 0, -1))
    ) %>%
    ungroup() %>%
    select(-S)

  #write.csv(color_values, "colors.csv", row.names = F)
}


#' Show all available colors
#'
#' Based on the \code{\link{build_colors}} function
#'
#' @param hue_max
#'
#' @return ggplot
#' @export
#' @importFrom dplyr mutate
#' @importFrom forcats fct_rev
#' @importFrom ggplot2 ggplot aes facet_grid geom_tile scale_fill_identity labs
#'
#' @examples
#'
#' show_colors(display_type = "colorbuildr")
#' show_colors(display_type = "1")
#' show_colors(display_type = "255")
#' show_colors(display_type = "260")

show_colors <- function(display_type = "colorbuildr") {

  df <-
    color_table %>%
    mutate(
      use_h = H360,
      use_l = L1,
      use_s = S1
    )

  if (display_type == "255") {

    df <- df %>% mutate(use_h = H255)

  } else if (display_type == "1") {

    df <-
      df %>%
      mutate(
        use_h = H1,
        use_l = L1,
        use_s = S1
      )

  } else {
    df <-
      df %>%
      mutate(
        use_l = fct_rev(factor(light)),
        use_s = sat
      )
  }

  df %>%
    ggplot() +
    facet_wrap(~use_h + paste0(color, " (", letter, ")"), nrow = 2) +
    geom_tile(aes(x = factor(use_s), y = factor(use_l), fill = hex), color = "white") +
    scale_fill_identity() +
    labs(
      x = "Saturation",
      y = "Light",
      title = paste("Hue referencing", display_type)
    )
}

