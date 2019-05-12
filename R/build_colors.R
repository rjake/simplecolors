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
#'

build_colors <- function() {

  hue_values <- # seq(0, 330, round(330 / 11, 0))
    c(0, 40, 60, 120, 180, 220, 270, 300)

  l_values <- c(.15, .3, .58, .78, .92)
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
          "120" = "green",
          "180" = "teal",
          "220" = "blue",
          "270" = "violet",
          "300" = "pink",
          "360" = "grey"
          ),
       letter = ifelse(color == "grey", "Gy", toupper(substr(color, 1, 1))),
       sat =
        recode(S,
          "0" = "",
          "1" = "bright",
          "2" = "",
          "3" = "muted",
          "4" = "dull"
        ),
       sat = fct_reorder(sat, S, max),
       color_sat = paste0(sat, tolower(color)),
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
    select(-S) %>%
    rbind(
      filter(., sat == "", light == 3) %>%
        mutate(
          color_name = gsub("3", "", color_name),
          light = NA_integer_
        )
    )

  #write.csv(color_values, "colors.csv", row.names = F)
}
#color_table <- build_colors()
#usethis::use_data(color_table, overwrite = TRUE)
#show_colors()

#' Show all available colors
#'
#' Based on the \code{\link{build_colors}} function
#'
#' @param display_type a string "1", "255", "360", or blank. This will show the colors based on different hue limits "1" for 0-1, "255" for 0-255, "360" for 0-360. Otherwise, the default is the built-in colornames.
#'
#' @return ggplot
#' @export
#' @importFrom dplyr mutate
#' @importFrom forcats fct_rev
#' @importFrom ggplot2 ggplot aes facet_wrap geom_tile scale_fill_identity labs
#'
#' @examples
#'
#' show_colors(display_type = "simplecolors")
#' show_colors(display_type = "1")
#' show_colors(display_type = "255")
#' show_colors(display_type = "260")

show_colors <- function(display_type = "simplecolors") {

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

  #vignette("spc", "rocqi")
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

