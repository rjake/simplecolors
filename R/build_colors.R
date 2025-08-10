#' Build table of colors to use
#'
#' Create a table using the HLS framework, hue values on 0-1, 0-255, and 0-360 ranges
#'
#' @importFrom dplyr mutate recode left_join rowwise select ungroup
#' @importFrom forcats fct_reorder
#' @importFrom colorspace hex HLS
#' @importFrom grDevices col2rgb
#'
#' @return data.frame
#' @noRd
#'
build_colors <- function() {
  hue_values <- # seq(0, 330, round(330 / 11, 0))
    # c(0, 40, 60, 120, 180, 220, 270, 300)
    data.frame(
      H360 = c(0, 40, 60, 120, 180, 220, 270, 300, 360),
      color = c(
        "red", "orange", "yellow", "green",
        "teal", "blue", "violet", "pink", "grey"
      ),
      stringsAsFactors = FALSE
    )

  s_values <- c(.30, .50, .75, 1.00)
  l_values <- c(.20, .35, .58, .78, .90)

  color_prep <-
    # all colors, 12 hues, 5 light, and 4 saturation values
    expand.grid(
      H360 = hue_values$H360[1:8],
      L1 = l_values,
      S1 = s_values,
      KEEP.OUT.ATTRS = FALSE
    ) %>%
    # add grey scale
    rbind(expand.grid(
      H360 = 360,
      L1 = l_values,
      S1 = 0
    )) %>%
    # add white & black
    rbind(c(360, 0, 0)) %>%
    rbind(c(360, 1, 0)) %>%
    #filter(H360 == 360 & S1 == 0 | H360 != 360) %>%
    mutate(
      light = as.integer(factor(L1, levels = rev(l_values))),
      S =
        ifelse(
          H360 == 360,
          0,
          as.integer(factor(S1, levels = rev(s_values)))
        )
    ) %>%
    # get color names
    left_join(hue_values)


  final_table <-
    color_prep %>%
    mutate(
      letter = ifelse(
        color == "grey",
        "Gy",
        toupper(substr(color, 1, 1))
      ),
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
    select(-S)

  # final output, add in colors without modifiers (ex: "red3" -> "red")
  final_table %>%
    rbind(
      final_table %>%
        filter(sat == "", light == 3) %>%
        mutate(
          color_name = gsub("3", "", color_name),
          light = NA_integer_
        )
    )
}

#' Show all available colors
#'
#' @description Plots all available color values.
#' @return Returns a ggplot object
#' @details Labels can be added by using the argument \code{labels = TRUE}
#'
#' @param labels logical TRUE (default) will plot the color with color names, FALSE will plot the colors only
#'
#' @importFrom dplyr mutate
#' @importFrom forcats fct_rev
#' @importFrom ggplot2 ggplot aes facet_wrap geom_tile geom_label scale_fill_identity labs theme element_rect
#'
#' @export
#'
#' @examples
#' show_colors()
show_colors <- function(labels = FALSE) {

  df <-
    simplecolors::color_table %>%
    filter(!is.na(light)) %>%
    mutate(
      use_h = H360,
      use_l = fct_rev(factor(light)),
      use_s = sat,
      x = factor(sat),
      y = factor(light) %>% fct_rev(),
      facet =
        paste0(color, " (", letter, ")") %>%
        fct_reorder(H360)
    )

  # vignette("extending-ggplot2", "ggplot2")
  p <-
    ggplot(df, aes(x, y)) +
    facet_wrap(~facet, nrow = 3, scales = "free_y") +
    geom_tile(aes(fill = hex), color = "grey90") +
    geom_tile(
      aes(x = factor(""), y = factor(3)),
      fill = NA, color = "white", linewidth = 1.5
    ) +
    scale_fill_identity() +
    theme(panel.background = element_rect(fill = "white", color = "grey90")) +
    labs(
      x = "Saturation",
      y = "Light",
      subtitle = 'The default is a lightness of 3 and can be specified by color name alone
ex. "red", "violet", "teal"
or with modifiers "brightpink2", "mutedred3", "blue4"'
    )

  if (labels) {
    p <-
      p +
      geom_label(
        data = df,
        aes(x, y, label = color_name),
        label.size = 0, alpha = 0.8
      )
  }

  p

}

