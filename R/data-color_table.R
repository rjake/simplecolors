#' Table of available colors
#'
#' This is a list of simplified color names
#'
#' @format A data frame with 200 observations and 15 variables
#' \describe{
#' \item{H360}{hue on a 0-360 scale}
#' \item{L1}{lightness on a 0-1 scale}
#' \item{S1}{saturation on a 0-1 scale}
#' \item{light}{the light value used in the package, 0-7}
#' \item{color}{the base color name (hue), red, cyan, etc.}
#' \item{letter}{the first letter of the color, for building palettes}
#' \item{sat}{the saturation value used in the package, "bright", "muted", "dull", or blank ""}
#' \item{color_sat}{the color + the saturation, ex: "brightblue", "dullred"}
#' \item{color_name}{the final unique name: color_sat + lightness, ex: "brightblue2", "mutedorange3"}
#' \item{H1}{hue on a 0-1 scale}
#' \item{HEX}{the hex code of the color}
#' \item{R}{the red of the RGB value}
#' \item{G}{the green of the RGB value}
#' \item{B}{the blue of the RGB value}
#' \item{255}{for convenience as some HLS seleciton tools use a 0-255 scale}
#' }
"color_table"
