#' @keywords internal
#' @import ggplot2
#' @import dplyr
#' @import stringr
#' @import stringi
#' @import tidyr
#' @import rlang
#' @importFrom grDevices cairo_pdf
#' @importFrom stats AIC BIC coef nls predict vcov as.formula time quantile aggregate
#' @importFrom utils globalVariables
#' @importFrom ggplot2 scale_color_manual scale_x_continuous scale_y_continuous scale_x_log10 scale_y_log10
#' @importFrom ggtext element_markdown
#' @importFrom reshape2 melt
#' @importFrom gridExtra grid.arrange
#' @importFrom mgcv gam
#' @importFrom minpack.lm nlsLM
#' @importFrom stats setNames reshape
#' @importFrom scales hue_pal
"_PACKAGE"

## Global variables used across non-standard evaluation (tidy eval)
utils::globalVariables(c(
  ".data", ".grouping", "model", "fitted_correct", "accumulated_advantage",
  "attention_threshold", "group_temp", "level_temp", "age_metric", "response",
  "p", "r", "label_both", "cairo_pdf", "time"
))
