#' Custom ggplot2 theme for decay plots
#'
#' @export
theme_scl <- function() {
  theme_minimal(base_size = 14) +
    theme(
      axis.line = element_line(size = 1, color = "black"),
      axis.ticks = element_line(size = 1, color = "black"),
      axis.text = element_text(size = 14, color = "black"),
      axis.title = element_text(size = 16, face = "bold"),
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5, color = "black"),
      plot.subtitle = element_text(size = 16, hjust = 0.5, color = "gray30"),
      legend.text = element_text(size = 12, color = "black"),
      legend.title = element_text(size = 14, face = "bold", color = "black"),
      legend.position = "top",
      legend.background = element_rect(fill = "transparent", color = NA),
      strip.text = element_text(size = 14, face = "bold", color = "black"),
      strip.background = element_rect(fill = "gray85", color = NA),
      panel.grid.major = element_line(color = "gray80", size = 0.5),
      panel.grid.minor = element_line(color = "gray90", size = 0.3),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
    )
}