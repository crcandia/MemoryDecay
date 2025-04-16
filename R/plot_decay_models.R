#' Plot Fitted Memory Decay Curves Against Observed Data
#'
#' Visualizes the fitted forgetting curve against observed attention or popularity values.
#' This function supports comparisons across groups and stratification by attention levels.
#'
#' @section Required Columns in the Fitted Data:
#' \itemize{
#'   \item \code{age_var}: Numeric variable indicating time since the event or subject (e.g., years).
#'   \item \code{observed_col}: Observed attention/popularity (e.g., survey recall, citations).
#'   \item \code{fitted_col}: Predicted values from a decay model.
#' }
#'
#' @section Optional Columns:
#' \itemize{
#'   \item \code{group_var}: Used to color lines/points by groups (e.g., same vs. different country).
#'   \item \code{group_var2}: Used to facet the plot by stratification variable (e.g., attention level).
#' }
#'
#' Supports log-log visualization to capture long-tail decay or exponential behavior.
#'
#' @param model_output Output from a model fitting function (e.g., \code{\link{fit_biexponential_model}}).
#' @param observed_col Name of the column containing observed values (default: "CurrentPopularity").
#' @param fitted_col Name of the column containing model predictions (default: "fitted_correct").
#' @param age_var Name of the age/time column (default: "age").
#' @param group_var Optional column for group-level coloring.
#' @param group_var2 Optional column for faceting (e.g., stratification by attention).
#' @param log_y Logical. Use log10 scale on the y-axis? (default: TRUE).
#' @param log_x Logical. Use log10 scale on the x-axis? (default: FALSE).
#'
#' @return A \code{ggplot} object showing observed vs fitted decay curves.
#'
#' @seealso \code{\link{plot_all_models}}, \code{\link{fit_all_models_log}}, \code{\link{plot_fitted_decay_for_publication}}
#' @export
plot_fitted_decay <- function(model_output,
                               observed_col = "CurrentPopularity",
                               fitted_col = "fitted_correct",
                               age_var = "age",
                               group_var = NULL,
                               group_var2 = NULL,
                               log_y = TRUE,
                               log_x = FALSE) {
  fitted_df <- model_output$fitted

  # Validate presence of key variables
  required_vars <- c(age_var, observed_col, fitted_col)
  missing_vars <- setdiff(required_vars, names(fitted_df))
  if (length(missing_vars) > 0) {
    stop("Missing required columns in fitted data: ", paste(missing_vars, collapse = ", "))
  }

  has_group <- !is.null(group_var) && group_var %in% names(fitted_df)
  has_facet <- !is.null(group_var2) && group_var2 %in% names(fitted_df)

  # Ensure grouping is factor if present
  if (has_group) {
    fitted_df[[group_var]] <- as.factor(fitted_df[[group_var]])
  }
  if (has_facet) {
    fitted_df[[group_var2]] <- as.factor(fitted_df[[group_var2]])
  }

  # Create base plot
  p <- ggplot(fitted_df, aes_string(x = age_var)) +
    labs(
      title = "Fitted vs. Observed Decay Curve",
      x = "Age (years)",
      y = "Attention / Popularity"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
      axis.title = element_text(face = "bold"),
      strip.background = element_rect(fill = "#f0f0f0", color = NA),
      strip.text = element_text(face = "bold", size = 12)
    )

  # Add observed points
  if (has_group) {
    p <- p + geom_point(aes_string(y = observed_col, color = group_var), alpha = 0.3, size = 2)
  } else {
    p <- p + geom_point(aes_string(y = observed_col), alpha = 0.3, size = 2, color = "#555555")
  }

  # Add fitted lines
  if (has_group) {
    p <- p + geom_line(aes_string(y = fitted_col, color = group_var), size = 1.2)
  } else {
    p <- p + geom_line(aes_string(y = fitted_col), color = "#D7263D", size = 1.2)
  }

  # Apply axis log scaling if requested
  if (log_y) p <- p + scale_y_log10()
  if (log_x) p <- p + scale_x_log10()

  # Facet if attention level column exists
  if (has_facet) {
    p <- p + facet_wrap(as.formula(paste("~", group_var2)), labeller = label_both)
  }

  return(p)
}


#' Plot Raw and Aggregated Memory Decay Data
#'
#' Creates a flexible and publication-ready visualization of memory or attention decay over time.
#' It overlays raw observations (e.g., from survey or time-series data) with smoothed or aggregated trends
#' (e.g., LOESS, GAM, or grouped means).
#'
#' The function is designed to support grouped comparisons and faceting by secondary grouping variables
#' (e.g., attention level or demographic strata). Both linear and log-log scales are supported.
#'
#' @section Input Requirements:
#' You must provide at least one of the following:
#' \itemize{
#'   \item \code{raw_df}: a data frame of raw individual-level responses.
#'   \item \code{aggregated_df}: a data frame with aggregated responses over age.
#' }
#'
#' @param raw_df (Optional) Raw individual-level dataset. Each row should represent a single observation
#' (e.g., a survey recall response). Requires columns \code{age_var} and \code{response_var_raw}.
#' @param aggregated_df (Optional) Output from \code{\link{smooth_survey_decay}} or manual aggregation. Should include
#' \code{age_var} and \code{response_var_agg}.
#' @param age_var String. Name of the column representing historical age or time since event (must be numeric).
#' @param response_var_raw String. Name of the response variable in \code{raw_df} (e.g., "correct", "recall").
#' @param response_var_agg String. Name of the aggregated response variable in \code{aggregated_df} (e.g., "mean_response", "loess_correct").
#' @param group_var String (optional). First grouping variable (e.g., "same_country", "decay_bin"). Used for coloring lines and points.
#' @param group_var2 String (optional). Second grouping variable for faceting (e.g., attention level, stratification).
#' @param log_y Logical. Whether to apply log10 transformation to the Y axis (default: TRUE).
#' @param log_x Logical. Whether to apply log10 transformation to the X axis (default: FALSE).
#' @param xlim_vals Optional numeric vector of length 2. Manual X-axis limits.
#' @param ylim_vals Optional numeric vector of length 2. Manual Y-axis limits. If using log10 and no valid values are detected, the function will request this explicitly.
#'
#' @return A \code{ggplot2} object showing raw data (if provided), aggregated trends, and stratified group patterns (if applicable).
#'
#' @seealso \code{\link{smooth_survey_decay}}, \code{\link{aggregate_mean_response}}, \code{\link{plot_fitted_decay}}
#' @export
plot_raw_memory_decay <- function(raw_df = NULL,
                                  aggregated_df = NULL,
                                  age_var,
                                  response_var_raw,
                                  response_var_agg = NULL,
                                  group_var = NULL,
                                  group_var2 = NULL,
                                  log_y = TRUE,
                                  log_x = FALSE,
                                  xlim_vals = NULL,
                                  ylim_vals = NULL) {

  if (is.null(raw_df) && is.null(aggregated_df)) {
    stop("You must provide at least `raw_df` or `aggregated_df`.")
  }

  has_group <- !is.null(group_var)
  has_facet <- !is.null(group_var2)

  # Ensure grouping variables are factors if present
  if (has_group) {
    if (!is.null(raw_df) && group_var %in% names(raw_df)) {
      raw_df[[group_var]] <- as.factor(raw_df[[group_var]])
    }
    if (!is.null(aggregated_df) && group_var %in% names(aggregated_df)) {
      aggregated_df[[group_var]] <- as.factor(aggregated_df[[group_var]])
    }
  }

  # Infer default axis limits
  if (is.null(xlim_vals) && !is.null(aggregated_df)) {
    xlim_vals <- range(aggregated_df[[age_var]], na.rm = TRUE)
  }

  # If log Y is on, we must make sure the limits are positive and finite
  if (log_y) {
    valid_y <- c()
    if (!is.null(raw_df) && response_var_raw %in% names(raw_df)) {
      valid_y <- c(valid_y, raw_df[[response_var_raw]])
    }
    if (!is.null(aggregated_df) && response_var_agg %in% names(aggregated_df)) {
      valid_y <- c(valid_y, aggregated_df[[response_var_agg]])
    }
    valid_y <- valid_y[is.finite(valid_y) & valid_y > 0]

    if (length(valid_y) == 0) {
      stop("Cannot apply log10 to Y axis: no valid positive values. Please specify `ylim_vals` manually.")
    }

    if (is.null(ylim_vals)) {
      ylim_vals <- range(valid_y, na.rm = TRUE)
    }
  } else {
    if (is.null(ylim_vals) && !is.null(aggregated_df) && !is.null(response_var_agg)) {
      ylim_vals <- range(aggregated_df[[response_var_agg]], na.rm = TRUE)
    }
  }

  # Start ggplot object
  p <- ggplot()

  # Raw data layer
  if (!is.null(raw_df) && all(c(age_var, response_var_raw) %in% names(raw_df))) {
    if (has_group && group_var %in% names(raw_df)) {
      p <- p + geom_point(
        data = raw_df,
        aes_string(x = age_var, y = response_var_raw, color = group_var),
        alpha = 0.4
      )
      if (is.null(aggregated_df)) {
        p <- p + geom_smooth(
          data = raw_df,
          aes_string(x = age_var, y = response_var_raw, color = group_var),
          method = "loess", se = FALSE, size = 1.5
        )
      }
    } else {
      p <- p + geom_point(
        data = raw_df,
        aes_string(x = age_var, y = response_var_raw),
        color = "gray50", alpha = 0.4
      )
      if (is.null(aggregated_df)) {
        p <- p + geom_smooth(
          data = raw_df,
          aes_string(x = age_var, y = response_var_raw),
          method = "loess", se = FALSE, size = 1.5, color = "black"
        )
      }
    }
  }

  # Aggregated data (curve or points)
  if (!is.null(aggregated_df) && all(c(age_var, response_var_agg) %in% names(aggregated_df))) {
    line_aes <- aes_string(x = age_var, y = response_var_agg)
    if (has_group && group_var %in% names(aggregated_df)) {
      line_aes$colour <- as.name(group_var)
    }

    if (response_var_agg == "mean_response") {
      p <- p + geom_point(data = aggregated_df, mapping = line_aes, size = 2)
    } else {
      p <- p + geom_line(data = aggregated_df, mapping = line_aes, size = 2)
    }
  }

  # Axes
  if (log_y) p <- p + scale_y_log10()
  if (log_x) p <- p + scale_x_log10()

  if (!is.null(xlim_vals) || !is.null(ylim_vals)) {
    p <- p + coord_cartesian(xlim = xlim_vals, ylim = ylim_vals)
  }

  # Titles, colors, theme
  p <- p +
    labs(
      title = "Temporal Decay of Collective Memory",
      subtitle = if (has_group) paste("Grouped by", group_var) else "Aggregated over all responses",
      x = stringr::str_to_title(gsub("_", " ", age_var)),
      y = stringr::str_to_title(gsub("_", " ", response_var_raw)),
      color = if (has_group) stringr::str_to_title(gsub("_", " ", group_var)) else NULL
    ) +
    theme_minimal(base_size = 16) +
    theme(
      plot.title = element_text(face = "bold", size = 18),
      plot.subtitle = element_text(size = 14, margin = margin(b = 10)),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 14),
      strip.text = element_text(size = 14, face = "bold")
    )

  # Facet if group_var2 is present
  if (has_facet && group_var2 %in% names(aggregated_df)) {
    p <- p + facet_wrap(as.formula(paste("~", group_var2)), labeller = label_both)
  }

  return(p)
}




#' Plot Fitted Decay Curve for Publication
#'
#' Creates a high-quality plot comparing observed and fitted memory decay curves
#' for inclusion in publications. The plot highlights temporal decay patterns
#' in attention or popularity and supports grouping and faceting by categorical variables.
#'
#' The function supports flexible axis scaling (log or linear), custom axis breaks and limits,
#' and exports to both PDF and SVG formats if desired.
#'
#' @section Requirements:
#' This function requires the following packages:
#' \itemize{
#'   \item \code{ggplot2}
#'   \item \code{ggtext}
#'   \item \code{scales}
#' }
#'
#' @param model_output A list returned by a model fitting function (e.g., \code{\link{fit_biexponential_model}}),
#' containing a data frame named \code{fitted} with predicted values.
#' @param observed_col Name of the column containing observed attention/popularity values
#' (default: "CurrentPopularity").
#' @param fitted_col Name of the column containing fitted model predictions
#' (default: "fitted_correct").
#' @param age_var Name of the column representing age or time since event (default: "age").
#' @param group_var Optional. A grouping variable to color lines and points (e.g., "same_country").
#' @param group_var2 Optional. A second grouping variable for faceting (e.g., attention level or quantile).
#' @param export_path Optional file path (without extension). If provided, saves both PDF and SVG outputs.
#' @param x_breaks Optional numeric vector of X-axis breaks.
#' @param x_limits Optional numeric vector of length 2 to set X-axis limits.
#' @param y_breaks Optional numeric vector of Y-axis breaks.
#' @param y_limits Optional numeric vector of length 2 to set Y-axis limits.
#' @param log_y Logical. If \code{TRUE}, uses log10 scale on Y-axis. Default is \code{FALSE}.
#' @param log_x Logical. If \code{TRUE}, uses log10 scale on X-axis. Default is \code{FALSE}.
#'
#' @return A \code{ggplot2} object suitable for academic publications.
#'
#' @seealso \code{\link{fit_all_models_log}}, \code{\link{plot_all_models}}, \code{\link{plot_raw_memory_decay}}
#' @export
plot_fitted_decay_for_publication <- function(model_output,
                                              observed_col = "CurrentPopularity",
                                              fitted_col = "fitted_correct",
                                              age_var = "age",
                                              group_var = NULL,
                                              group_var2 = NULL,
                                              export_path = NULL,
                                              x_breaks = NULL,
                                              x_limits = NULL,
                                              y_breaks = NULL,
                                              y_limits = NULL,
                                              log_y = FALSE,
                                              log_x = FALSE) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 is required.")
  if (!requireNamespace("ggtext", quietly = TRUE)) stop("ggtext is required.")
  if (!requireNamespace("scales", quietly = TRUE)) stop("scales is required.")


# Detect if input is a list with $fitted or a data.frame directly
if (is.data.frame(model_output)) {
  fitted_df <- model_output
} else if (is.list(model_output) && "fitted" %in% names(model_output)) {
  fitted_df <- model_output$fitted
} else {
  stop("Input must be either a data.frame or a list with a 'fitted' element.")
}


  required_vars <- c(age_var, observed_col, fitted_col)
  missing_vars <- setdiff(required_vars, names(fitted_df))
  if (length(missing_vars) > 0) {
    stop("Missing required columns in fitted data: ", paste(missing_vars, collapse = ", "))
  }

  has_group <- !is.null(group_var) && group_var %in% names(fitted_df)
  has_facet <- !is.null(group_var2) && group_var2 %in% names(fitted_df)

  # Handle grouping variable robustly
  if (has_group) {
    fitted_df[[group_var]] <- as.character(fitted_df[[group_var]])

    # Recode expected Yes/No/missing into factor with friendly labels
    valid_codes <- c("Yes", "No", "missing")
    if (all(fitted_df[[group_var]] %in% valid_codes)) {
      fitted_df[[group_var]] <- factor(fitted_df[[group_var]],
                                       levels = c("Yes", "No", "missing"),
                                       labels = c("Same country", "Different country", "Missing country"))
    } else {
      fitted_df[[group_var]] <- as.factor(fitted_df[[group_var]])
    }

    group_levels <- levels(fitted_df[[group_var]])

    # Custom palette
    default_palette <- c(
      "Different country" = "#E63946",
      "Same country" = "#1D3557",
      "Missing country" = "#999999"
    )
    extra_levels <- setdiff(group_levels, names(default_palette))
    extra_palette <- if (length(extra_levels) > 0) {
      setNames(hue_pal()(length(extra_levels)), extra_levels)
    } else {
      c()
    }
    color_palette <- c(default_palette, extra_palette)
    color_palette <- color_palette[group_levels]
  }

  if (has_facet) {
    fitted_df[[group_var2]] <- as.factor(fitted_df[[group_var2]])
  }

  # Auto-set axis limits and breaks if not provided
  if (is.null(x_limits)) x_limits <- range(fitted_df[[age_var]], na.rm = TRUE)
  if (is.null(x_breaks)) x_breaks <- pretty(x_limits, n = 6)

  if (is.null(y_limits)) {
    min_y <- min(fitted_df[[observed_col]], fitted_df[[fitted_col]], na.rm = TRUE)
    max_y <- max(fitted_df[[observed_col]], fitted_df[[fitted_col]], na.rm = TRUE)
    y_limits <- c(floor(min_y * 10) / 10, ceiling(max_y * 10) / 10)
  }
  if (is.null(y_breaks)) y_breaks <- pretty(y_limits, n = 6)

  # Build the plot
  p <- ggplot(fitted_df, aes_string(x = age_var)) +
    geom_point(aes_string(y = observed_col, color = group_var), alpha = 0.25, size = 2.2) +
    geom_line(aes_string(y = fitted_col, color = group_var), linewidth = 1.3) +
    labs(
      title = "Temporal Decay of Collective Memory",#"Temporal Decay in Historical Knowledge of Cultural Icons",
      # subtitle = "Memory decay across historical age differs based on country match with respondent",
      x = "Age (Years)",
      y = "Collective Attention (S(t))"
    ) +
    scale_x_continuous(breaks = x_breaks, limits = x_limits) +
    scale_y_continuous(breaks = y_breaks, limits = y_limits) +
    theme_minimal(base_family = "ArialMT",base_size = 16) +
    theme(
      plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 14, hjust = 0.5),
      axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 10)),
      axis.title.y = element_text(size = 16, face = "bold", margin = margin(r = 10), angle = 90),
      axis.text = element_text(size = 13),
      legend.position = "right",
      legend.title = element_blank(),
      legend.text = element_text(size = 13),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(size = 0.3),
      plot.margin = margin(10, 10, 10, 10)
    )

  if (has_group) {
    p <- p + scale_color_manual(values = color_palette)
  }

     # Axis transforms
  if (log_x) {
    p <- p + scale_x_log10()
  } else {
    p <- p + scale_x_continuous(breaks = x_breaks, limits = x_limits)
  }

  if (log_y) {
    p <- p + scale_y_log10()
  } else {
    p <- p + scale_y_continuous(breaks = y_breaks, limits = y_limits)
  }


  if (has_facet) {
    p <- p + facet_wrap(as.formula(paste("~", group_var2)), labeller = label_both)
  }

  if (!is.null(export_path)) {
    ggsave(paste0(export_path, ".pdf"), plot = p, width = 10, height = 8, device = cairo_pdf)
    ggsave(paste0(export_path, ".svg"), plot = p, width = 10, height = 8)
  }

  return(p)
}






#' Plot AIC or BIC Comparison Across Forgetting Models
#'
#' Generates a comparative bar chart showing how different forgetting models
#' (biexponential, exponential, log-normal) perform based on the selected
#' information criterion (AIC or BIC).
#'
#' This function is typically used after fitting all models with
#' \code{\link{fit_all_models_log}}, and helps visually identify the model
#' with the best fit for each group or condition.
#'
#' @param model_comparison A data frame returned by \code{fit_all_models_log()} in the element \code{$model_comparison},
#' containing AIC/BIC scores and group identifiers (if applicable).
#' @param metric String. The information criterion to plot: either \code{"AIC"} or \code{"BIC"} (default: "AIC").
#'
#' @return A \code{ggplot2} bar chart comparing model fit quality across groups or levels.
#'
#' @seealso \code{\link{fit_all_models_log}}, \code{\link{plot_all_models}}, \code{\link{plot_fitted_decay_for_publication}}
#' @export
compare_model_fits <- function(model_comparison, metric = "AIC") {
  if (!metric %in% c("AIC", "BIC")) {
    stop("metric must be 'AIC' or 'BIC'")
  }

  ggplot(model_comparison, aes(x = model, y = .data[[metric]], fill = model)) +
    geom_col(position = "dodge", width = 0.7, color = "black", alpha = 0.8) +
    facet_wrap(~group, scales = "free_y") +
    labs(
      title = paste("Model comparison using", metric),
      x = "Model",
      y = metric
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      legend.position = "none",
      axis.title = element_text(face = "bold")
    )
}





#' Plot All Fitted Forgetting Curves (Biexponential, Exponential, Log-normal)
#'
#' This function overlays the fitted forgetting curves from three competing models:
#' \itemize{
#'   \item \strong{Biexponential}: captures short- and long-term decay via two interacting memory systems.
#'   \item \strong{Exponential}: simple continuous decay process.
#'   \item \strong{Log-normal modulated power-law}: captures early attention rise followed by long-tail decay.
#' }
#'
#' It compares each model's predictions against observed attention or recall values.
#' The function supports grouping (color/style by category) and faceting for stratified comparison
#' across populations or experimental conditions.
#'
#' @param model_outputs A list returned by \code{\link{fit_all_models_log}}, containing fitted values for each model
#' and a merged data frame in \code{$fitted}.
#' @param age_var Name of the column indicating age or time since the event (default: "age").
#' @param observed_col Name of the observed attention or recall variable (default: "CurrentPopularity").
#' @param group_var Optional. Primary grouping variable for line color and point shape (e.g., "same_country", "decay_bin").
#' @param group_var2 Optional. Secondary grouping variable used for faceting (e.g., attention level or demographic stratum).
#' @param log_y Logical. If \code{TRUE}, apply log10 transformation to the Y axis. Default is \code{TRUE}.
#' @param log_x Logical. If \code{TRUE}, apply log10 transformation to the X axis. Default is \code{FALSE}.
#'
#' @return A \code{ggplot2} object with layered curves and observed values, styled and faceted according to grouping variables.
#'
#' @seealso \code{\link{fit_biexponential_model}}, \code{\link{fit_lognormal_log_model}},
#' \code{\link{fit_exponential_log_model}}, \code{\link{compare_model_fits}}
#' @export
plot_all_models <- function(model_outputs,
                            age_var = "age",
                            observed_col = "CurrentPopularity",
                            group_var = NULL,
                            group_var2 = NULL,
                            log_y = TRUE,
                            log_x = FALSE) {

  if (!all(c("biexponential", "exponential", "lognormal") %in% names(model_outputs))) {
    stop("model_outputs must include 'biexponential', 'exponential', and 'lognormal'")
  }

  # Combine and tag model
  tag_model <- function(df, model_name) {
    df$model <- model_name
    return(df)
  }

  all_fits <- dplyr::bind_rows(
    tag_model(model_outputs$biexponential$fitted, "biexponential"),
    tag_model(model_outputs$exponential$fitted, "exponential"),
    tag_model(model_outputs$lognormal$fitted, "lognormal")
  )

  required_cols <- c(age_var, observed_col, "fitted_correct")
  missing_cols <- setdiff(required_cols, names(all_fits))
  if (length(missing_cols) > 0) {
    stop("Missing required column(s): ", paste(missing_cols, collapse = ", "))
  }

  # Prepare grouping if exists
  if (!is.null(group_var) && group_var %in% names(all_fits)) {
    all_fits <- all_fits |>
      dplyr::mutate(
        !!group_var := as.factor(.data[[group_var]]),
        .grouping = interaction(model, .data[[group_var]])
      )
    point_aes <- aes(y = .data[[observed_col]], shape = .data[[group_var]])
    line_aes  <- aes(y = fitted_correct, color = model, linetype = .data[[group_var]], group = .grouping)
    guides_list <- guides(
      color = guide_legend(order = 1),
      shape = guide_legend(order = 2, override.aes = list(size = 4, alpha = 1)),
      linetype = guide_legend(order = 2)
    )
  } else {
    all_fits <- all_fits |>
      dplyr::mutate(.grouping = model)
    point_aes <- aes(y = .data[[observed_col]])
    line_aes  <- aes(y = fitted_correct, color = model, group = .grouping)
    guides_list <- guides(color = guide_legend(order = 1))
  }

  # Plot
  p <- ggplot(all_fits, aes(x = .data[[age_var]])) +
    geom_point(point_aes,
               alpha = 0.3, size = 2.5, color = "gray50") +
    geom_line(line_aes,
              size = 1.3, alpha = 0.85) +
    scale_color_manual(values = c(
      "biexponential" = "#C2185B",
      "exponential" = "#1976D2",
      "lognormal" = "#388E3C"
    )) +
    labs(
      title = "Forgetting Curve Fits by Model",
      subtitle = "Fitted vs Observed Popularity over Age",
      x = "Age (Years)",
      y = "Popularity (Observed and Fitted)",
      caption = "Models: Biexponential, Exponential, Log-normal"
    ) +
    theme_minimal(base_size = 20) +
    theme(
      legend.position = "top",
      legend.title = element_blank(),
      plot.title = element_text(face = "bold", size = 18),
      plot.subtitle = element_text(size = 15, color = "gray30"),
      panel.grid.minor = element_blank()
    ) +
    guides_list

  if (log_y) p <- p + scale_y_log10()
  if (log_x) p <- p + scale_x_log10()

  if (!is.null(group_var2) && group_var2 %in% names(all_fits)) {
    p <- p + facet_wrap(as.formula(paste("~", group_var2)), scales = "free_y")
  }

  return(p)
}
