#' Normalize a String for Consistent Text Matching
#'
#' This utility function standardizes character strings by applying the following transformations:
#' \itemize{
#'   \item Converts all characters to lowercase.
#'   \item Removes accents and diacritics.
#'   \item Trims leading and trailing whitespace.
#'   \item Removes all non-alphanumeric characters (except underscores).
#' }
#'
#' It is useful for text preprocessing tasks such as:
#' \itemize{
#'   \item Comparing entity names or labels across datasets,
#'   \item Deduplicating responses in surveys,
#'   \item Cleaning category labels before grouping or joining.
#' }
#'
#' @param x A character vector to normalize.
#'
#' @return A character vector of normalized strings.
#'
#' @examples
#' normalize_string(c(" García ", "garcia", "GARCÍA!", "Garcia_1"))
#' # Returns: "garcia", "garcia", "garcia", "garcia_1"
#'
#' @importFrom magrittr %>%
#' @importFrom stringr str_to_lower str_trim str_replace_all
#' @importFrom stringi stri_trans_general
#' @export
normalize_string <- function(x) {
  x %>%
    stringr::str_to_lower() %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    stringr::str_trim() %>%
    stringr::str_replace_all("[^a-z0-9]", "")
}





#' Process and Clean Survey-Based Memory Data for Decay Curve Modeling
#'
#' This function prepares noisy memory or recall survey data for forgetting curve analysis. 
#' It includes filtering, outlier removal, and optional grouping or stratification steps. 
#' It is tailored for use in cross-sectional or recall-based studies (e.g., cultural memory, icon recall).
#'
#' Specifically, the function:
#' \itemize{
#'   \item Filters out IDs with too few responses (e.g., less than 3).
#'   \item Removes outliers in accumulated attention using a top percentile threshold.
#'   \item Optionally classifies items into quantiles based on attention/popularity (e.g., deciles).
#'   \item Keeps grouping variables for later stratified model fitting.
#' }
#'
#' This is often a first preprocessing step before applying forgetting models such as
#' \code{\link{fit_biexponential_model}}, \code{\link{fit_lognormal_log_model}}, or 
#' \code{\link{fit_exponential_log_model}}.
#'
#' @param data A data frame with raw survey-based memory or attention responses.
#' @param id_col Name of the unique identifier column (e.g., "slug", "person_id"). Default: \code{"slug"}.
#' @param age_var Name of the age or time-since-event variable. Default: \code{"age_metric"}.
#' @param group_var Optional. First grouping variable for stratified modeling (e.g., "same_country").
#' @param replies_col Optional. Column with number of responses per item (e.g., "n_replies") used for filtering.
#' @param group_var2 Optional. Column representing accumulated attention (e.g., "global") used for outlier removal and quantile binning.
#' @param quantile Integer. Number of quantile bins to create from \code{group_var2} (default: 1 = no bins).
#' @param percentile Numeric [0, 1]. Threshold to remove top outliers in \code{group_var2} (default: 0.99).
#' @param filter_n Minimum number of replies required to keep an item (default: 3).
#'
#' @return A cleaned and annotated data frame ready for forgetting curve modeling. Columns may include:
#' \itemize{
#'   \item Filtered items with sufficient responses.
#'   \item Age variable standardized to \code{age_metric}.
#'   \item Optional columns for group, attention level, and outlier-stripped popularity.
#' }
#'
#' @importFrom dplyr rename ungroup group_by filter mutate select ntile
#' @importFrom rlang sym
#' @importFrom stats quantile
#' 
#' @examples
#' # Load and preprocess a dataset
#' data(survey_data)
#' cleaned <- process_data(
#'   survey_data,
#'   id_col = "entity_id",
#'   age_var = "age_metric",
#'   replies_col = "reply_count",
#'   group_var = "location_flag",
#'   group_var2 = "performance_score",
#'   quantile = 3
#' )
#'
#' @export
process_data <- function(data,
                         id_col = "slug",
                         age_var = "age_metric",
                         group_var = NULL,
                         replies_col = NULL,
                         group_var2 = NULL,
                         quantile = 1,
                         percentile = 0.99,
                         filter_n = 3) {

  # --- Check that required columns exist ---
  required_cols <- c(id_col, age_var)
  missing_required <- setdiff(required_cols, names(data))
  if (length(missing_required) > 0) {
    stop("Missing required column(s): ", paste(missing_required, collapse = ", "))
  }

  # --- Internal alias for age variable ---
  age_internal <- "age_metric"
  data <- data %>%
    dplyr::rename(!!age_internal := !!rlang::sym(age_var))

  # --- Detect presence of optional columns ---
  has_group <- !is.null(group_var) && group_var %in% names(data)
  has_replies <- !is.null(replies_col) && replies_col %in% names(data)
  has_attention <- !is.null(group_var2) && group_var2 %in% names(data)

  # --- Store original number of rows for later percentage ---
  original_n <- nrow(data)

  # --- Start from ungrouped version of the input ---
  df <- data %>% dplyr::ungroup()

  # --- Step 1: Filter out IDs with too few replies (if replies_col is present) ---
  if (has_replies) {
    if (has_group) {
      df <- df %>%
        dplyr::group_by(.data[[id_col]], .data[[group_var]]) %>%
        dplyr::filter(all(.data[[replies_col]] >= filter_n)) %>%
        dplyr::ungroup()
    } else {
      df <- df %>%
        dplyr::group_by(.data[[id_col]]) %>%
        dplyr::filter(all(.data[[replies_col]] >= filter_n)) %>%
        dplyr::ungroup()
    }
  }

  # --- Step 2: Handle accumulated attention (if provided) ---
  if (has_attention) {

    # 2a. Optional grouping before assigning quantile levels
    if (has_group) {
      df <- df %>% dplyr::group_by(.data[[group_var]])
    }

    # 2b. Assign quantile-based levels only if quantile > 1
    df <- df %>%
      dplyr::mutate(
        accumulated_advantage = .data[[group_var2]],
        accumulated_advantage_level = if (quantile > 1) {
          factor(dplyr::ntile(accumulated_advantage, quantile))
        } else {
          NA
        }
      ) %>%
      dplyr::ungroup()

    # 2c. Filter top percentile of attention
    if (has_group) {
      df <- df %>%
        dplyr::group_by(.data[[age_internal]], .data[[group_var]])
    } else {
      df <- df %>%
        dplyr::group_by(.data[[age_internal]])
    }

    df <- df %>%
      dplyr::mutate(
        attention_threshold = quantile(accumulated_advantage, percentile, na.rm = TRUE)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::filter(accumulated_advantage <= attention_threshold) %>%
      dplyr::select(-attention_threshold)
  }

  # --- Final message ---
  final_n <- nrow(df)
  perc <- round(100 * final_n / original_n, 2)

  message("------------------------------------")
  message("Number of Observations: ", final_n)
  message("Data retained: ", perc, "% of original (", original_n, " rows)")
  message("------------------------------------")

  return(df)
}





#' Smooth and Aggregate Noisy Survey-Based Memory Decay Data
#'
#' This function is designed to smooth and aggregate memory or attention decay data 
#' collected from cross-sectional **survey instruments**. It is particularly suited 
#' for analyzing recall accuracy or attention scores over time (e.g., the age of 
#' historical or cultural figures).
#'
#' It computes:
#' \itemize{
#'   \item \strong{Mean response} per age and group
#'   \item \strong{LOESS-smoothed} trend using local polynomial regression
#'   \item \strong{GAM-smoothed} trend using penalized cubic splines
#' }
#'
#' The function supports one or two grouping variables:
#' \itemize{
#'   \item \code{group_var}: Primary grouping (e.g., same vs. different country)
#'   \item \code{group_var2}: Secondary grouping (e.g., attention decile, demographic strata)
#' }
#' These allow stratified smoothing and facilitate visual comparisons across subpopulations.
#'
#' @param data A data frame with survey-based memory or attention data.
#' @param age_var String. Name of the age variable (e.g., "age_metric"). Must be numeric.
#' @param response_var String. Name of the response variable (e.g., "performance_score").
#' @param group_var (Optional) First grouping variable (default: NULL).
#' @param group_var2 (Optional) Second grouping variable for faceting or stratification (default: NULL).
#' @param filter_age Numeric. Maximum age to retain (default: Inf = include all).
#'
#' @return A data frame with the following columns:
#' \describe{
#'   \item{\code{age_metric}}{Age variable in numeric form.}
#'   \item{\code{response}}{Raw response values (copied from \code{response_var}).}
#'   \item{\code{mean_response}}{Mean response for each age/group combination.}
#'   \item{\code{loess_correct}}{LOESS-smoothed response over age.}
#'   \item{\code{gam_correct}}{GAM-smoothed response over age.}
#'   \item{\code{group_var, group_var2}}{Preserved grouping columns (if provided).}
#' }
#'
#' @examples
#' data(survey_data)
#' smooth_df <- smooth_survey_decay(
#'   data = survey_data,
#'   age_var = "age_metric",
#'   response_var = "performance_score",
#'   group_var = "location_flag"
#' )
#'
#' @seealso \code{\link{plot_raw_memory_decay}}, \code{\link{fit_biexponential_model}}
#' @export
smooth_survey_decay <- function(data,
                                  age_var,
                                  response_var,
                                  group_var = NULL,
                                  group_var2 = NULL,
                                  filter_age = Inf) {
  # ---------------------------------------
  # Check required columns
  # ---------------------------------------
  required_cols <- c(age_var, response_var)
  missing <- setdiff(required_cols, names(data))
  if (length(missing) > 0) {
    stop("Missing required column(s): ", paste(missing, collapse = ", "))
  }

  # ---------------------------------------
  # Prepare dataframe and handle grouping vars
  # ---------------------------------------
  df <- data

  # Rename required variables for internal processing
  df <- df %>%
    dplyr::rename(
      age_metric = !!rlang::sym(age_var),
      response = !!rlang::sym(response_var)
    )

  has_group <- !is.null(group_var) && group_var %in% names(df)
  has_level <- !is.null(group_var2) && group_var2 %in% names(df)

  if (has_group) {
    df <- df %>% dplyr::rename(group_temp = !!rlang::sym(group_var))
  } else {
    df <- df %>% dplyr::mutate(group_temp = "all")
  }

  if (has_level) {
    df <- df %>% dplyr::rename(level_temp = !!rlang::sym(group_var2))
  } else {
    df <- df %>% dplyr::mutate(level_temp = "all")
  }

  # ---------------------------------------
  # Compute LOESS smoothing on mean response by age
  # ---------------------------------------
  result <- df %>%
    dplyr::group_by(group_temp, level_temp, age_metric) %>%
    dplyr::mutate(mean_response = mean(response, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(group_temp, level_temp) %>%
    dplyr::group_modify(~ {
      subdata <- .x
      # LOESS
      loess_fit <- loess(response ~ age_metric, 
                         data = subdata, 
                         degree = 2, 
                        #  span=0.75,
                         family = "symmetric", 
                         na.action = na.exclude)

      loess_pred <- predict(loess_fit, newdata = subdata)
        

      # GAM (penalized cubic spline)
      gam_fit <- mgcv::gam(response ~ s(age_metric, bs = "cs",k=15), 
                           data = subdata, 
                           method = "REML")
      
      gam_pred <- predict(gam_fit, newdata = subdata)

      dplyr::tibble(
        age_metric = subdata$age_metric,
        response=subdata$response,
        mean_response = subdata$mean_response,
        loess_correct = loess_pred,
        gam_correct = gam_pred
      )
    }) %>%
    dplyr::ungroup() %>%
    dplyr::filter(age_metric <= filter_age)

  # ---------------------------------------
  # Restore original grouping var names
  # ---------------------------------------
  if (has_group) {
    result <- result %>% dplyr::rename(!!group_var := group_temp)
  } else {
    result <- result %>% dplyr::select(-group_temp)
  }

  if (has_level) {
    result <- result %>% dplyr::rename(!!group_var2 := level_temp)
  } else {
    result <- result %>% dplyr::select(-level_temp)
  }

  # ---------------------------------------
  # User message
  # ---------------------------------------
  message("Aggregated ", nrow(result), " observations using LOESS smoothing.")
  if (!has_group && !has_level) {
    message("No grouping or attention level variable detected. Aggregated over all data.")
  }

  return(result)
}




##### FOR TIME SERIES DATA #########

#' Reshape Citation Time Series from Long to Wide Format
#'
#' This function transforms a long-format citation dataset into wide format,
#' where each row represents a time point (e.g., year or semester) and each column
#' corresponds to a unique item identifier (typically a paper DOI).
#'
#' The resulting matrix is suitable for computing cumulative attention, decay curves,
#' and memory stratification. Each cell in the matrix represents the number of citations 
#' (or another attention metric) that a specific item received at a given time point.
#' If a citation is missing for a given time-item pair, the corresponding cell will contain NA.
#'
#' This reshaping is typically used as the first step in the `MemoryDecay` pipeline for
#' time-series data, prior to applying \code{\link{compute_cumulative_matrix}} and
#' \code{\link{assign_decay_bins}}.
#'
#' @param data A data frame in long format. Must include:
#' \describe{
#'   \item{\code{time}}{Numeric variable indicating the time point (e.g., year or semester).}
#'   \item{\code{doi}}{Unique identifier for each paper or entity (e.g., "10.1103/PhysRevLett.1.1").}
#'   \item{\code{value}}{Citation count or attention metric at each time point.}
#' }
#'
#' @return A wide-format data frame where:
#' \itemize{
#'   \item Each row is a unique time point.
#'   \item Each column is a paper/item ID prefixed with \code{value.}, containing the attention received at that time.
#'   \item The first column is \code{time}, ordered in ascending order.
#' }
#'
#' @examples
#' # Sample long-format data
#' citation_data <- data.frame(
#'   time = rep(1980:1982, each = 2),
#'   doi = rep(c("A", "B"), 3),
#'   value = c(3, 5, 4, 6, 2, 1)
#' )
#'
#' wide_df <- reshape_citation_timeseries(citation_data)
#' head(wide_df)
#'
#' @seealso \code{\link{compute_cumulative_matrix}}, \code{\link{assign_decay_bins}}, \code{\link{merge_bins_with_original}}
#' @export
reshape_citation_timeseries <- function(data) {
  if (!all(c("time", "doi") %in% names(data))) {
    stop("Input data must contain 'time' and 'doi' columns.")
  }
  wide_data <- reshape(data, idvar = 'time', timevar = 'doi', direction = 'wide')
  wide_data <- wide_data[order(wide_data$time), ]
  rownames(wide_data) <- seq_len(nrow(wide_data))
  return(wide_data)
}


#' Compute Cumulative Attention Over Time
#'
#' This function calculates the cumulative attention (e.g., citations) received by each item 
#' over time. It takes as input a wide-format time-series matrix — typically generated 
#' using \code{\link{reshape_citation_timeseries}} — and returns the element-wise cumulative sum 
#' for each item across successive time points.
#'
#' Each column (except \code{time}) represents a unique item (e.g., paper or cultural artifact),
#' and each row corresponds to a discrete time point (e.g., year or semester).
#' Missing values (NAs) are replaced with zeros before computing cumulative sums, assuming
#' that missing data corresponds to zero observed attention at that time.
#'
#' This step is essential before assigning decay bins using \code{\link{assign_decay_bins}}.
#'
#' @param wide_data A data frame in wide format, with:
#' \describe{
#'   \item{\code{time}}{A numeric column representing the temporal axis (e.g., year or semester).}
#'   \item{Other columns}{Named after unique item identifiers (e.g., DOIs) containing the attention values (e.g., citation counts).}
#' }
#'
#' @return A data frame with the same structure (excluding \code{time}) where each cell contains 
#' the cumulative value of attention received by an item up to that time point.
#'
#' @examples
#' # Example input matrix
#' wide_df <- data.frame(
#'   time = 1980:1982,
#'   value.A = c(3, 4, 2),
#'   value.B = c(5, 6, 1)
#' )
#'
#' cum_df <- compute_cumulative_matrix(wide_df)
#' head(cum_df)
#'
#' @seealso \code{\link{reshape_citation_timeseries}}, \code{\link{assign_decay_bins}}, \code{\link{merge_bins_with_original}}
#' @export
compute_cumulative_matrix <- function(wide_data) {
  if (!"time" %in% names(wide_data)) {
    stop("Input must include a 'time' column.")
  }
  m <- as.matrix(subset(wide_data, select = -time))
  m[is.na(m)] <- 0
  cumu <- apply(m, 2, cumsum)
  return(as.data.frame(cumu))
}





#' Assign Decay Bins to Cumulative Attention Values (Log-Scaled)
#'
#' Assigns each cumulative attention value (e.g., cumulative citations) to a discrete bin
#' for stratified modeling of memory decay. Bins can be defined either:
#' - Manually using custom breakpoints (`breaks`), or
#' - Automatically using log-scaled quantization (`n_bins`).
#'
#' The input should be a wide-format matrix of cumulative values over time, typically produced
#' by \code{\link{compute_cumulative_matrix}}. Columns represent items (e.g., papers),
#' and rows represent time steps.
#'
#' ## Treatment of Zeros
#'
#' Items with \strong{zero cumulative attention} are excluded from bin breakpoint computation:
#' - Log-scale binning is undefined at zero (\code{log10(0)} = \code{-Inf}).
#' - Including zeros would skew binning and reduce stratification resolution.
#' 
#' In this function, **zero values are intentionally excluded** from the binning computation. 
#' This is because log-scale binning requires strictly positive values and because 
#' zero cumulative attention reflects complete forgetting — which is not comparable 
#' in magnitude to any non-zero value. Therefore, items with zero cumulative attention 
#' are assigned `NA` as their decay bin.
#' These zero entries are retained in the output but marked as \code{NA} to indicate that
#' they are unbinned. This allows separate modeling of completely forgotten items.
#'
#' ## Rationale: Controlling for Preferential Attachment
#'
#' As described by Candia et al. (2019, \emph{Nature Human Behaviour}), binning items
#' by cumulative attention allows researchers to control for preferential attachment—
#' the tendency of popular items to accumulate even more attention.
#' This enables fair comparisons of memory decay across items with similar total attention.
#'
#' @param cumulative_df A data frame of cumulative values (from \code{\link{compute_cumulative_matrix}}).
#' @param breaks Optional numeric vector of manual breakpoints (strictly increasing).
#'               If provided, \code{n_bins} is ignored.
#' @param n_bins Integer. Number of log-spaced bins to compute automatically (default: 3).
#' @param return_labels Logical. If \code{TRUE}, returns bins as factor labels (1 = highest);
#'                      if \code{FALSE}, returns numeric bin index.
#'
#' @return A data frame with the same shape as \code{cumulative_df}, containing bin assignments.
#'         Zero-valued entries are returned as \code{NA}.
#'
#' @examples
#' mat <- data.frame(A = c(0, 2, 10, 100), B = c(0, 1, 5, 50))
#' assign_decay_bins(mat, n_bins = 3)
#'
#' @seealso \code{\link{merge_bins_with_original}}, \code{\link{reshape_citation_timeseries}}, \code{\link{process_time_series_bins}}
#' @export

assign_decay_bins <- function(cumulative_df,
                              breaks = NULL,
                              n_bins = 3,
                              return_labels = TRUE) {
  # Validate input
  if (!is.data.frame(cumulative_df)) stop("Input must be a data frame.")
  mat <- as.matrix(cumulative_df)
  mat[is.na(mat)] <- 0

  # Extract all non-zero values for break calculation
  all_values <- as.vector(mat)
  all_values <- all_values[all_values > 0]

  if (length(all_values) == 0) {
    stop("No non-zero values in cumulative data. Cannot compute bins.")
  }

  # Determine breaks (either user-specified or automatic)
  if (!is.null(breaks)) {
    if (!is.numeric(breaks) || length(breaks) < 2) {
      stop("breaks must be a numeric vector of length >= 2.")
    }
    breaks <- sort(unique(breaks))
  } else {
    # Automatic log-scale binning
    log_min <- log10(min(all_values))
    log_max <- log10(max(all_values))
    breaks <- 10^seq(log_min, log_max, length.out = n_bins + 1)
  }

  # Apply binning to each cell
  bin_mat <- matrix(NA_integer_, nrow = nrow(mat), ncol = ncol(mat))
  for (j in seq_len(ncol(mat))) {
    bin_mat[, j] <- cut(mat[, j],
                        breaks = breaks,
                        include.lowest = TRUE,
                        labels = if (return_labels) seq_len(length(breaks) - 1) else FALSE)
  }

  bin_df <- as.data.frame(bin_mat)
  colnames(bin_df) <- colnames(cumulative_df)

  return(bin_df)
}




#' Merge Decay Bins with Original Time-Series Data
#'
#' This function combines the original wide-format time-series matrix with its corresponding decay bin matrix
#' (e.g., from \code{\link{assign_decay_bins}}), converting both into long format and joining them
#' for downstream modeling or visualization.
#'
#' Each entry in the resulting dataset represents the attention (e.g., citations, views, mentions)
#' received by an item at a specific time, along with its corresponding decay bin. This is especially useful
#' for stratified modeling of attention decay across different levels of cumulative popularity.
#'
#' @section Treatment of Zero Attention:
#'
#' As emphasized in Candia et al. (2019, \emph{Nature Human Behaviour}), attention values of zero are
#' not missing data — they indicate genuine lack of attention or forgetting. Therefore, by default, this function:
#' \itemize{
#'   \item Converts all \code{NA} values in the attention matrix to 0.
#'   \item Ensures that zeroes contribute to aggregated averages (e.g., \eqn{\Delta c(t)}), making them reflective of the true collective state.
#' }
#' This behavior can be disabled by setting \code{replace_na_with_zero = FALSE}.
#'
#' In this function, **NA values are replaced by 0** to represent *genuine lack of attention* 
#' (e.g., zero citations in a given time window). This ensures that zeros are 
#' **included in averages and decay modeling**, as described in Candia et al. (2019). 
#' Including zeros is important for measuring the true dynamics of collective forgetting.
#'
#' @param wide_data A data frame in wide format, with a \code{time} column and one column per entity (e.g., paper or product).
#' @param bins_df A data frame of decay bins with the same number of rows as \code{wide_data} and one column per entity (excluding time).
#' @param replace_na_with_zero Logical. If \code{TRUE} (default), missing values in the attention matrix are replaced with 0.
#'
#' @return A long-format data frame with the following columns:
#' \describe{
#'   \item{time}{Time point (e.g., year or semester).}
#'   \item{doi}{Unique identifier for each entity (e.g., paper ID).}
#'   \item{value}{Observed attention value at that time (e.g., number of citations).}
#'   \item{decay_bin}{Decay bin assigned to that time/entity pair (may be \code{NA} for zero attention).}
#' }
#'
#' @examples
#' # See full pipeline: reshape_citation_timeseries() → compute_cumulative_matrix() → assign_decay_bins()
#' # Then use merge_bins_with_original() to finalize the long-format dataset.
#'
#' @seealso \code{\link{assign_decay_bins}}, \code{\link{process_time_series_bins}}, \code{\link{reshape_citation_timeseries}}
#' @export
merge_bins_with_original <- function(wide_data, bins_df,replace_na_with_zero=TRUE) {
  if (!"time" %in% names(wide_data)) {
    stop("wide_data must include a 'time' column.")
  }

  # Melt the wide data (value matrix) and bins
  reshaped_data <- reshape2::melt(wide_data, id.vars = "time")
  
  # Ensure bins_df has rownames set as time
  if (nrow(bins_df) != length(unique(wide_data$time))) {
    stop("Row count mismatch: 'bins_df' must have same number of rows as time points in 'wide_data'.")
  }
  bins_df$time <- sort(unique(wide_data$time))  # Add time column explicitly
  reshaped_bins <- reshape2::melt(bins_df, id.vars = "time")

  # Merge on time and variable
  merged_df <- merge(
    reshaped_data,
    reshaped_bins,
    by = c("time", "variable"),
    all.x = TRUE
  )

  # Rename for clarity
  colnames(merged_df) <- c("time", "doi", "value", "decay_bin")
  
  # Replace NA with zeroes to reflect "no attention"
  if (replace_na_with_zero) {
    merged_df$value[is.na(merged_df$value)] <- 0
  }


  return(merged_df)
}




#' Process Time-Series Citation Data and Assign Decay Bins
#'
#' This function provides a full pipeline to prepare time-series attention or citation data
#' for decay analysis. It reshapes the data from long to wide format, computes cumulative
#' attention over time, assigns each item to a decay bin using log-scaled thresholds, and
#' merges the result back to a long-format data frame for modeling or visualization.
#'
#' The typical use case involves datasets where each row represents a citation event,
#' with columns such as `time` and `doi`, and the goal is to understand forgetting
#' or attention decay dynamics across items of different cumulative popularity.
#'
#' ## Steps Performed:
#' \enumerate{
#'   \item Reshape long-format citation data into wide format using \code{\link{reshape_citation_timeseries}}.
#'   \item Compute cumulative citation counts over time using \code{\link{compute_cumulative_matrix}}.
#'   \item Assign each cumulative value to a decay bin using \code{\link{assign_decay_bins}}, based on log-scaled binning.
#'   \item Merge decay bin labels back to the original data using \code{\link{merge_bins_with_original}}.
#' }
#'
#' ## Treatment of Zero Values:
#'
#' - When computing decay bins, items with \strong{zero cumulative attention} are excluded from bin computation
#'   because log-scaling is undefined at zero. These entries are assigned \code{NA} in the decay_bin column.
#'
#' - However, when merging bins back to the time-series matrix, \code{\link{merge_bins_with_original}} replaces
#'   missing values (NAs) in the attention matrix with \code{0}, treating them as informative "no-attention" events.
#'   This is consistent with the logic in Candia et al. (2019, \emph{Nature Human Behaviour}).
#'
#' @param data A long-format citation dataset with at least two columns: 
#'             \code{time} (e.g., semester, year) and \code{doi} (unique item ID).
#'
#' @return A long-format data frame with the following columns:
#' \describe{
#'   \item{time}{Time point of the observation (e.g., year or semester).}
#'   \item{doi}{Unique identifier for each paper or entity.}
#'   \item{value}{Number of citations or attention units at that time.}
#'   \item{decay_bin}{Decay bin assigned based on cumulative attention (can be \code{NA} for uncited items).}
#' }
#'
#' @seealso \code{\link{assign_decay_bins}}, \code{\link{merge_bins_with_original}}, \code{\link{plot_raw_memory_decay}}
#' @export
process_time_series_bins <- function(data) {
  wide_data <- reshape_citation_timeseries(data)
  cumu_data <- compute_cumulative_matrix(wide_data)
  bin_data <- assign_decay_bins(cumu_data)
  merged <- merge_bins_with_original(wide_data, bin_data)
  return(merged)
}




#' Aggregate Mean Response by Age and Grouping Variables
#'
#' This function computes the average of a response variable (e.g., recall accuracy, citation count)
#' over time or historical age. It is particularly useful for preparing clean, aggregated data
#' for plotting or model fitting in memory decay studies.
#'
#' You can optionally provide up to two grouping variables to stratify the results:
#' - `group_var`: A primary categorical dimension (e.g., geographic match, decay bin).
#' - `group_var2`: A secondary categorical dimension (e.g., attention level, quantile group).
#'
#' Both grouping variables are preserved in the output using their original column names.
#' If no grouping is provided, the function returns the global mean response at each time point.
#'
#' The output always contains a column named `age_metric` to standardize time/age representation
#' across functions in the package.
#'
#' @param data A data frame containing memory or attention data (e.g., from surveys, citations, or popularity time series).
#' @param age_var Name of the column representing age or time (e.g., years since event or birth).
#' @param response_var Name of the numeric response variable to average (e.g., correct recall rate, citations).
#' @param group_var (Optional) First grouping variable name (e.g., "same_country", "decay_bin").
#' @param group_var2 (Optional) Second grouping variable name (e.g., "attention_level", "demographic").
#' @param filter_age (Optional) Maximum value of age to include in the output (default: \code{Inf} = keep all).
#'
#' @return A data frame with one row per combination of:
#' \describe{
#'   \item{age_metric}{Numeric value representing time or age.}
#'   \item{mean_response}{The average of the response variable within each group/age.}
#'   \item{group_var}{(Optional) Column named as passed by user.}
#'   \item{group_var2}{(Optional) Column named as passed by user.}
#' }
#'
#' @examples
#' # Aggregate recall by age and location
#' aggregated <- aggregate_mean_response(
#'   data = survey_data,
#'   age_var = "age_metric",
#'   response_var = "performance_score",
#'   group_var = "location_flag"
#' )
#'
#' @export
aggregate_mean_response <- function(data,
                                    age_var,
                                    response_var,
                                    group_var = NULL,
                                    group_var2 = NULL,
                                    filter_age = Inf) {

  # Check for required columns
  required_cols <- c(age_var, response_var)
  missing <- setdiff(required_cols, names(data))
  if (length(missing) > 0) {
    stop("Missing required columns: ", paste(missing, collapse = ", "))
  }

  # Rename columns for internal processing
  df <- data
  df$age_metric <- df[[age_var]]
  df$response <- df[[response_var]]
  df$group1 <- if (!is.null(group_var)) df[[group_var]] else "all"
  df$group2 <- if (!is.null(group_var2)) df[[group_var2]] else "all"

  # Filter by age
  df <- df[df$age_metric <= filter_age, ]

  # Aggregate mean response
  result <- stats::aggregate(response ~ age_metric + group1 + group2,
                             data = df,
                             FUN = mean,
                             na.rm = TRUE)

  # Rename response column
  names(result)[names(result) == "response"] <- "mean_response"

  # Restore original group_var names
  if (!is.null(group_var)) {
    names(result)[names(result) == "group1"] <- group_var
  } else {
    result$group1 <- NULL
  }

  if (!is.null(group_var2)) {
    names(result)[names(result) == "group2"] <- group_var2
  } else {
    result$group2 <- NULL
  }

  message("Aggregated ", nrow(result), " rows with mean response.")
  return(result)
}
