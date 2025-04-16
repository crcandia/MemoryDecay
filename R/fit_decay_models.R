#' Fit biexponential model to memory data (grouped or not)
#'
#' This function fits a biexponential forgetting curve to memory-related data,
#' optionally by group and/or attention level. It allows the user to specify a
#' reference value for \eqn{N} and tries a wide range of parameter initializations to
#' improve model convergence.
#'
#' ## Biexponential Decay Model:
#'
#' The model estimated (on the log scale) is:
#' \deqn{
#' \log S(t) = \log \left[ N \left( e^{-(p+r)t} + \frac{r}{p + r - q} \left( e^{-qt} - e^{-(p+r)t} \right) \right) \right]
#' }
#'
#' where:
#' \itemize{
#'   \item \eqn{S(t)}: attention/popularity at time \eqn{t}
#'   \item \eqn{N}: initial popularity level
#'   \item \eqn{p}: communicative decay rate
#'   \item \eqn{r}: communicative-to-cultural transfer rate
#'   \item \eqn{q}: cultural decay rate
#' }
#'
#' ## Why use the log-transformed model?
#'
#' Working in the log scale provides several practical and statistical advantages:
#' \itemize{
#'   \item Stabilizes the variance across time, improving homoscedasticity.
#'   \item Enhances numerical stability during optimization.
#'   \item Prevents invalid predictions (e.g., negative popularity).
#'   \item Aligns with likelihood-based estimation for proportion/attention data.
#' }
#'
#' ## Critical Time:
#'
#' The *critical time* \eqn{t_c} is the point at which communicative and cultural memory contributions are equal:
#' \deqn{
#' t_c = \frac{1}{p + r - q} \log \left( \frac{(p + r)(p - q)}{rq} \right)
#' }
#'
#' The function also estimates the standard error of \eqn{t_c} using the delta method,
#' propagating uncertainty via the gradient and the parameter variance-covariance matrix.
#'
#' ## Input Requirements:
#'
#' \itemize{
#'   \item `age_var`: numeric column indicating the age (e.g., time since event)
#'   \item `observed_col`: numeric column with observed popularity or attention values
#'   \item `group_var` (optional): grouping variable for stratified fitting
#'   \item `group_var2` (optional): secondary grouping variable (e.g., attention level)
#'   \item `accumulated_attention` (optional): used to estimate critical time error
#' }
#'
#' @param data A data frame containing memory data.
#' @param age_var Name of the age column (default: "age").
#' @param observed_col Name of the observed popularity/attention column (default: "CurrentPopularity").
#' @param group_var (Optional) Name of a grouping column (e.g., same_country).
#' @param group_var2 (Optional) Name of attention-level group column (e.g., accumulated_advantage_level).
#' @param N_ref Optional value for N to use in all starting points (default: NULL).
#' @param weight_early_points Logical. Whether to give more weight to early time points (default: FALSE).
#'
#' @return A list with:
#' \describe{
#'   \item{fitted}{A data frame with fitted values and predicted popularity.}
#'   \item{params}{A data frame of estimated parameters:
#'      \eqn{N}, \eqn{p}, \eqn{r}, \eqn{q}, AIC, BIC, R², \eqn{t_c}, and its standard error.}
#' }
#'
#' @aliases fit_biexponential_model_agg
#' @seealso \code{\link{plot_fitted_decay}}, \code{\link{fit_all_models_log}}
#' @export
fit_biexponential_model <- function(data,
                                    age_var = "age",
                                    observed_col = "CurrentPopularity",
                                    group_var = NULL,
                                    group_var2 = NULL,
                                    N_ref = NULL,
                                    weight_early_points = FALSE) {

  required_cols <- c(age_var, observed_col)
  missing <- setdiff(required_cols, names(data))
  if (length(missing) > 0) {
    stop("Missing required column(s): ", paste(missing, collapse = ", "))
  }

  if (!is.null(group_var2) && !(group_var2 %in% names(data))) {
    stop("The group_var2 column was specified but not found in the dataset.")
  }

  biexponential_model <- function(t, N, p, r, q) {
    eps <- 1e-6
    p <- max(p, eps)
    r <- max(r, eps)
    q <- max(q, eps)
    log(N * (exp(-(p + r) * t) + (r / (p + r - q + eps)) * (exp(-q * t) - exp(-(p + r) * t))))
  }

compute_critical_time_and_se <- function(params, vcov_mat) {
  p <- as.numeric(params["p"])
  r <- as.numeric(params["r"])
  q <- as.numeric(params["q"])

  if (any(is.na(c(p, r, q))) || !all(is.finite(c(p, r, q)))) {
    return(list(critical_time = NA, critical_time_se = NA))
  }

  t_c <- (1 / (p + r - q)) * log(((p + r) * (p - q)) / (r * q))

  dp <- (log(((p + r) * (p - q)) / (r * q)) / (p + r - q)) -
        ((1 / (p + r - q)^2) * log(((p + r) * (p - q)) / (r * q)))
  dr <- (log(((p + r) * (p - q)) / (r * q)) / (p + r - q)) -
        ((1 / (p + r - q)^2) * log(((p + r) * (p - q)) / (r * q))) - (1 / (p + r))
  dq <- (1 / q) -
        (log(((p + r) * (p - q)) / (r * q)) / (p + r - q)) -
        ((1 / (p + r - q)^2) * log(((p + r) * (p - q)) / (r * q))) -
        (1 / (p - q))

  grad <- c(dp, dr, dq)
  se <- tryCatch({
    sqrt(as.numeric(t(grad) %*% vcov_mat[c("p", "r", "q"), c("p", "r", "q")] %*% grad))
  }, error = function(e) NA)

  list(critical_time = t_c, critical_time_se = se)
}


  starting_params_list <- list(
    list(p = 0.4, r = 0.15, q = 0.01),
    list(p = 0.3, r = 0.07, q = 0.01),
    list(p = 0.01, r = 0.015, q = 0.0001),
    list(p = 0.015, r = 0.02, q = 0.0002),
    list(p = 0.02, r = 0.03, q = 0.0003),
    list(p = 0.025, r = 0.04, q = 0.0005),
    list(p = 0.03, r = 0.05, q = 0.0008),
    list(p = 0.005, r = 0.00001, q = 0.00005),
    list(p = 0.007, r = 0.012, q = 0.00008),
    list(p = 0.009, r = 0.008, q = 0.000006)
  )

  fit_model_nlsLM <- function(df) {

    weights <- if (weight_early_points) {
        1 / (df[[age_var]] + 1e-3)
      } else {
        rep(1, nrow(df))
      }

    N_values <- if (!is.null(N_ref)) c(N_ref) else c(100, 50, 20, 15, 10,5, 4,3,2,1, 0.8, 1.0, 1.2, 1.5,0.5)

    for (N in N_values) {
      for (params in starting_params_list) {
        model <- tryCatch({
          minpack.lm::nlsLM(
            stats::as.formula(paste("log(", observed_col, ") ~ biexponential_model(", age_var, ", N, p, r, q)")),
            data = df,
            start = c(N = N, params),
            weights = weights,
            control = minpack.lm::nls.lm.control(maxiter = 1000, ftol = 1e-1)
          )
        }, error = function(e) NULL)
        if (!is.null(model)) return(model)
      }
    }
    return(NULL)
  }

  if (!is.null(group_var2)) {
    level_groups <- data %>% dplyr::group_by(.data[[group_var2]]) %>% dplyr::group_split()
  } else {
    level_groups <- list(data)
  }

  results_all <- purrr::map(level_groups, function(level_df) {
    facet_level <- if (!is.null(group_var2)) unique(level_df[[group_var2]]) else NULL

    if (!is.null(group_var) && group_var %in% names(level_df)) {
      group_results <- level_df %>%
        dplyr::group_by(.data[[group_var]]) %>%
        dplyr::group_split() %>%
        purrr::map(function(df_group) {
          model <- fit_model_nlsLM(df_group)
          if (!is.null(model)) {
            params <- as.list(coef(model))
            fitted_vals <- exp(predict(model, newdata = df_group))
            residuals <- df_group[[observed_col]] - fitted_vals
            r2 <- 1 - (sum(residuals^2) / sum((df_group[[observed_col]] - mean(df_group[[observed_col]]))^2))
            crit <- compute_critical_time_and_se(params, vcov(model))
            df_group$fitted_correct <- fitted_vals
            param_df <- tibble::tibble(
              # group = unique(df_group[[group_var]]),
              !!group_var := unique(df_group[[group_var]]),
              N = params$N, p = params$p, r = params$r, q = params$q,
              AIC = AIC(model), BIC = BIC(model), R2 = r2,
              critical_time = crit$critical_time,
              critical_time_se = crit$critical_time_se
            )
          } else {
            df_group$fitted_correct <- NA
            param_df <- tibble::tibble(
              # group = unique(df_group[[group_var]]),
              !!group_var := unique(df_group[[group_var]]),
              N = NA, p = NA, r = NA, q = NA,
              AIC = NA, BIC = NA, R2 = NA,
              critical_time = NA,
              critical_time_se = NA
            )
          }
          if (!is.null(facet_level)) {
            df_group[[group_var2]] <- facet_level
            param_df[[group_var2]] <- facet_level
          }
          list(fitted = df_group, parameters = param_df)
        })

      list(
        fitted = purrr::list_rbind(purrr::map(group_results, "fitted")),
        parameters = purrr::list_rbind(purrr::map(group_results, "parameters"))
      )
    } else {
      model <- fit_model_nlsLM(level_df)
      if (!is.null(model)) {
        params <- as.list(coef(model))
        fitted_vals <- exp(predict(model, newdata = level_df))
        residuals <- level_df[[observed_col]] - fitted_vals
        r2 <- 1 - (sum(residuals^2) / sum((level_df[[observed_col]] - mean(level_df[[observed_col]]))^2))
        crit <- compute_critical_time_and_se(params, vcov(model))
        level_df$fitted_correct <- fitted_vals
        param_df <- tibble::tibble(
          group = "all",
          N = params$N, p = params$p, r = params$r, q = params$q,
          AIC = AIC(model), BIC = BIC(model), R2 = r2,
          critical_time = crit$critical_time,
          critical_time_se = crit$critical_time_se
        )
      } else {
        level_df$fitted_correct <- NA
        param_df <- tibble::tibble(
          group = "all",
          N = NA, p = NA, r = NA, q = NA,
          AIC = NA, BIC = NA, R2 = NA,
          critical_time = NA,
          critical_time_se = NA
        )
        warning("Model did not converge. Try adjusting starting parameters or weights.")
      }
      if (!is.null(facet_level)) {
        level_df[[group_var2]] <- facet_level
        param_df[[group_var2]] <- facet_level
      }
      list(fitted = level_df, parameters = param_df)
    }
  })

  fitted_data <- purrr::list_rbind(purrr::map(results_all, "fitted"))
  parameters <- purrr::list_rbind(purrr::map(results_all, "parameters"))

  message("Fitted Bi-exponential model for ", 
          sum(!is.na(parameters$N)), " out of ", nrow(parameters), " group(s).")

  return(list(fitted = fitted_data, params = parameters))
}





#' Add critical time to model parameters from biexponential fit
#'
#' Computes the critical time \eqn{t_c} from the parameters \eqn{p}, \eqn{r}, and \eqn{q}
#' obtained via the biexponential model. This is the point in time at which the contributions
#' of communicative and cultural memory become equal.
#'
#' The formula used is:
#' \deqn{
#' t_c = \frac{1}{p + r - q} \log \left( \frac{(p + r)(p - q)}{rq} \right)
#' }
#'
#' This function is designed to work with the `params` output of \code{\link{fit_biexponential_model}}.
#'
#' @param parameters A data frame containing columns \code{p}, \code{r}, and \code{q}, 
#' typically returned by \code{fit_biexponential_model()}.
#'
#' @return A data frame with a new column:
#' \describe{
#'   \item{critical_time}{The estimated time \eqn{t_c} at which cultural memory begins to dominate.}
#' }
#'
#' @seealso \code{\link{fit_biexponential_model}}
#' @export
add_critical_time <- function(parameters) {
  parameters %>%
    dplyr::mutate(
      critical_time = ifelse(
        is.na(p) | is.na(r) | is.na(q),
        NA,
        (1 / (p + r - q)) * log(((p + r) * (p - q)) / (r * q))
      )
    )
}


#' Fit Exponential Decay Model (log-transformed)
#'
#' Fits a log-transformed exponential decay model to collective memory or attention data.
#' The model assumes attention decays continuously over time without a peak.
#'
#' @section Exponential Decay Model:
#'
#' The model is expressed in log-transformed form as:
#' \deqn{
#' \log S(t) = \log c - q \cdot t
#' }
#' which is equivalent to:
#' \deqn{
#' S(t) = c \cdot e^{-qt}
#' }
#'
#' where:
#' \itemize{
#'   \item \eqn{S(t)} is the observed popularity or recall at time \eqn{t},
#'   \item \eqn{c} is the initial attention (scaling factor),
#'   \item \eqn{q} is the exponential decay rate.
#' }
#'
#' Fitting in log-space has several advantages:
#' \itemize{
#'   \item It linearizes the decay process for stable nonlinear least squares (NLS) estimation.
#'   \item Reduces the influence of high-value outliers.
#'   \item Emphasizes early decay, typical in forgetting processes.
#'   \item Improves variance stability in heteroscedastic data.
#' }
#'
#' This model assumes **monotonic decay**, and may not be appropriate if the data exhibit
#' a peak or burst pattern followed by decline.
#'
#' @param data A data frame with attention or recall values.
#' @param age_var Name of the column representing age (e.g., time since birth or publication).
#' @param observed_col Name of the observed response variable (e.g., "loess_correct").
#' @param group_var Optional: primary grouping column (e.g., "same_country").
#' @param group_var2 Optional: secondary grouping column (e.g., "attention_level").
#' @param weight_early_points Logical. Currently ignored in this model. Included for API consistency.
#'
#' @return A list containing:
#' \describe{
#'   \item{fitted}{A data frame including predicted values.}
#'   \item{params}{Parameter estimates \eqn{c} and \eqn{q}, along with AIC, BIC, and pseudo-R².}
#' }
#'
#' @seealso \code{\link{fit_biexponential_model}}, \code{\link{fit_lognormal_log_model}}, \code{\link{fit_all_models_log}}
#' @export
fit_exponential_log_model <- function(data,
                                       age_var = "age",
                                       observed_col = "CurrentPopularity",
                                       group_var = NULL,
                                       group_var2 = NULL,
                                       weight_early_points = FALSE) {

  if (weight_early_points) {
    message("Note: 'weight_early_points = TRUE' is ignored in fit_exponential_log_model(). No weighting is applied.")
  }

  data <- data |>
    dplyr::filter(.data[[age_var]] > 0)

  fit_model <- function(df) {
    model <- tryCatch({
      nls(
        stats::as.formula(paste0("log(`", observed_col, "`) ~ log(c) - qq * ", age_var)),
        data = df,
        start = list(qq = 0.1, c = 0.07),
        control = list(maxiter = 1000)
      )
    }, error = function(e) NULL)

    if (!is.null(model)) {
      preds <- exp(predict(model, newdata = df))
      df$fitted_correct <- preds

      residuals <- df[[observed_col]] - preds
      r2 <- 1 - sum(residuals^2) / sum((df[[observed_col]] - mean(df[[observed_col]]))^2)

      param_df <- tibble::tibble(
        qq = coef(model)["qq"],
        c = coef(model)["c"],
        AIC = AIC(model),
        BIC = BIC(model),
        R2 = r2
      )
    } else {
      df$fitted_correct <- NA
      param_df <- tibble::tibble(
        qq = NA, c = NA, AIC = NA, BIC = NA, R2 = NA
      )
    }

    list(fitted = df, params = param_df)
  }

  if (!is.null(group_var2) && group_var2 %in% names(data)) {
    level_groups <- split(data, data[[group_var2]])

    results <- purrr::map(level_groups, function(level_df) {
      facet_level <- unique(level_df[[group_var2]])

      if (!is.null(group_var) && group_var %in% names(level_df)) {
        group_results <- level_df |>
          dplyr::group_by(.data[[group_var]]) |>
          dplyr::group_split() |>
          purrr::map(function(df_group) {
            result <- fit_model(df_group)
            result$fitted[[group_var]] <- unique(df_group[[group_var]])
            result$params[[group_var]] <- unique(df_group[[group_var]])
            result$fitted[[group_var2]] <- facet_level
            result$params[[group_var2]] <- facet_level
            result
          })
        list(
          fitted = purrr::list_rbind(purrr::map(group_results, "fitted")),
          params = purrr::list_rbind(purrr::map(group_results, "params"))
        )
      } else {
        result <- fit_model(level_df)
        result$fitted[[group_var2]] <- facet_level
        result$params[[group_var2]] <- facet_level
        result
      }
    })

    return(list(
      fitted = purrr::list_rbind(purrr::map(results, "fitted")),
      params = purrr::list_rbind(purrr::map(results, "params"))
    ))
  }

  if (!is.null(group_var) && group_var %in% names(data)) {
    group_results <- data |>
      dplyr::group_by(.data[[group_var]]) |>
      dplyr::group_split() |>
      purrr::map(function(df_group) {
        result <- fit_model(df_group)
        result$fitted[[group_var]] <- unique(df_group[[group_var]])
        result$params[[group_var]] <- unique(df_group[[group_var]])
        result
      })
    return(list(
      fitted = purrr::list_rbind(purrr::map(group_results, "fitted")),
      params = purrr::list_rbind(purrr::map(group_results, "params"))
    ))
  }

  return(fit_model(data))
}









#' Fit Log-Normal Modulated Power-Law to Memory Data (Grouped or Not)
#'
#' Fits a log-normal-inspired forgetting curve to memory or attention data,
#' optionally stratified by grouping variables. The model is estimated using a
#' quadratic regression on the log-log scale.
#'
#' @section Model Specification:
#'
#' The model is estimated as:
#' \deqn{
#' \log S(t) = b + b_1 \log(t) - b_2 (\log(t))^2
#' }
#'
#' which is equivalent to:
#' \deqn{
#' S(t) = \exp(b) \cdot t^{b_1} \cdot \exp\left(-b_2 (\log t)^2\right)
#' }
#'
#' where:
#' \itemize{
#'   \item \eqn{S(t)} is the observed popularity or recall at time \eqn{t},
#'   \item \eqn{b, b_1, b_2} are parameters controlling early growth and long-tail decay.
#' }
#'
#' This model captures:
#' \itemize{
#'   \item Initial attention growth via the power-law term \eqn{t^{b_1}},
#'   \item Long-tail memory decay via the log-normal term \eqn{\exp(-b_2 (\log t)^2)}.
#' }
#'
#' ## Why log-log transformation?
#' \itemize{
#'   \item Improves numerical stability during model fitting.
#'   \item Enables interpretable parameters in logarithmic space.
#'   \item Captures the heavy-tailed nature of attention decay.
#'   \item Linearizes a nonlinear decay relationship for fitting.
#' }
#'
#' @param data A data frame containing memory or attention data.
#' @param age_var Name of the age column (e.g., time since event or publication).
#' @param observed_col Name of the observed attention/popularity column.
#' @param group_var Optional: primary grouping column (e.g., "same_country").
#' @param group_var2 Optional: secondary grouping column (e.g., "attention_level").
#' @param weight_early_points Logical. Whether to give more weight to early time points (default: FALSE).
#'
#' @return A list containing:
#' \describe{
#'   \item{fitted}{A data frame with predicted values by age and group.}
#'   \item{params}{Estimated parameters \eqn{b}, \eqn{b_1}, \eqn{b_2}, and model metrics (AIC, BIC, R²).}
#' }
#'
#' @seealso \code{\link{fit_biexponential_model}}, \code{\link{fit_exponential_log_model}}, \code{\link{fit_all_models_log}}
#' @export
fit_lognormal_log_model <- function(data,
                                    age_var = "age",
                                    observed_col = "CurrentPopularity",
                                    group_var = NULL,
                                    group_var2 = NULL,
                                    weight_early_points = FALSE) {

  required_cols <- c(age_var, observed_col)
  missing <- setdiff(required_cols, names(data))
  if (length(missing) > 0) {
    stop("Missing required column(s): ", paste(missing, collapse = ", "))
  }

  if (!is.null(group_var2) && !(group_var2 %in% names(data))) {
    stop("The group_var2 column was specified but not found in the dataset.")
  }

  data <- data %>% dplyr::filter(.data[[age_var]] > 0)

  fit_model <- function(df) {
    df <- df %>% dplyr::mutate(log_age = log(df[[age_var]]))
    weights <- if (weight_early_points) 1 / (df[[age_var]] + 1e-3) else NULL

    model <- tryCatch({
      nls(log(df[[observed_col]]) ~ b + b1 * log_age - b2 * log_age^2,
          data = df,
          start = list(b = 1, b1 = -0.67, b2 = 0.01),
          lower = c(b = -Inf, b1 = -Inf, b2 =  -Inf), # For decay b2 should be >0. If b2=0, the deacy is purely power-law
          algorithm = "port",
          weights = weights,
          control = list(maxiter = 1000))
    }, error = function(e) NULL)

    if (!is.null(model)) {
      preds <- exp(predict(model, newdata = df))
      residuals <- df[[observed_col]] - preds
      r2 <- 1 - (sum(residuals^2) / sum((df[[observed_col]] - mean(df[[observed_col]]))^2))

      df <- df %>% dplyr::mutate(fitted_correct = preds)

      param_df <- tibble::tibble(
        b = coef(model)["b"],
        b1 = coef(model)["b1"],
        b2 = coef(model)["b2"],
        AIC = AIC(model),
        BIC = BIC(model),
        R2 = r2
      )
    } else {
      df <- df %>% dplyr::mutate(fitted_correct = NA)
      param_df <- tibble::tibble(
        b = NA, b1 = NA, b2 = NA,
        AIC = NA, BIC = NA, R2 = NA
      )
    }

    return(list(fitted = df, params = param_df))
  }

  if (!is.null(group_var2)) {
    results <- data %>% dplyr::group_split(.data[[group_var2]]) %>%
      purrr::map(function(level_df) {
        facet_level <- unique(level_df[[group_var2]])

        if (!is.null(group_var) && group_var %in% names(level_df)) {
          grouped_results <- level_df %>% dplyr::group_by(.data[[group_var]]) %>%
            dplyr::group_split() %>%
            purrr::map(function(df_group) {
              result <- fit_model(df_group)
              result$fitted[[group_var]] <- unique(df_group[[group_var]])
              result$params[[group_var]] <- unique(df_group[[group_var]])
              result$fitted[[group_var2]] <- facet_level
              result$params[[group_var2]] <- facet_level
              return(result)
            })

          list(
            fitted = dplyr::bind_rows(purrr::map(grouped_results, "fitted")),
            params = dplyr::bind_rows(purrr::map(grouped_results, "params"))
          )

        } else {
          result <- fit_model(level_df)
          result$fitted[[group_var2]] <- facet_level
          result$params[[group_var2]] <- facet_level
          return(result)
        }
      })

    fitted_data <- dplyr::bind_rows(purrr::map(results, "fitted"))
    parameters <- dplyr::bind_rows(purrr::map(results, "params"))

  } else if (!is.null(group_var) && group_var %in% names(data)) {
    results <- data %>% dplyr::group_by(.data[[group_var]]) %>% dplyr::group_split() %>%
      purrr::map(function(df_group) {
        result <- fit_model(df_group)
        result$fitted[[group_var]] <- unique(df_group[[group_var]])
        result$params[[group_var]] <- unique(df_group[[group_var]])
        return(result)
      })

    fitted_data <- dplyr::bind_rows(purrr::map(results, "fitted"))
    parameters <- dplyr::bind_rows(purrr::map(results, "params"))

  } else {
    result <- fit_model(data)
    fitted_data <- result$fitted
    parameters <- result$params
  }

  if (all(is.na(fitted_data$fitted_correct))) {
    warning("Model did not converge for any subset. Consider tuning initial parameters.")
  } else {
    message("Log-normal model (log-transformed) successfully fitted.")
  }

  return(list(fitted = fitted_data, params = parameters))
}



#' Fit All Log-Transformed Decay Models: Biexponential, Log-Normal, Exponential
#'
#' Fits three theoretical memory decay models to time-dependent attention or recall data:
#'
#' \itemize{
#'   \item \strong{Biexponential Decay:}
#'   \deqn{
#'   S(t) = N \left[ \exp(-(p + r)t) + \frac{r}{p + r - q} \left( \exp(-qt) - \exp(-(p + r)t) \right) \right]
#'   }
#'
#'   \item \strong{Log-Normal Modulated Power Law:}
#'   \deqn{
#'   S(t) = \exp(b) \cdot t^{b_1} \cdot \exp\left(-b_2 (\log t)^2\right)
#'   }
#'
#'   \item \strong{Exponential Decay:}
#'   \deqn{
#'   S(t) = c \cdot \exp(-qt)
#'   }
#' }
#'
#' All models are fitted in log-transformed form using nonlinear least squares (NLS).
#' Grouped and stratified fitting is supported through one or two categorical variables.
#'
#' @param data A data frame with time-series or cross-sectional data (e.g., memory or attention scores).
#' @param age_var Name of the column representing age (e.g., time since event or figure).
#' @param observed_col Name of the observed response variable to be modeled (e.g., "loess_correct").
#' @param group_var Optional: first grouping variable (e.g., "same_country", "decay_bin").
#' @param group_var2 Optional: second grouping variable (e.g., attention level, quantiles).
#' @param N_ref Optional: fixed value for parameter \eqn{N} in the biexponential model. If \code{NULL}, it will be estimated.
#' @param weight_early_points Logical. If \code{TRUE}, assigns higher weight to early observations to prioritize early decay (default: \code{TRUE}).
#'
#' @return A named list with the following components:
#' \describe{
#'   \item{biexponential}{A data frame of fitted values and parameters for the biexponential model.}
#'   \item{lognormal}{Fitted results for the log-normal modulated power-law model.}
#'   \item{exponential}{Fitted results for the exponential decay model.}
#'   \item{model_comparison}{Model comparison table including AIC and BIC for each fitted model.}
#' }
#'
#' @seealso \code{\link{fit_biexponential_model}}, \code{\link{fit_lognormal_log_model}}, \code{\link{fit_exponential_log_model}}, \code{\link{plot_all_models}}
#'
#' @export
fit_all_models_log <- function(data,
                                age_var = "age",
                                observed_col = "CurrentPopularity",
                                group_var = NULL,
                                group_var2 = NULL,
                                N_ref = NULL,
                                weight_early_points = TRUE) {

  # ---- Fit all models ----
  biexp_fit <- fit_biexponential_model(
    data,
    age_var = age_var,
    observed_col = observed_col,
    group_var = group_var,
    group_var2 = group_var2,
    N_ref = N_ref,
    weight_early_points = weight_early_points
  )

  logn_fit <- fit_lognormal_log_model(
    data,
    age_var = age_var,
    observed_col = observed_col,
    group_var = group_var,
    group_var2 = group_var2,
    weight_early_points = weight_early_points
  )

  exp_fit <- fit_exponential_log_model(
    data,
    age_var = age_var,
    observed_col = observed_col,
    group_var = group_var,
    group_var2 = group_var2,
    weight_early_points = weight_early_points
  )

  # ---- Helper to attach model name ----
  safe_add_model <- function(df, name) {
    if (is.null(df) || !inherits(df, "data.frame")) return(NULL)
    if (!all(c("AIC", "BIC") %in% names(df))) return(NULL)
    df$model <- name
    dplyr::relocate(df, model, .after = dplyr::last_col())
  }

    # ---- Combine comparisons ----
  comparison_raw <- list(
    safe_add_model(biexp_fit$params, "biexponential"),
    safe_add_model(logn_fit$params, "lognormal"),
    safe_add_model(exp_fit$params, "exponential")
  ) |>
    purrr::compact() |>
    dplyr::bind_rows()

  # Determine ID columns dynamically
  id_vars <- c(group_var, group_var2)
  id_vars <- id_vars[!is.null(id_vars) & id_vars %in% names(comparison_raw)]

  # Build comparison table with AIC, BIC, and R2
  comparison <- comparison_raw |>
    dplyr::select(dplyr::any_of(c(id_vars, "model", "AIC", "BIC", "R2")))

    
  return(list(
    biexponential = biexp_fit,
    lognormal = logn_fit,
    exponential = exp_fit,
    model_comparison = comparison
  ))
}
