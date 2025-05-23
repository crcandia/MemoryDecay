% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_decay_models.R
\name{plot_all_models}
\alias{plot_all_models}
\title{Plot All Fitted Forgetting Curves (Biexponential, Exponential, Log-normal)}
\usage{
plot_all_models(
  model_outputs,
  age_var = "age",
  observed_col = "CurrentPopularity",
  group_var = NULL,
  group_var2 = NULL,
  log_y = TRUE,
  log_x = FALSE
)
}
\arguments{
\item{model_outputs}{A list returned by \code{\link{fit_all_models_log}}, containing fitted values for each model
and a merged data frame in \code{$fitted}.}

\item{age_var}{Name of the column indicating age or time since the event (default: "age").}

\item{observed_col}{Name of the observed attention or recall variable (default: "CurrentPopularity").}

\item{group_var}{Optional. Primary grouping variable for line color and point shape (e.g., "same_country", "decay_bin").}

\item{group_var2}{Optional. Secondary grouping variable used for faceting (e.g., attention level or demographic stratum).}

\item{log_y}{Logical. If \code{TRUE}, apply log10 transformation to the Y axis. Default is \code{TRUE}.}

\item{log_x}{Logical. If \code{TRUE}, apply log10 transformation to the X axis. Default is \code{FALSE}.}
}
\value{
A \code{ggplot2} object with layered curves and observed values, styled and faceted according to grouping variables.
}
\description{
This function overlays the fitted forgetting curves from three competing models:
\itemize{
  \item \strong{Biexponential}: captures short- and long-term decay via two interacting memory systems.
  \item \strong{Exponential}: simple continuous decay process.
  \item \strong{Log-normal modulated power-law}: captures early attention rise followed by long-tail decay.
}
}
\details{
It compares each model's predictions against observed attention or recall values.
The function supports grouping (color/style by category) and faceting for stratified comparison
across populations or experimental conditions.
}
\seealso{
\code{\link{fit_biexponential_model}}, \code{\link{fit_lognormal_log_model}},
\code{\link{fit_exponential_log_model}}, \code{\link{compare_model_fits}}
}
