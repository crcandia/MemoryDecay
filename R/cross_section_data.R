#' Example dataset: Cross-sectional song popularity data (Billboard-based)
#'
#' This dataset contains cross-sectional data on the popularity of songs and artists,
#' measured on October 29th, 2016. Each row corresponds to a song that entered the
#' Billboard ranking at some point prior to that date.
#'
#' The data is used to illustrate the modeling of forgetting curves in collective attention.
#'
#' @format A data frame with multiple rows and the following variables:
#' \describe{
#'   \item{Song}{Character. Title of the song.}
#'   \item{Artist}{Character. Name of the artist.}
#'   \item{Date}{Date. The date when the song entered the Billboard ranking (i.e., date of accomplishment).}
#'   \item{CurrentPopularity}{Numeric. Popularity of the song as of October 29th, 2016, measured by streaming volume, standardized and in linear scale.}
#'   \item{age}{Numeric. Time since the song entered the ranking, measured in years.}
#'   \item{Control1}{Numeric. Control variable reflecting initial popularity at the time of Billboard entry.}
#'   \item{Control2}{Numeric. Another control for initial attention at Billboard entry.}
#' }
#'
#' @source Derived from Billboard song rankings and streaming data as of 2016-10-29.
#'
#' @examples
#' data("cross_section_data")
#' head(cross_section_data)
"cross_section_data"
