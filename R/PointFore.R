#' PointFore: A package for estimating state-dependent quantile and expectile levels from a time series of point forecasts and observations
#'
#' Estimate specification models for the state-dependent level of an optimal quantile/expectile forecast.
#' Wald Tests and the test of overidentifying restrictions are implemented. Plotting of the estimated specification model is possible.
#' The package contains daily accumulated precipitation at London, UK from the high-resolution model of the
#' European Centre for Medium-Range Weather Forecasts (ECMWF, https://www.ecmwf.int/). The package further contains quarterly GDP growth data with observations and forecasts from the Federal Reserve's Greenbook.
#' Based on "Interpretation of Point Forecasts" by Patrick Schmidt, Matthias Katzfuss, and Tilmann Gneiting.
#'
#' @section PointFore functions:
#' The main function is \code{\link{estimate.functional}}. It returns an object which can be
#' analyzed with \code{\link{plot.pointfore}} and \code{\link{summary.pointfore}}.
#'
#' @docType package
#'
#' @name PointFore
NULL
