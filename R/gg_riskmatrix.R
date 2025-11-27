#' Plot risk matrices
#'
#' Depict risk matrix plots, a graph in which the case (e.g. injury) incidence (frequency)
#' is plotted against the average days lost per case (consequence). The point
#' estimate of case incidence together with its confidence interval is
#' plotted, according to the method specified. On the y-axis, the mean time-loss
#' per case together with \eqn{\pm} IQR (days) is plotted. The number shown
#' inside the point and the point size itself, report the case burden (days
#' lost per athlete-exposure time), the bigger the size the greater the burden.
#' See References section.
#'
#' @param injd \code{injd} \strong{S3} object (see
#'   \code{\link[=prepare_all]{prepare_all()}}).
#' @param by Character specifying the name of the column. A (categorical)
#'   variable referring to the "type of case" (e.g. "type of injury"
#'   muscular/articular/others or overuse/not-overuse etc.) according to which
#'   visualize epidemiological summary statistics (optional, defaults to
#'   \code{NULL}).
#' @param method Method to estimate the incidence (burden) rate. One of
#'   "poisson", "negbin", "zinfpois" or "zinfnb"; that stand for Poisson method,
#'   negative binomial method, zero-inflated Poisson and zero-inflated negative
#'   binomial.
#' @param add_contour Logical, whether or not to add contour lines of the
#'   product between case incidence and mean severity (i.e. 'incidence x
#'   average time-loss'), which leads to case burden (defaults to
#'   \code{TRUE}).
#' @param title Text for the main title passed to
#'   \code{\link[ggplot2:ggtitle]{ggplot2::ggtitle()}}.
#' @param xlab  x-axis label to be passed to
#'   \code{\link[ggplot2:xlab]{ggplot2::xlab()}}.
#' @param ylab  y-axis label to be passed to
#'   \code{\link[ggplot2:ylab]{ggplot2::ylab()}}.
#' @param errh_height Set the height of the horizontal interval whiskers; the
#'   \code{height} argument for
#'   \code{\link[ggplot2:geom_errorbar]{ggplot2::geom_errorbar()}}.
#' @param errv_width Set the width of the vertical interval whiskers; the
#'   \code{width} argument for \cr
#'   \code{\link[ggplot2:geom_errorbar]{ggplot2::geom_errorbar()}}.
#' @param cont_max_x,cont_max_y Numerical (optional) values indicating the
#'  maximum on the x-axis and y-axis, respectively, to be reached by the
#'  contour.
#' @param ... Other arguments passed on to
#'  \code{\link[ggplot2:geom_contour]{ggplot2::geom_contour()}} and
#'  \code{\link[metR:geom_text_contour]{metR::geom_text_contour()}}.
#'  These are often aesthetics like `bins = 15` or `breaks = 10`.
#'
#' @return  A ggplot object (to which optionally more layers can be added).
#' @export
#'
#' @importFrom checkmate assert checkClass assert_subset
#' @importFrom metR geom_text_contour
#' @importFrom stringr str_split
#' @importFrom rlang sym
#'
#' @references
#' Bahr R, Clarsen B, Derman W, et al. International Olympic Committee
#' consensus statement: methods for recording and reporting of epidemiological
#' data on injury and illness in sport 2020 (including STROBE Extension for
#' Sport Injury and Illness Surveillance (STROBE-SIIS)) \emph{British Journal of
#' Sports Medicine} 2020; 54:372-389.
#'
#' Fuller C. W. (2018). Injury Risk (Burden), Risk Matrices and Risk Contours
#' in Team Sports: A Review of Principles, Practices and Problems.\emph{Sports
#' Medicine}, 48(7), 1597–1606. \cr \url{https://doi.org/10.1007/s40279-018-0913-5}
#'
#' @examples
#' \donttest{
#' df_exposures <- prepare_exp(raw_df_exposures, person_id = "player_name",
#'                             date = "year", time_expo = "minutes_played")
#' df_injuries  <- prepare_inj(raw_df_injuries, person_id = "player_name",
#'                             date_injured = "from", date_recovered = "until")
#' injd         <- prepare_all(data_exposures = df_exposures,
#'                             data_injuries  = df_injuries,
#'                             exp_unit = "matches_minutes")
#' }
#'
#' \donttest{
#' gg_riskmatrix(injd)
#' gg_riskmatrix(injd, by = "injury_type", title = "Risk matrix")
#' }
gg_riskmatrix <- function(injd, by = NULL,
                          method = c("poisson", "negbin", "zinfpois", "zinfnb"),
                          add_contour = TRUE,
                          title = NULL,
                          xlab = "Incidence (injuries per _)",
                          ylab = "Mean time-loss (days) per injury",
                          errh_height = 1, errv_width = 0.05,
                          cont_max_x = NULL,
                          cont_max_y = NULL,
                          ...) {
  ## check inputs
  assert(checkClass(injd, "injd"))
  assert_subset(by, c(names(injd), NULL))

  summary_data <- calc_summary(injd, by = by, overall = TRUE,
                               scale = TRUE, quiet = TRUE)

  if (!is.null(by)) by <- rlang::sym(by)

  if (any(summary_data$mean_dayslost > summary_data$qt25_dayslost)) {
    warning("An injury average time-loss is bigger than its 75% quartile
    time-loss. Possibly, there is a concrete long-term injury in the data.
    For plotting purposes the vertical upper limit (i.e. 75% quartile)
    is lengthened to the average value.")
    ## just for plotting
    idx <- which(summary_data$mean_dayslost > summary_data$qt75_dayslost)
    summary_data$qt75_dayslost[idx] <- summary_data$mean_dayslost[idx]
  }

  if (xlab == "Incidence (injuries per _)") {
    unit <- attr(injd, "unit_exposure")
    unit_timerisk <- label_unit(unit)
    xlab <- stringr::str_replace(xlab, "_", unit_timerisk)
  }

  if (add_contour) {
    if (is.null(cont_max_x)) {
      cont_max_x <- ceiling(max(summary_data[["incidence_upper"]]))
    }
    if (is.null(cont_max_y)) {
      cont_max_y <- ceiling(max(summary_data[["qt75_dayslost"]]))
    }
    grid <- expand.grid(x = seq(0, cont_max_x, length.out = 100),
                        y = seq(0, cont_max_y, length.out = 100))
    grid$z <- grid$x * grid$y
  }

  ## plot
  p <- ggplot(data = summary_data, aes(x = .data$incidence, y = .data$mean_dayslost, group = !! by)) +
    geom_errorbar(aes(xmin = .data$incidence_lower, xmax = .data$incidence_upper), height = errh_height, orientation = "y") +
    geom_errorbar(aes(ymin  = .data$qt25_dayslost,   ymax = .data$qt75_dayslost), width = errv_width) +
    geom_point(aes(fill = !! by, size = .data$burden), colour = "black", shape = 21) +
    geom_text(aes(label = round(.data$burden), fontface = "bold")) +
    # quit size legend
    scale_size_continuous(range = c(4, 9), guide = "none") +
    xlim(c(-0.05, NA)) +
    ylim(c(-0.05, NA)) +
    xlab(xlab) + ylab(ylab) + ggtitle(title) +
    theme_bw()

  if (add_contour) {
    p <- p +
      geom_contour(aes(x = .data$x, y = .data$y, z = .data$z),
                   data = grid, inherit.aes = F,
                   linetype = "longdash", colour = "gray", ...) +
      geom_text_contour(aes(x = .data$x, y = .data$y, z = .data$z),
                        data = grid, inherit.aes = F,
                        colour = "gray", stroke = 0.15,...)
  }

  return(p)
}
