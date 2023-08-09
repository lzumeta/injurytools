#' Plot risk matrices
#'
#' Given an \code{injds} \strong{S3} object, it depicts risk matrix plots , a
#' graph in which the injury incidence (frequency) is plotted against the
#' average days lost per injury (consequence). The point estimate of injury
#' incidence together with its confidence interval is plotted, according to the
#' method used when running \code{\link[=injsummary]{injsummary()}} function. On
#' the y-axis, the mean time-loss per injury together with \eqn{\pm} IQR (days)
#' is plotted. The number shown inside the point and the point size itself,
#' report the injury burden (days lost per player-exposure time), the bigger the
#' size the greater the burden. See References section.
#'
#'
#' @param injds \code{injds} \strong{S3} object (see
#'   \code{\link[=injsummary]{injsummary()}})
#' @param var_type_injury Character specifying the name of the column. A
#'   (categorical) variable referring to the "type of injury" (e.g.
#'   muscular/articular/others or overuse/not-overuse etc.) according to which
#'   visualize injury summary statistics (optional, defaults to \code{NULL}).
#' @param add_contour Logical, whether or not to add contour lines of the
#'   product between injury incidence and mean severity (i.e. 'incidence x
#'   average time-loss'), which leads to injury burden (defaults to
#'   \code{TRUE}).
#' @param title Text for the main title passed to
#'   \code{\link[ggplot2:ggtitle]{ggplot2::ggtitle()}}.
#' @param xlab  x-axis label to be passed to
#'   \code{\link[ggplot2:xlab]{ggplot2::xlab()}}.
#' @param ylab  y-axis label to be passed to
#'   \code{\link[ggplot2:ylab]{ggplot2::ylab()}}.
#' @param errh_height Set the height of the horizontal interval whiskers; the
#'   \code{height} argument for
#'   \code{\link[ggplot2:geom_errorbarh]{ggplot2::geom_errorbarh()}}
#' @param errv_width Set the width of the vertical interval whiskers; the
#'   \code{width} argument for \cr
#'   \code{\link[ggplot2:geom_errorbar]{ggplot2::geom_errorbar()}}
#'
#' @return  A ggplot object (to which optionally more layers can be added).
#' @export
#'
#' @importFrom checkmate assert checkClass assert_subset
#' @importFrom metR geom_text_contour
#' @importFrom stringr str_split
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
#' Medicine}, 48(7), 1597–1606. \cr https://doi.org/10.1007/s40279-018-0913-5
#'
#' @examples
#' \donttest{
#' df_exposures <- prepare_exp(raw_df_exposures, player = "player_name",
#'                             date = "year", time_expo = "minutes_played")
#' df_injuries  <- prepare_inj(raw_df_injuries, player = "player_name",
#'                             date_injured = "from", date_recovered = "until")
#' injd         <- prepare_all(data_exposures = df_exposures,
#'                             data_injuries  = df_injuries,
#'                             exp_unit = "matches_minutes")
#' }
#'
#' \donttest{
#' injds <- injsummary(injd)
#' injds2 <- injsummary(injd, var_type_injury = "injury_type")
#'
#' gg_injriskmatrix(injds)
#' gg_injriskmatrix(injds2, var_type_injury = "injury_type", title = "Risk matrix")
#' }
gg_injriskmatrix <- function(injds, var_type_injury = NULL,
                             add_contour = TRUE,
                             title = NULL,
                             xlab = "Incidence (injuries per _)",
                             ylab = "Mean time-loss (days) per injury",
                             errh_height = 1, errv_width = 0.05) {
  ## check inputs
  assert(checkClass(injds, "injds"))
  assert_subset(var_type_injury, c(names(injds[[2]]), NULL))

  injds_data <- injds[[2]]
  if (!is.null(var_type_injury)) var_type_injury <- rlang::sym(var_type_injury)
  ## add proper iqr numeric values
  injds_data <- injds_data |>
    mutate(quart25_dayslost = as.numeric(stringr::str_split(.data$iqr_dayslost, "-", simplify = T)[, 1]),
           quart75_dayslost = as.numeric(stringr::str_split(.data$iqr_dayslost, "-", simplify = T)[, 2]))

  if (any(injds_data$mean_dayslost > injds_data$quart75_dayslost)) {
    warning("An injury average time-loss is bigger than its 75% quartile
    time-loss. Possibly, there is a concrete long-term injury in the data.
    For plotting purposes the vertical upper limit (i.e. 75% quartile)
    is lengthened to the average value.")
    ## just for plotting
    idx <- which(injds_data$mean_dayslost > injds_data$quart75_dayslost)
    injds_data$quart75_dayslost[idx] <- injds_data$mean_dayslost[idx]
  }

  if (xlab == "Incidence (injuries per _)") {
    unit_timerisk <- attr(injds, "unit_timerisk")
    xlab <- stringr::str_replace(xlab, "_", unit_timerisk)
  }

  if (add_contour) {
    grid <- expand.grid(x = seq(0, ceiling(max(injds_data[["injincidence_upper"]])), length.out = 100),
                        y = seq(0, ceiling(max(injds_data[["quart75_dayslost"]]))),  length.out = 100)
    grid$z <- grid$x*grid$y
    # labels <- unique(grid$z[grid$z <= max(injds_data[["quart75_dayslost"]])])
    # grid_label <- data.frame(x = rep(max(grid$x), each = length(labels)),
    #                          y = labels)
  }

  ## plot
  p <- ggplot(data = injds_data, aes(x = .data$injincidence, y = .data$mean_dayslost, group = !! var_type_injury)) +
    geom_errorbarh(aes(xmin = .data$injincidence_lower, xmax = .data$injincidence_upper), height = errh_height) +
    geom_errorbar(aes(ymin  = .data$quart25_dayslost,   ymax = .data$quart75_dayslost), width = errv_width) +
    geom_point(aes(fill = !! var_type_injury, size = .data$injburden), colour = "black", shape = 21) +
    geom_text(aes(label = round(.data$injburden), fontface = "bold")) +
    # quit size legend
    scale_size_continuous(range = c(4, 9), guide = "none") +
    xlab(xlab) + ylab(ylab) + ggtitle(title) +
    theme_bw()

  if (add_contour) {
    p <- p +
      geom_contour(aes(x = .data$x, y = .data$y, z = .data$z),
                   data = grid, inherit.aes = F,
                   linetype = "longdash", colour = "gray") +
      geom_text_contour(aes(x = .data$x, y = .data$y, z = .data$z),
                        data = grid, inherit.aes = F,
                        colour = "gray", stroke = 0.15)
  }

  return(p)
}
