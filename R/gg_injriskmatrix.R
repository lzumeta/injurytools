#' Risk matrix plot
#'
#' Given an \code{injds} \strong{S3} object it depicts risk matrix plots (see
#' References section), a graph of injury severity plotted against injury
#' incidence. The point estimates of each measure, as well as their confidence
#' intervals, are plotted according to the method used when running
#' \code{\link[=injsummary]{injsummary()}} function.
#'
#' @param injds \code{injds} \strong{S3} object (see
#'   \code{\link[=injsummary]{injsummary()}})
#' @param var_type_injury Character specifying the name of the column, a
#'   (categorical) variable referring to the "type of injury" (e.g.
#'   muscular/articular/others or overuse/not-overuse etc.) according to which
#'   visualize injury summary statistics (optional, defaults \code{NULL}).
#' @param add_contour FILL
#' @param title Text for the main title, passed to
#'   \code{\link[ggplot2:ggtitle]{ggplot2::ggtitle()}}.
#' @param xlab  x-axis label to be passed to
#'   \code{\link[ggplot2:xlab]{ggplot2::xlab()}}.
#' @param ylab  y-axis label to be passed to
#'   \code{\link[ggplot2:ylab]{ggplot2::ylab()}}.
#' @param errh_height Set the height of the horizontal interval whiskers;
#'   \code{height} argument for
#'   \code{\link[ggplot2:geom_errorbarh]{ggplot2::geom_errorbarh()}}
#' @param errv_width Set the width of the vertical interval whiskers;
#'   \code{width} argument for
#'   \code{\link[ggplot2:geom_errorbar]{ggplot2::geom_errorbar()}}
#'
#' @return  A ggplot object (to which optionally more layers can be added).
#' @export
#'
#' @importFrom checkmate assert checkClass assert_subset
#' @importFrom metR geom_text_contour
#'
#' @references
#' Bahr R, Clarsen B, Derman W, et al. International Olympic Committee
#' consensus statement: methods for recording and reporting of epidemiological
#' data on injury and illness in sport 2020 (including STROBE Extension for
#' Sport Injury and Illness Surveillance (STROBE-SIIS)) \emph{British Journal of
#' Sports Medicine} 2020; 54:372-389.
#'
#' Fuller C. W. (2018). Injury Risk (Burden), Risk Matrices and Risk Contours
#' in Team Sports: A Review of Principles, Practices and Problems. \emph{Sports
#' Medicine}, 48(7), 1597–1606. https://doi.org/10.1007/s40279-018-0913-5
#'
#' @examples
#' # Prepare data
#' # df_exposures <- prepare_exp(raw_df_exposures, player = "player_name",
#' #                             date = "year", time_expo = "minutes_played")
#' # df_injuries  <- prepare_inj(raw_df_injuries, player = "player_name",
#' #                             date_injured = "from", date_recovered = "until")
#' # injd         <- prepare_all(data_exposures = df_exposures,
#' #                             data_injuries  = df_injuries,
#' #                             exp_unit = "matches_minutes")
#'
#' # Estimate injury summary statistics
#' injds <- injsummary(injd)
#' injds2 <- injsummary(injd, var_type_injury = "injury_type")
#'
#' # Plot
#' gg_injriskmatrix(injds)
#' gg_injriskmatrix(injds2, var_type_injury = "injury_type", title = "Risk matrix")
gg_injriskmatrix <- function(injds, var_type_injury = NULL,
                             add_contour = TRUE,
                             title = NULL,
                             xlab = "Incidence (injuries per _)",
                             ylab = "Burden (days lost per _)",
                             errh_height = 1, errv_width = 0.05) {
  ## check inputs
  assert(checkClass(injds, "injds"))
  assert_subset(var_type_injury, c(names(injds[[2]]), NULL))

  injds_data <- injds[[2]]
  if(!is.null(var_type_injury)) var_type_injury <- rlang::sym(var_type_injury)

  if (xlab == "Incidence (injuries per _)" & ylab == "Burden (days lost per _)") {
    unit_timerisk <- attr(injds, "unit_timerisk")
    xlab <- stringr::str_replace(xlab, "_", unit_timerisk)
    ylab <- stringr::str_replace(ylab, "_", unit_timerisk)
  }

  if (add_contour) {
    grid <- expand.grid(x = seq(0, ceiling(max(injds_data[["injincidence_upper"]]))),
                        y = seq(0, ceiling(max(injds_data[["injburden_upper"]]))))
    grid$z <- grid$x*grid$y
    labels <- unique(grid$z[grid$z <= max(injds_data[["injburden_upper"]])])
    grid_label <- data.frame(x = rep(max(grid$x), each = length(labels)),
                             y = labels)
  }

  ## plot
  p <- ggplot(data = injds_data, aes(x = .data$injincidence, y = .data$injburden, group = !! var_type_injury)) +
    geom_errorbarh(aes(xmin = .data$injincidence_lower, xmax = .data$injincidence_upper), height = errh_height) +
    geom_errorbar(aes(ymin = .data$injburden_lower, ymax = .data$injburden_upper), width = errv_width) +
    geom_point(aes(fill = !! var_type_injury), size = 3.5, colour = "black", shape = 21) +
    xlab(xlab) + ylab(ylab) + ggtitle(title) +
    theme_bw()
  if (add_contour) {
    p <- p +
      geom_contour(aes(x = .data$x, y = .data$y, z = .data$z),
                   data = grid, inherit.aes = F,
                   linetype = "longdash", colour = "gray") +
      geom_text_contour(aes(x = .data$x, y = .data$y, z = .data$z), data = grid, inherit.aes = F,
                        colour = "gray", stroke = 0.15)
  }
  return(p)
}
