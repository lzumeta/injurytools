#' Plot athlete's health problem incidence or burden ranking
#'
#' A bar chart that shows athlete-wise summary statistics, either case
#' incidence or injury burden, ranked in descending order.
#'
#' @inheritParams calc_ncases
#' @param summary_stat A character value indicating whether to plot case
#'   incidence's (case's) or injury burden's (days losts') ranking. One of
#'   "incidence" ("ncases") or "burden" ("ndayslost"), respectively.
#' @param line_overall Logical, whether to draw a vertical red line indicating the overall incidence or burden. Defaults to \code{FALSE}.
#' @param title Text for the main title.
#'
#' @return A ggplot object (to which optionally more layers can be added).
#' @export
#'
#' @importFrom checkmate assert checkClass checkChoice
#' @importFrom forcats fct_reorder
#' @importFrom tidyselect all_of
#' @importFrom dplyr select
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
#'}
#'
#'
#' p1 <- gg_rank(injd, summary_stat = "incidence",
#'               line_overall = TRUE,
#'               title = "Overall injury incidence per player") +
#'   ggplot2::ylab(NULL)
#' p2 <- gg_rank(injd, summary_stat = "burden",
#'               line_overall = TRUE,
#'               title = "Overall injury burden per player") +
#'   ggplot2::ylab(NULL)
#'
#' # install.packages("gridExtra")
#' # library(gridExtra)
#' if (require("gridExtra")) {
#'   gridExtra::grid.arrange(p1, p2, nrow = 1)
#' }
gg_rank <- function(injd,
                    by = NULL,
                    summary_stat = c("incidence", "burden", "ncases", "ndayslost"),
                    line_overall = FALSE,
                    title = NULL) {

  if (missing(summary_stat)) summary_stat <- "incidence"

  ## check inputs
  assert(checkClass(injd, "injd"),
         checkChoice(summary_stat, c(missing(summary_stat),
                                     "incidence", "burden",
                                     "ncases", "ndayslost")),
         checkClass(by, "character", null.ok = TRUE),
         checkLogical(line_overall),
         combine = "and")

  summary_data <- calc_summary(injd, by = by, overall = FALSE, quiet = T)
  col_summary_stat <- summary_stat

  if (line_overall) {
    summary_ov_data <- calc_summary(injd, by = by, overall = TRUE, quiet = T)
    summary_ov_data <- summary_ov_data |>
      dplyr::select(tidyselect::all_of(by), tidyselect::all_of(col_summary_stat))
  }

  p <- ggplot(data = summary_data,
         aes(x = forcats::fct_reorder(.data$person_id, .data[[col_summary_stat]]), y = .data[[col_summary_stat]])) +
    geom_point() +
    geom_linerange(aes(x = .data$person_id, ymin = 0, ymax = .data[[col_summary_stat]])) +
    coord_flip() +
    ylab(paste0("Case ", summary_stat)) + xlab("") +
    ggtitle(title) +
    theme_bw()

  if (!is.null(by)) {
    p <- p + facet_wrap(~.data[[by]])
  }

  if (line_overall) {
    p <- p + geom_hline(aes(yintercept = .data[[col_summary_stat]]),
                        col = "red", data = summary_ov_data)
  }

  return(p)
}
