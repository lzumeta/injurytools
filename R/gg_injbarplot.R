#' Plot player's injury incidence/burden ranking
#'
#' A bar chart that shows player-wise injury summary statistics, either injury
#' incidence or injury burden, ranked in descending order.
#'
#' @param injds \code{injds} \strong{S3} object (see
#'   \code{\link[=injsummary]{injsummary()}}).
#' @param type A character value indicating whether to plot injury incidence's
#'             or injury burden's ranking. One of "incidence" or "burden",
#'             respectively.
#' @param title Text for the main title.
#'
#' @return A ggplot object (to which optionally more layers can be added).
#' @export
#'
#' @importFrom checkmate assert checkClass checkChoice
#' @importFrom forcats fct_reorder
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
#'}
#'
#' injds <- injsummary(injd)
#'
#' p1 <- gg_injbarplot(injds, type = "incidence",
#'                     title = "Overall injury incidence per player")
#' p2 <- gg_injbarplot(injds, type = "burden",
#'                     title = "Overall injury burden per player")
#'
#' # install.packages("gridExtra")
#' # library(gridExtra)
#' if (require("gridExtra")) {
#'   gridExtra::grid.arrange(p1, p2, nrow = 1)
#' }
gg_injbarplot <- function(injds, type = c("incidence", "burden"), title = NULL) {

  ## check inputs
  if (missing(type)) type <- "incidence"
  assert(checkClass(injds, "injds"),
         checkChoice(type, c(missing(type), "incidence", "burden")),
         combine = "and")

  injds <- injds[[1]]
  col_type <- dplyr::if_else(type == "incidence", "injincidence", "injburden")

  ggplot(data = injds,
         aes(x = forcats::fct_reorder(.data$player, .data[[col_type]]), y = .data[[col_type]])) +
    geom_point() +
    geom_linerange(aes(x = .data$player, ymin = 0, ymax = .data[[col_type]])) +
    coord_flip() +
    ylab(paste0("Injury ", type)) + xlab("") +
    ggtitle(title) +
    theme_bw()
}
