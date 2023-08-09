#' Plot polar area diagrams representing available/injured players proportions
#'
#' Plot the proportions of available and injured players in the cohort, on a
#' monthly or season basis, by a polar area diagram. Further information on the
#' type of injury may be specified so that the injured players proportions are
#' disaggregated and reported according to this variable.
#'
#' @param injd Prepared data, an \code{injd} object.
#' @param by Character, one of "monthly" or "season", specifying the periodicity
#'   according to which to calculate the proportions of available and injured
#'   players/athletes.
#' @param var_type_injury Character specifying the name of the column on the
#'   basis of which to classify the injuries and calculate proportions of the
#'   injured players. It should refer to a (categorical) variable that describes
#'   the "type of injury". Defaults to \code{NULL}.
#' @param title Text for the main title.
#'
#' @return  A ggplot object (to which optionally more layers can be added).
#' @export
#'
#' @import ggplot2
#' @importFrom checkmate assert checkClass
#' @importFrom stats relevel
#' @importFrom purrr map_dfr
#' @importFrom lubridate month year
#' @importFrom stringr str_to_title
#' @importFrom withr with_locale
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
#' library(ggplot2)
#' our_palette <- c("seagreen3", "red3", rev(RColorBrewer::brewer.pal(5, "Reds")))
#' gg_injprev_polar(injd, by = "monthly", var_type_injury = "injury_type",
#'             title = "Polar area diagram\ninjured and available (healthy) players per month") +
#'   scale_fill_manual(values = our_palette)
#' gg_injprev_polar(injd, by = "monthly",
#'             title = "Polar area diagram\ninjured and available (healthy) players per month") +
#'   scale_fill_manual(values = our_palette)
#'}
gg_injprev_polar <- function(injd,
                        by = c("monthly", "season"),
                        var_type_injury = NULL,
                        title = "Polar area diagram\ninjured and available (healthy) players") {

  assert(checkClass(injd, "injd"))

  df_polar <- injprev(injd, by, var_type_injury)



  if (by == "monthly") {
    p <- df_polar |>
      dplyr::arrange(-.data$prop) |>
      ggplot(aes(x = month, y = .data$prop)) +
      geom_bar(aes(fill = .data$type_injury),
               width = 1, stat = "identity", colour = "grey",
               linewidth = 0.3, position = position_identity(), alpha = 0.75) +
      coord_polar() +
      facet_wrap(~season) +
      # scale_fill_manual(values = our_palette[seq_len(nlevels(df_polar$type_injury))]) + ## change this!!
      scale_y_sqrt() +
      ylab("") + xlab("") +
      ggtitle(title) +
      theme_bw()
  }

  if (by == "season") {
    p <- df_polar |>
      dplyr::arrange(-.data$prop) |>
      ggplot(aes(x = .data$season, y = .data$prop)) +
      geom_bar(aes(fill = .data$type_injury),
               width = 1, stat = "identity", colour = "grey",
               linewidth = 0.3, position = position_identity(), alpha = 0.75) +
      coord_polar() +
      # scale_fill_manual(values = our_palette[seq_len(nlevels(df_polar$type_injury))]) + ## change this!!
      scale_y_sqrt() +
      ylab("") + xlab("") +
      ggtitle(title) +
      theme_bw()
  }

  return(p)
}


