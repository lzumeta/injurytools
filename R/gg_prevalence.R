#' Plot polar area diagrams representing players' prevalence
#'
#' Plot the proportions of available and injured players in the cohort, on a
#' monthly or season basis, by a polar area diagram. Further information on the
#' type of injury may be specified so that the injured players proportions are
#' disaggregated and reported according to this variable.
#'
#' @param injd Prepared data, an \code{injd} object.
#' @param time_period Character. One of "monthly" or "season", specifying the
#'   periodicity according to which to calculate the proportions of available
#'   and injured athletes.
#' @param by Character specifying the name of the column on the basis of which
#'   to classify the injuries and calculate proportions of the injured athletes.
#'   Defaults to \code{NULL}.
#' @param line_mean TOWRITE!!!
#' @param title Text for the main title.
#'
#' @return  A ggplot object (to which optionally more layers can be added).
#' @export
#'
#' @import ggplot2
#' @importFrom checkmate assert checkClass
#' @importFrom dplyr filter
#' @importFrom scales percent_format
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
#' library(ggplot2)
#' our_palette <- c("red3", rev(RColorBrewer::brewer.pal(5, "Reds")), "seagreen3")
#' gg_prevalence(injd, time_period = "monthly", by = "injury_type",
#'               title = "Monthly prevalence of each type of sports injury") +
#'   scale_fill_manual(values = our_palette)
#' gg_prevalence(injd, time_period = "monthly",
#'               title = "Monthly prevalence of sports injuries") +
#'   scale_fill_manual(values = our_palette)
#'}
gg_prevalence <- function(injd,
                          time_period = c("monthly", "season"),
                          by = NULL,
                          line_mean = FALSE,
                          title = NULL) {

  assert(checkClass(injd, "injd"),
         checkChoice(time_period, c("monthly", "season")),
         checkClass(by, "character", null.ok = TRUE),
         checkClass(title, "character", null.ok = TRUE),
         combine = "and")

  df_prev <- calc_prevalence(injd, time_period, by) |>
    dplyr::filter(.data$status != "Available")

  if (line_mean) {
    if (!is.null(by)) {
      if (time_period == "monthly") tmp <- rlang::sym("month") else tmp <- NULL
      df_prev_aux <- df_prev |>
        group_by(.data$season, {{ tmp }}) |>
        summarize(prop = sum(.data$prop)) |>
        ungroup()
    } else {
      df_prev_aux <- df_prev
    }
    if (time_period == "monthly") tmp <- rlang::sym("season") else tmp <- NULL
    df_line_mean <- df_prev_aux |>
      group_by({{ tmp }}) |>
      summarize(line_mean = mean(.data$prop))
    if (!is.null(tmp)) {
      df_prev <- dplyr::left_join(df_prev, df_line_mean, by = "season")
    } else {
      df_prev <- cbind(df_prev, df_line_mean)
    }
  }

  if (!line_mean) df_prev_aux <- df_prev

  ymax <- pmax(100, max(df_prev_aux$prop))
  if (ymax > 100) {
    warning("Overall stacked prevalence >100% because there are athletes
            contributing to more than one case-specific prevalence.")
  }

  if (time_period == "monthly") {
    p <- df_prev |>
      ggplot(aes(x = month, y = .data$prop)) +
      geom_bar(aes(fill = .data$status),
               width = 0.8, stat = "identity", colour = "grey",
               linewidth = 0.3, alpha = 0.75) +
      {if(line_mean) geom_hline(aes(yintercept = line_mean), col = "red")} +
      facet_wrap(~season) +
      scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, ymax)) +
      # scale_fill_manual(values = our_palette[seq_len(nlevels(df_prev$status))]) + ## change this!!
      ylab("") + xlab("") +
      ggtitle(title) +
      theme_bw() +
      theme(legend.title = element_blank())
  }

  if (time_period == "season") {
    p <- df_prev |>
      ggplot(aes(x = .data$season, y = .data$prop)) +
      geom_bar(aes(fill = .data$status),
               width = 0.8, stat = "identity", colour = "grey",
               linewidth = 0.3, alpha = 0.75) +
      {if(line_mean) geom_hline(aes(yintercept = line_mean), col = "red")} +
      # scale_fill_manual(values = our_palette[seq_len(nlevels(df_prev$status))]) + ## change this!!
      scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, ymax)) +
      ylab("") + xlab("") +
      ggtitle(title) +
      theme_bw() +
      theme(legend.title = element_blank())
  }

  return(p)
}


