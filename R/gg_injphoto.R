#' Plot injuries over the follow-up period
#'
#' Given an \code{injd} \strong{S3} object it plots an overview of the injuries
#' sustained by each player/athlete in the cohort during the follow-up. Each
#' subject timeline is depicted horizontally where the red cross indicates the
#' exact injury date, the blue circle the recovery date and the bold black line
#' indicates the duration of the injury (time-loss).
#'
#' @param injd Prepared data. An \code{injd} object.
#' @param title Text for the main title.
#' @param fix A logical value indicating whether to limit the range of date (x
#'           scale) to the maximum observed exposure date or not to limit the x
#'           scale, regardless some recovery dates might be longer than the
#'           maximum observed exposure date.
#' @param by_date increment of the date sequence at which x-axis tick-marks are
#'   to drawn. An argument to be passed to
#'   \code{\link[base:seq.Date]{base::seq.Date()}}.
#'
#'
#' @return A ggplot object (to which optionally more layers can be added).
#' @export
#'
#' @import ggplot2
#' @importFrom checkmate assert checkClass
#'
#' @examples
#' # Prepare data
#' # df_exposures <- prepare_exp(raw_df_exposures, player = "player_name",
#' #                             date = "year", time_expo = "minutes_played")
#' # df_injuries  <- prepare_inj(raw_df_injuries, player = "player_name",
#' #                             date_injured = "from", date_recovered = "until")
#' # injd         <- prepare_all(data_exposures = df_exposures,
#' #                             data_injuries  = df_injuries,
#' #                             exp_unit = "minutes")
#'
#' gg_injphoto(injd, title = "Injury Overview", by_date = "1 years")
gg_injphoto <- function(injd, title = NULL, fix = FALSE, by_date = "1 months") {

  ## check inputs
  assert(checkClass(injd, "injd"))

  ## extract necessary data
  data_followup <- attr(injd, "follow_up")
  data_injuries <- attr(injd, "data_injuries")

  data_injuries_long <- data_injurieslong(data_injuries)

  ## if data_injuries_long$date >= data_followup$tf
  if (fix) {
    idx <- which(data_injuries_long$date >= max(data_followup$tf))
    data_injuries_long$event <- as.character(data_injuries_long$event)
    data_injuries_long$event[idx] <- "still_injured"
    data_injuries_long$event <- factor(data_injuries_long$event)
    data_injuries_long$date[idx] <- max(data_followup$tf)

    idx <- which(data_injuries$date_recovered >= max(data_followup$tf))
    data_injuries$date_recovered[idx] <- max(data_followup$tf)

    values_shape <-  c(4, 1, -10) ## -10 = no shape
    labels_shape <- c("Injured date", "Recovered date", "")
    values_color <-  c("red", "blue", "white")
    labels_color <-  c("Injured date", "Recovered date", "")
  } else {
    values_shape <- c(4, 1)
    labels_shape <-  c("Injured date", "Recovered date")
    values_color <- c("red", "blue")
    labels_color <-  c("Injured date", "Recovered date")
  }

  p1 <- ggplot() +
    geom_segment(data = data_followup, aes(x = .data$t0, xend = .data$tf,
                                           y = .data$player, yend = .data$player)) +
    geom_segment(data = data_injuries, aes(x = .data$date_injured, xend = .data$date_recovered,
                                           y = .data$player, yend = .data$player), linewidth = 1) +
    geom_point(data = data_injuries_long, aes(x = .data$date, y = .data$player,
                                              shape = .data$event, color = .data$event), size = 5) +
    xlab(NULL) + ylab(NULL) + ggtitle(title) +
    theme_bw()

  times <- seq.Date(from = as.Date(min(data_followup$t0)), to = as.Date(max(data_followup$tf)), by = by_date)

  p2 <- p1 +
    scale_shape_manual(name = "", values = values_shape, labels = labels_shape) +
    scale_color_manual(name = "", values = values_color, labels = labels_color) +
    #scale_color_manual(values = c("blue", "red", rep("dodgerblue", 3)), labels = c("Recovered date", "Injured date", "", "", "")) + ## lo de dodgerblue es para lineas verticales?
    scale_x_date(breaks = times, date_labels = "%Y-%m-%d") +
    theme_bw()

  p2
}


