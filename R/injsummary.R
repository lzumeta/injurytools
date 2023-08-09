#' Estimate injury summary statistics
#'
#' Calculate injury summary statistics such as injury incidence and injury
#' burden (see Bahr et al. 20), including total number of injuries, number of
#' days lost due to injury, total time of exposure etc., by means of a (widely
#' used) Poisson method, negative binomial, zero-inflated poisson or
#' zero-inflated negative binomial, on a player and overall basis.
#'
#' @param injd \code{injd} \strong{S3} object (see
#'   \code{\link[=prepare_all]{prepare_all()}}).
#' @param var_type_injury Character specifying the name of the column according
#'   to which compute injury summary statistics. It should refer to a
#'   (categorical) variable that describes the "type of injury". Optional,
#'   defaults to \code{NULL}.
#' @param method Method to estimate injury incidence and injury burden. One of
#'   "poisson", "negbin", "zinfpois" or "zinfnb"; characters that stand for
#'   Poisson method, negative binomial method, zero-inflated Poisson and
#'   zero-inflated negative binomial.
#' @param conf_level Confidence level (defaults to 0.95).
#' @param quiet Logical, whether or not to silence the warning messages
#'   (defaults to \code{FALSE}).
#'
#' @return A list of two data frames comprising player-wise and overall injury
#'   summary statistics, respectively, that constitute an \code{injds}
#'   \strong{S3} object. Both of them made up of the following columns:
#'   \itemize{
#'        \item \code{ninjuries}: number of injuries sustained by the player or
#'        overall in the team over the given period specified by the \code{injd}
#'        data frame.
#'        \item \code{ndayslost}: number of days lost by the player or overall
#'        in the team due to injury over the given period specified by the
#'        \code{injd} data frame.
#'        \item \code{mean_dayslost}: average of number of days lost (i.e.
#'        \code{ndayslost}) playerwise or overall in the team.
#'        \item \code{median_dayslost}: median of number of days lost (i.e.
#'        \code{ndayslost}) playerwise or overall in the team.
#'        \item \code{iqr_dayslost}: interquartile range of number of days lost
#'        (i.e. \code{ndayslost}) playerwise or overall in the team.
#'        \item \code{totalexpo}: total exposure that the player has been under risk
#'        of sustaining an injury.
#'        \item \code{injincidence}: injury incidence, number of injuries per unit of
#'        exposure.
#'        \item \code{injburden}: injury burden, number of days lost per unit of
#'        exposure.
#'        \item \code{var_type_injury}: only if it is specified as an argument to
#'        function.
#' }
#' Apart from this column names, they may further include these other columns
#' depending on the user's specifications to the function:
#' \itemize{
#'      \item \code{percent_ninjuries}: percentage (%) of number of injuries of
#'      that type relative to all types of injuries (if \code{var_type_injury}
#'      specified).
#'      \item \code{percent_dayslost}: percentage (%) of number of days lost
#'      because of injuries of that type relative to the total number of days
#'      lost because of all types of injuries (if \code{var_type_injury}
#'      specified).
#'      \item \code{injincidence_sd} and \code{injburden_sd}: estimated standard
#'      deviation, by the specified \code{method} argument, of injury incidence
#'      (\code{injincidence}) and injury burden (\code{injburden}), for the
#'      overall injury summary statistics (the 2nd element of the function
#'      output).
#'      \item \code{injincidence_lower} and \code{injburden_lower}: lower bound
#'      of, for example, 95% confidence interval (if \code{conf_level = 0.95})
#'      of injury incidence (\code{injincidence}) and injury burden
#'      (\code{injburden}), for the overall injury summary statistics (the 2nd
#'      element of the function output).
#'      \item \code{injincidence_upper} and \code{injburden_upper}: the same (as
#'      above item) applies but for the upper bound.
#' }
#' @export
#'
#' @references
#'  Bahr R., Clarsen B., & Ekstrand J. (2018). Why we should focus on the burden
#'  of injuries and illnesses, not just their incidence. \emph{British Journal of
#'  Sports Medicine}, 52(16), 1018–1021.
#'  https://doi.org/10.1136/bjsports-2017-098160
#'
#'  Waldén M., Mountjoy M., McCall A., Serner A., Massey A., Tol J. L.,
#'  ... & Andersen T. E. (2023). Football-specific extension of the IOC
#'  consensus statement: methods for recording and reporting of epidemiological
#'  data on injury and illness in sport 2020. \emph{British journal of sports
#'  medicine}.
#'
#' @importFrom rlang .data ensym
#' @importFrom dplyr filter mutate select
#' @importFrom checkmate assert checkClass assert_subset
#' @importFrom stats qnorm quantile median
#' @importFrom tidyselect everything all_of
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
#' injsummary(injd)
#' injsummary(injd, var_type_injury = "injury_type")
injsummary <- function(injd, var_type_injury = NULL,
                       method = c("poisson","negbin", "zinfpois", "zinfnb"),
                       conf_level = 0.95,
                       quiet = FALSE) {
  ## check inputs
  assert(checkClass(injd, "injd"))
  assert_subset(var_type_injury, c(names(injd), NULL))
  if (missing(method)) method <- "poisson"

  df_exposures <- attr(injd, "data_exposures")
  df_exposures_summary <- df_exposures |>
    dplyr::group_by(.data$player) |>
    dplyr::summarise(totalexpo = sum(.data$time_expo))
  totalexpo_vec <- sum(df_exposures_summary$totalexpo)

  ## if total expo = 0, omit those variables and throw a message
  if (any(df_exposures_summary$totalexpo == 0)) {
    if (!quiet) {
      warning(paste0("\n* There are players with no exposure..\n",
                     "  We assume that they have not been exposed to injury and thus,",
                     " they are omitted from this summary\n"))
    }
    quit_players <- df_exposures_summary |>
      dplyr::filter(.data$totalexpo == 0) |>
      dplyr::select("player") |>
      (\(x) x[[1]])() |>
      as.character()
    injd <- injd |>
      dplyr::filter(.data$player != quit_players) |>
      droplevels()
  }

  if(!is.null(var_type_injury)) var_type_injury <- rlang::ensym(var_type_injury)

  ## calculate summary statistics
  ## - playerwise
  injds <- injd |>
    dplyr::left_join(df_exposures_summary, by = "player") |>
    dplyr::group_by(.data$player, {{ var_type_injury }}) |>
    dplyr::summarise(ninjuries       = sum(.data$status),
                     ndayslost       = sum(.data$days_lost),
                     mean_dayslost   = mean(.data$days_lost),
                     median_dayslost = stats::median(.data$days_lost),
                     iqr_dayslost    = paste0(stats::quantile(.data$days_lost, 0.25), "-", stats::quantile(.data$days_lost, 0.75)),
                     totalexpo       = dplyr::first(.data$totalexpo),
                     injincidence    = .data$ninjuries/.data$totalexpo,
                     injburden       = .data$ndayslost/.data$totalexpo,
                     .groups = "keep") |>
    dplyr::ungroup() |>
    (\(x)
      if(!is.null(var_type_injury)) {
        filter(x, !is.na({{var_type_injury}})) |>
          tidyr::complete(.data$player, {{ var_type_injury }}) |>
          dplyr::group_by(.data$player) |>
          dplyr::mutate(totalexpo = mean(.data$totalexpo, na.rm = TRUE)) |>  ## replacing NAs with the same totalexpo values
          dplyr::mutate(iqr_dayslost = ifelse(is.na(.data$ndayslost), "0-0", .data$iqr_dayslost),
                        dplyr::across(c("ninjuries":"median_dayslost", "totalexpo":"injburden"),
                                      ~ifelse(is.na(.), 0, .))) |>
          dplyr::ungroup()
      } else {
        x
      })()

  ## - overall
  injds_overall <-  injds |>
    dplyr::select("player", "ninjuries", "ndayslost", "totalexpo", {{var_type_injury}}) |>
    dplyr::group_by({{ var_type_injury }}) |>
    dplyr::summarise(ninjuries       = sum(.data$ninjuries),
                     ndayslost       = sum(.data$ndayslost),
                     mean_dayslost   = mean(injd$days_lost),
                     median_dayslost = stats::median(injd$days_lost), ## Important: median of injd$days_lost and not .data$...
                     iqr_dayslost    = paste0(stats::quantile(injd$days_lost, 0.25), "-", stats::quantile(injd$days_lost, 0.75)), ## Important: iqr of injd$days_lost and not .data$...
                     totalexpo       = totalexpo_vec,
                     injincidence    = .data$ninjuries/.data$totalexpo,
                     injburden       = .data$ndayslost/.data$totalexpo,
                     .groups = "keep") |>
    dplyr::ungroup() |>
    (\(x)
      if(!is.null(var_type_injury)) {
        dplyr::filter(x, !is.na({{var_type_injury}})) |>
          tidyr::complete({ {var_type_injury }}) |>
          dplyr::mutate(totalexpo    = mean(.data$totalexpo, na.rm = TRUE), ## replacing NAs with the same totalexpo values
                        iqr_dayslost = ifelse(is.na(.data$ndayslost), "0-0", .data$iqr_dayslost),
                        dplyr::across(c("ninjuries":"median_dayslost", "totalexpo":"injburden"),
                                      ~ifelse(is.na(.), 0, .)),
                        percent_ninjuries = round(.data$ninjuries*100/sum(.data$ninjuries), 2),
                        percent_dayslost  = round(.data$ndayslost*100/sum(.data$ndayslost), 2)) |>
          dplyr::select(tidyselect::all_of(var_type_injury), "ninjuries", "percent_ninjuries",
                        "ndayslost", "percent_dayslost", tidyselect::everything()) # order the column names
      } else {
        x
      })()

  ## correct mean_dayslost, median_dayslost and iqr_dayslost values if var_type_injury specified
  if(!is.null(var_type_injury)) {
    injds_overall_aux <- injd |>
      dplyr::group_by({{ var_type_injury }}) |>
      dplyr::summarise(mean_dayslost   = mean(.data$days_lost, na.rm = T),
                       median_dayslost = stats::median(.data$days_lost, na.rm = T),
                       iqr_dayslost    = paste0(stats::quantile(.data$days_lost, 0.25), "-", stats::quantile(.data$days_lost, 0.75))) |>
      dplyr::ungroup()
    ## merge with injds_overall (smash these two variables)
    injds_overall <- dplyr::left_join(injds_overall, injds_overall_aux, by = names(injds_overall)[[1]]) |>
      dplyr::rename(mean_dayslost   = "mean_dayslost.y",
                    median_dayslost = "median_dayslost.y",
                    iqr_dayslost    = "iqr_dayslost.y") |>
      dplyr::select(tidyselect::all_of(var_type_injury), "ninjuries", "percent_ninjuries",
                    "ndayslost", "percent_dayslost", "mean_dayslost",
                    "median_dayslost","iqr_dayslost", tidyselect::everything(),
                    -"mean_dayslost.x", -"median_dayslost.x", -"iqr_dayslost.x")
  }


  ## method (point estimate and standard error)
  minusalphahalf <- conf_level + (1 - conf_level)/2
  if (method == "poisson") { ## assuming that the number of injuries follows a poisson and basing on CLT  ## CHECK THIS WITH pois.exact and check ggeffects package!
    injds_overall <- injds_overall |>
      dplyr::mutate(injincidence_sd = sqrt(.data$injincidence/.data$totalexpo),
                    injburden_sd    = sqrt(.data$injburden/.data$totalexpo),
                    injincidence_lower = .data$injincidence - stats::qnorm(minusalphahalf)*.data$injincidence_sd,
                    injincidence_upper = .data$injincidence + stats::qnorm(minusalphahalf)*.data$injincidence_sd,
                    injburden_lower = .data$injburden - qnorm(minusalphahalf)*.data$injburden_sd,
                    injburden_upper = .data$injburden + qnorm(minusalphahalf)*.data$injburden_sd)
  }

  ## put proper units to injury incidence and injury burden
  unit <- attr(injd, "unit_exposure")
  injds_aux <- injsummary_unit(unit, injds, quiet)
  injds <- injds_aux$injds
  injds_overall <- injsummary_unit(unit, injds_overall, quiet)[[1]]

  ## create 'injds' object
  injds <- list(playerwise = injds, overall = injds_overall)
  class(injds) <- c("injds", class(injds))
  attr(injds, "unit_exposure") <- unit
  attr(injds, "unit_timerisk") <- injds_aux$unit_timerisk
  attr(injds, "conf_level")    <- conf_level

  return(injds)
}

#' Transform injsummary() output according to the unit of exposure
#'
#' @param unit Character that indicates the unit of exposure of the sports
#'   injury data.
#' @param injds \code{injds} data frame to be transformed.
#' @param quiet Logical, whether or not to silence the warning messages
#'   (defaults to \code{FALSE}).
#'
#' @return A list of two elements:
#'   (i) same injds data frame with 'injincidence'
#'   and 'injburden' values transformed according to \code{unit}, named
#'   \code{injds} and
#'   (ii) a character vector that expresses the unit used for the rates, i.e.
#'   for the player's time at risk, named \code{unit_timerisk}.
#'
#' @keywords internal
injsummary_unit <- function(unit, injds, quiet) {
  if(unit %in% c("minutes", "hours")) {
    if (!quiet) {
      warning(paste0("\n Exposure time unit is ", unit,
                     "\n  So... Injury incidence and injury burden are calculated per 1000h of player-exposure\n\n"))
    }
    if (unit == "minutes") {
      injds <- injds |>
        dplyr::mutate(dplyr::across(.data$injincidence:dplyr::last_col(), ~ .x*60*1000))
    } else {
      injds <- injds |>
        dplyr::mutate(dplyr::across(.data$injincidence:dplyr::last_col(), ~ .x*1000))
    }
    unit_timerisk <- "1000h player-exposure"
  } else if (unit == "matches_minutes") {
    if (!quiet) {
      warning(paste0("\n  Exposure time unit is ", unit,
                     "\n  So... Injury incidence and injury burden are calculated per 100 player-matches of exposure (90 minutes times 100)\n\n"))
    }
    injds <- injds |>
      dplyr::mutate(dplyr::across("injincidence":dplyr::last_col(), ~ .x*90*100))
    unit_timerisk <- "100 player-match"
  } else if (unit %in% c("matches_num", "days", "activity_days")) {
    if (!quiet) {
      warning(paste0("\n  Exposure time unit is ", unit,
                     "\n  So... Injury incidence and injury burden are calculated per 100 of ", unit, " of exposure\n\n"))
    }
    injds <- injds |>
      dplyr::mutate(dplyr::across(.data$injincidence:dplyr::last_col(), ~ .x*100))
    unit <- dplyr::case_when(unit == "matches_num" ~ "matches",
                             unit == "days" ~ "days",
                             unit == "activity_days" ~ "activity days")
    unit_timerisk <- paste0("100 player-", unit)
  } else {
    unit_timerisk <- "player-seasons"
  }

  res <- list(injds = injds, unit_timerisk = unit_timerisk)
  return(res)
}

#' Check if an object is of class \code{injds}
#'
#' Check if an object x is of class \code{injds}.
#'
#' @param x any R object.
#' @return A logical value: \code{TRUE} if \code{x} inherits from \code{injds}
#'   class, \code{FALSE} otherwise.
#' @export
is_injds <- function(x) inherits(x, "injds")
