#' Injury summary statistics
#'
#' Calculate injury summary statistics such as injury incidence and injury
#' burden (REF-ak), including total number of injuries, number of days lost due
#' to injury, total time of exposure etc., by means of a classic-standard
#' (Poisson) method, negative binomial, zero-inflated poisson or zero-inflated
#' negative binomial, on a player and overall basis.
#'
#' @param injd \code{injd} \strong{S3} object (see \code{\link[=prepare_all]{prepare_all()}}).
#' @param var_type_injury Character specifying the name of the column according
#'   to which compute injury summary statistics. It should refer to a
#'   (categorical) variable that describes the "type of injury". Optional,
#'   defaults to \code{NULL}.
#' @param method Method to estimate injury incidence and injury burden. One of
#'   "poisson", "negbin", "zinfpois" or "zinfnb"; characters that stand for
#'   Poisson method, negative binomial method, zero-inflated Poisson and
#'   zero-inflated negative binomial.
#' @param conf_level Confidence level (default = 0.95).
#' @param quiet Logical, whether or not to silence the warning messages
#'   (defaults to \code{FALSE}).
#'
#' @return A list of two data frames comprising player-wise and overall
#'   injury summary statistics, respectively, that constitute an \code{injds}
#'   \strong{S3} object, and both of them made up of the following columns:
#'   \itemize{
#'        \item \code{ninjuries}: number of injuries sustained by the player over the
#'        given period specified by the \code{injd} data frame.
#'        \item \code{ndayslost}: number of days lost by the player due to injury
#'        over the given period specified by the \code{injd} data frame.
#'        \item \code{totalexpo}: total exposure that the player has been under risk
#'        of sustaining an injury.
#'        \item \code{injincidence}: injury incidence, number of injuries per unit of
#'        exposure.
#'        \item \code{injburden}: injury burden, number of days lost per unit of
#'        exposure.
#'        \item \code{var_type_injury}: only if it is specified as an argument to
#'        function.
#'  }
#' @export
#'
#' @importFrom rlang .data
#' @importFrom dplyr filter
#' @importFrom checkmate assert checkClass assert_subset
#' @importFrom stats qnorm
#'
#' @examples
#' # df_exposures <- prepare_exp(raw_df_exposures, player = "player_name",
#' #                             date = "year", time_expo = "minutes_played")
#' # df_injuries  <- prepare_inj(raw_df_injuries, player = "player_name",
#' #                             date_injured = "from", date_recovered = "until")
#' # injd         <- prepare_all(data_exposures = df_exposures,
#' #                             data_injuries  = df_injuries,
#' #                             exp_unit = "matches_minutes")
#'
#' injsummary(injd)
injsummary <- function(injd, var_type_injury = NULL,
                       method = c("poisson","negbin", "zinfpois", "zinfnb"),
                       conf_level = 0.95,
                       quiet = FALSE) {
  ## check inputs
  assert(checkClass(injd, "injd"))
  assert_subset(var_type_injury, c(names(injd), NULL))
  if (missing(method)) method <- "poisson"

  df_exposures <- attr(injd, "data_exposures")
  df_exposures_summary <- df_exposures %>%
    dplyr::group_by(.data$player) %>%
    dplyr::summarise(totalexpo = sum(.data$time_expo))

  ## if total expo = 0, omit those variables and throw a message
  if (any(df_exposures_summary$totalexpo == 0)) {
    if (!quiet) {
      warning(paste0("\n* There are players with no exposure..\n",
                     "  We assume that they have not been exposed to injury and thus,",
                     " they are omitted from this summary\n"))
    }
    quit_players <- df_exposures_summary %>%
      dplyr::filter(.data$totalexpo == 0) %>%
      select(.data$player) %>%
      dplyr::pull() %>%
      as.character()
    injd <- injd %>%
      dplyr::filter(.data$player != quit_players) %>%
      droplevels()
  }

  if(!is.null(var_type_injury)) var_type_injury <- rlang::ensym(var_type_injury)

  ## calculate summary statistics
  injds <- injd %>%
    dplyr::left_join(df_exposures_summary, by = "player") %>%
    dplyr::group_by(.data$player, {{ var_type_injury }}) %>%
    dplyr::summarise(ninjuries = sum(.data$status),
                     ndayslost = sum(.data$days_lost),
                     totalexpo = dplyr::first(.data$totalexpo),
                     injincidence = .data$ninjuries/.data$totalexpo,
                     injburden = .data$ndayslost/.data$totalexpo,
                     .groups = "keep") %>%
    dplyr::ungroup() %>%
    {
      if(!is.null(var_type_injury)) {
        filter(., !is.na({{var_type_injury}})) %>%
          tidyr::complete(.data$player, {{ var_type_injury }}) %>%
          dplyr::group_by(.data$player) %>%
          dplyr::mutate(totalexpo = mean(.data$totalexpo, na.rm = TRUE)) %>%  ## replacing NAs with the same totalexpo values
          dplyr::mutate(dplyr::across(.data$ninjuries:.data$injburden, ~ifelse(is.na(.), 0, .))) %>%
          dplyr::ungroup()
      } else .
    }

  injds_overall <-  injds %>%
    dplyr::select(.data$player, .data$ninjuries, .data$ndayslost, .data$totalexpo, {{var_type_injury}}) %>%
    dplyr::group_by({{ var_type_injury }}) %>%
    dplyr::summarise(ninjuries = sum(.data$ninjuries),
                     ndayslost = sum(.data$ndayslost),
                     totalexpo = sum(.data$totalexpo),
                     injincidence = .data$ninjuries/.data$totalexpo,
                     injburden = .data$ndayslost/.data$totalexpo,
                     .groups = "keep") %>%
    dplyr::ungroup() %>%
    {
      if(!is.null(var_type_injury)) {
        filter(., !is.na({{var_type_injury}})) %>%
          tidyr::complete({ {var_type_injury }}) %>%
          dplyr::mutate(totalexpo = mean(.data$totalexpo, na.rm = TRUE)) %>%  ## replacing NAs with the same totalexpo values
          dplyr::mutate(dplyr::across(.data$ninjuries:.data$injburden, ~ifelse(is.na(.), 0, .)))
      } else .
    }

  ## method (point estimate and standard error)
  if (method == "poisson") { ## assuming that the number of injuries follows a poisson and basing on CLT  ## CHECK THIS WITH pois.exact and check ggeffects package!
    injds_overall <- injds_overall %>%
      dplyr::mutate(injincidence_sd = sqrt(.data$injincidence/.data$totalexpo),
                    injburden_sd = sqrt(.data$injburden/.data$totalexpo),
                    injincidence_lower = .data$injincidence - qnorm(conf_level)*.data$injincidence_sd,
                    injincidence_upper = .data$injincidence + qnorm(conf_level)*.data$injincidence_sd,
                    injburden_lower = .data$injburden - qnorm(conf_level)*.data$injburden_sd,
                    injburden_upper = .data$injburden + qnorm(conf_level)*.data$injburden_sd)
  }

  ## fix-transform injury incidence and injury burden units
  unit <- attr(injd, "unit_exposure")
  injds_aux <- injsummary_unit(unit, injds, quiet)
  injds <- injds_aux$injds
  injds_overall <- injsummary_unit(unit, injds_overall, quiet)[[1]]

  ## create object
  injds <- list(playerwise = injds, overall = injds_overall)
  class(injds) <- c("injds", class(injds))
  attr(injds, "unit_exposure") <- unit
  attr(injds, "unit_timerisk") <- injds_aux$unit_timerisk
  attr(injd, "conf_level") <- conf_level

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
#' @return A list of two elements: (i) same injds data frame with 'injincidence'
#'   and 'injburden' values transformed according to \code{unit}, named
#'   \code{injds} and (ii) a character vector that expresses the statistic used
#'   for the rates (the unit used for the player's time at risk).
#'
#' @keywords internal
injsummary_unit <- function(unit, injds, quiet) {
  if(unit %in% c("minutes", "hours")) {
    if (!quiet) {
      warning(paste0("\n* Exposure time unit is ", unit,
                     "\n  So... Injury incidence and injury burden are calculated per 1000h of player-exposure\n\n"))
    }
    if (unit == "minutes") {
      injds <- injds %>%
        dplyr::mutate(dplyr::across(.data$injincidence:dplyr::last_col(), ~ .x*60*1000))
    } else {
      injds <- injds %>%
        dplyr::mutate(dplyr::across(.data$injincidence:dplyr::last_col(), ~ .x*1000))
    }
    unit_timerisk <- "1000h player-exposure"
  } else if (unit == "matches_minutes") {
    if (!quiet) {
      warning(paste0("\n* Exposure time unit is ", unit,
                     "\n  So... Injury incidence and injury burden are calculated per 100 player-matches of exposure (90 minutes times 100)\n\n"))
    }
    injds <- injds %>%
      dplyr::mutate(dplyr::across(.data$injincidence:dplyr::last_col(), ~ .x*90*100))
    unit_timerisk <- "100 player-match"
  } else if (unit %in% c("matches_num", "days", "activity_days")) {
    if (!quiet) {
      warning(paste0("\n* Exposure time unit is ", unit,
                     "\n  So... Injury incidence and injury burden are calculated per 100 of ", unit, " of exposure\n\n"))
    }
    injds <- injds %>%
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

#' Check if object is of class \code{injds}
#'
#' Check if an object x is of class \code{injds}.
#'
#' @param x any R object.
#' @export
is_injds <- function(x) inherits(x, "injds")
