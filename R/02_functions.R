create_modelling_data <- function(data) {
  referrals <- data |>
    filter(
      type == "Referrals"
    ) |>
    distinct(
      trust,
      trust_name,
      specialty,
      period_id,
      value
    ) |>
    rename(
      referrals = "value"
    ) |>
    tidyr::nest(
      referrals_data = c(
        period_id,
        referrals
      )
    )

  completes <- data |>
    filter(
      type == "Complete"
    ) |>
    distinct(
      trust,
      trust_name,
      specialty,
      period_id,
      months_waited_id,
      value
    ) |>
    rename(
      treatments = "value"
    ) |>
    tidyr::nest(
      completes_data = c(
        period_id,
        months_waited_id,
        treatments
      )
    )


  incompletes <- data |>
    filter(
      type == "Incomplete"
    ) |>
    distinct(
      trust,
      trust_name,
      specialty,
      period_id,
      months_waited_id,
      value
    ) |>
    rename(
      incompletes = "value"
    ) |>
    tidyr::nest(
      incompletes_data = c(
        period_id,
        months_waited_id,
        incompletes
      )
    )

  all_calibration_data <- completes |>
    left_join(
      referrals,
      by = join_by(
        trust, trust_name, specialty
      )
    ) |>
    left_join(
      incompletes,
      by = join_by(
        trust, trust_name, specialty
      )
    )

  return(all_calibration_data)

}


reorder_vector <- function(x, vals) {
  first_part <- x[!(x %in% vals)]

  reordered <- c(first_part, vals)

  return(reordered)
}



# include a proper rounding function
# from here: https://stackoverflow.com/questions/12688717/round-up-from-5
round2 = function(x, digits) {
  posneg = sign(x)
  z = abs(x)*10^digits
  z = z + 0.5 + sqrt(.Machine$double.eps)
  z = trunc(z)
  z = z/10^digits
  z*posneg
}



#' Forecast n period based on a selected method
#'
#' @param rtt_table tibble containing a record for each period, and a value
#'   column
#' @param number_timesteps integer; number of time steps to forecast
#' @param method string; "tbats" or "linear"
#' @param percent_change numeric vector; if method is "linear" the percentage
#'   uplift required by the final time step relative to the extrapolated first
#'   time step (where 1 is a 1% uplift). The number of items determines the
#'   number of columns in the final tibble
#'
forecast_function <- function(rtt_table, number_timesteps, method, percent_change) {
  if (method == "tbats") {
    fcast <- rtt_table |>
      pull(value) |>
      ts(frequency = 12) |>
      forecast::tbats() |>
      forecast::forecast(h = number_timesteps) |>
      tidyr::as_tibble() |>
      select(
        Expected_referrals = "Point Forecast",
        Low_referrals = "Lo 80",
        High_referrals = "Hi 80"
      ) |>
      mutate(
        period_id = dplyr::row_number()
      )
  } else if (method == "linear") {
    # first, calculate the value for the first time step as either a linear
    # extrapolation of the data provided (if significant) or a mean (if linear
    # model is not significant)

    first_period_id <- max(rtt_table[["period_id"]])# + 1

    first_val <- rtt_table |>
      select(period_id, value) |>
      arrange(period_id) %>%
      summarise(
        mean_val = mean(value),
        lm_fit = list(
          lm(value ~ period_id, data = .)
        ),
        pval = broom::tidy(lm_fit[[1]]) |>
          filter(term == "period_id") |>
          pull(p.value),
        lm_val = list(
          predict(
            object = lm_fit[[1]],
            newdata = data.frame(period_id = first_period_id)
          )
        )
      ) |>
      mutate(
        # t_1_val = mean_val
        t_1_val = case_when(
          pval <= 0.05 ~ as.numeric(lm_val),
          .default = mean_val
        ),
        # capacity can't be less than zero, so it is fixed to zero if so
        t_1_val = case_when(
          t_1_val < 0 ~ 0,
          .default = t_1_val
        )
      ) |>
      pull(t_1_val)

    final_val <- first_val * (1 + (percent_change / 100))

    fcast <- tibble(
      final = final_val,
      first = first_val,
      scenario = names(percent_change)
    ) |>
      pivot_longer(
        cols = c(first, final),
        names_to = "period_id",
        values_to = "value"
      ) |>
      mutate(
        period_id = case_when(
          period_id == "first" ~ 1,
          period_id == "final" ~ number_timesteps,
          .default = NA_real_
        )
      ) |>
      summarise(
        lm_fit = list(
          lm(value ~ period_id, data = cur_data())
        ),
        .by = scenario
      ) |>
      mutate(
        project = purrr::map(
          lm_fit,
          ~ predict(
            object = .x,
            newdata = data.frame(period_id = 1:number_timesteps)
          )
        )
      )|>
      select(c("scenario", "project")) |>
      mutate(
        project = map(
          project,
          ~ tibble::enframe(.x, name = "period_id")
        )
      ) |>
      unnest(project) |>
      pivot_wider(
        names_from = scenario,
        values_from = value
      )


  }


  return(fcast)
}
