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
