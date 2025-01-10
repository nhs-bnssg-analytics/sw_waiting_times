source("R/00_libraries.R")

# configuration

calibration_start <- as.Date("2022-12-01")
calibration_end <- as.Date("2024-11-30")

# validate on the 6 months prior to the calibration period
validation_end <- calibration_start - 1
validation_start <- lubridate::floor_date(
  validation_end, unit = "months"
) %m-% months(6)

prediction_start <- calibration_end + 1
prediction_end <- as.Date("2026-03-31")


max_months_waited <- 4 # I am only interested in waiting time bins up to 4 months

trust_lkp <- tibble::tribble(
  ~trust_code,                                                 ~trust_name,
  "R0D",             "UNIVERSITY HOSPITALS DORSET NHS FOUNDATION TRUST",
  "RA7", "UNIVERSITY HOSPITALS BRISTOL AND WESTON NHS FOUNDATION TRUST",
  "RA9",                  "TORBAY AND SOUTH DEVON NHS FOUNDATION TRUST",
  "RBD",                  "DORSET COUNTY HOSPITAL NHS FOUNDATION TRUST",
  "RD1",             "ROYAL UNITED HOSPITALS BATH NHS FOUNDATION TRUST",
  "REF",                           "ROYAL CORNWALL HOSPITALS NHS TRUST",
  "RH5",                                "SOMERSET NHS FOUNDATION TRUST",
  "RH8",       "ROYAL DEVON UNIVERSITY HEALTHCARE NHS FOUNDATION TRUST",
  "RJ8",                    "CORNWALL PARTNERSHIP NHS FOUNDATION TRUST",
  "RK9",                      "UNIVERSITY HOSPITALS PLYMOUTH NHS TRUST",
  "RN3",                 "GREAT WESTERN HOSPITALS NHS FOUNDATION TRUST",
  "RNZ",                               "SALISBURY NHS FOUNDATION TRUST",
  "RTE",               "GLOUCESTERSHIRE HOSPITALS NHS FOUNDATION TRUST",
  "RVJ",                                      "NORTH BRISTOL NHS TRUST"
)


treatment_function_codes <- c(
  "(:?C_|[INA]P)?100" = "General Surgery",
  "(:?C_|[INA]P)?101" = "Urology",
  "(:?C_|[INA]P)?110" = "Trauma and Orthopaedic",
  "(:?C_|[INA]P)?120" = "Ear Nose and Throat",
  "(:?C_|[INA]P)?130" = "Ophthalmology",
  "(:?C_|[INA]P)?140" = "Oral Surgery",
  "(:?C_|[INA]P)?150" ="Neurosurgical",
  "(:?C_|[INA]P)?160" = "Plastic Surgery",
  "(:?C_|[INA]P)?170" = "Cardiothoracic Surgery",
  "C_300" = "General Internal Medicine",
  "(:?C_|[INA]P)?301" = "Gastroenterology",
  "(:?C_|[INA]P)?320" = "Cardiology",
  "(:?C_|[INA]P)?330" = "Dermatology",
  "(:?C_|[INA]P)?340" = "Respiratory Medicine",
  "(:?C_|[INA]P)?400" = "Neurology",
  "(:?C_|[INA]P)?410" = "Rheumatology",
  "(:?C_|[INA]P)?430" = "Elderly Medicine",
  "(:?C_|[INA]P)?502" = "Gynaecology",
  "X0[1-6]" = "Other",
  "C_999" = "Total"
)



# download data -----------------------------------------------------------

all_complete <- NHSRtt::get_rtt_data(
  type = "complete",
  date_start = validation_start,
  date_end = calibration_end,
  show_progress = TRUE
)

all_incomplete <- NHSRtt::get_rtt_data(
  type = "incomplete",
  date_start = validation_start,
  date_end = calibration_end,
  show_progress = TRUE
)

all_referral <- NHSRtt::get_rtt_data(
  type = "referral",
  date_start = validation_start,
  date_end = calibration_end,
  show_progress = TRUE
)


# aggregate and manipulate ------------------------------------------------

monthly_rtt <- dplyr::bind_rows(
  all_complete,
  all_incomplete,
  all_referral
) |>
  inner_join(
    trust_lkp,
    by = join_by(
      trust == trust_code
    )
  ) |>
  mutate(
    type = case_when(
      type == "Non-Admitted" ~ "Complete",
      type == "Admitted" ~ "Complete",
      type == "New Periods" ~ "Referrals",
      .default = type
    ),
    months_waited_id = convert_months_waited_to_id(
      months_waited,
      max_months_waited
    ),
    specialty = stringr::str_replace_all(
      specialty,
      treatment_function_codes
    )
  ) |>
  summarise(
    value = sum(value),
    .by = c(
      trust,
      trust_name,
      specialty,
      period,
      type,
      months_waited_id
    )
  )

# calculate SW values
monthly_rtt_sw <- monthly_rtt |>
  summarise(
    value = sum(value),
    .by = c(
      specialty,
      period,
      type,
      months_waited_id
    )
  ) |>
  mutate(
    trust = "Y58",
    trust_name = "SW Total"
  )

monthly_rtt <- bind_rows(
  monthly_rtt,
  monthly_rtt_sw
  ) |>
  arrange(
    trust,
    trust_name,
    specialty,
    type,
    months_waited_id,
    period
  ) |>
  mutate(
    period_id = dplyr::row_number(), # we need period_id for later steps
    .by = c(
      trust,
      trust_name,
      specialty,
      type,
      months_waited_id
    )
  )


calibration_period <- monthly_rtt |>
  filter(
    between(
      period,
      calibration_start,
      calibration_end
    )
  ) |>
  summarise(
    value = sum(value),
    .by = c(
      trust,
      trust_name,
      specialty,
      period_id,
      type,
      months_waited_id
    )
  ) |>
  dplyr::select(
    trust,
    trust_name,
    specialty,
    period_id,
    type,
    months_waited_id,
    value
  )

# identify combinations of trusts/specialties that have at least 1 completed
# pathway per month waited (within the calibration period)
combinations_to_remove <- calibration_period |>
  filter(type == "Complete") |>
  summarise(
    count_of_0 = sum(value == 0),
    .by = c(
      trust, specialty, months_waited_id
    )
  ) |>
  filter(count_of_0 != 0) |>
  distinct(
    trust, specialty
  )


referrals <- calibration_period |>
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

completes <- calibration_period |>
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


incompletes <- calibration_period |>
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
  ) |>
  anti_join(
    combinations_to_remove,
    by = join_by(
      trust, specialty
    )
  )
# calculating model parameters --------------------------------------------

params <- all_calibration_data |>
  mutate(
    params = purrr::pmap(
      .l = list(
        referrals_data,
        completes_data,
        incompletes_data
      ),
      .f = \(ref, comp, incomp) calibrate_capacity_renege_params(
        referrals = ref,
        completes = comp,
        incompletes = incomp,
        max_months_waited = max_months_waited,
        redistribute_m0_reneges = TRUE,
        full_breakdown = FALSE # this can be set to TRUE to see all the transitions for all months waited at each time step
      )
    )
  ) |>
  select(
    trust, trust_name, specialty, params
  )


# validating models -------------------------------------------------------

# set up validation data
validation_period <- monthly_rtt |>
  filter(
    between(
      period,
      validation_start,
      validation_end
    )
  ) |>
  dplyr::select(
    trust,
    trust_name,
    specialty,
    period_id,
    type,
    months_waited_id,
    value
  )

validation_referrals <- validation_period |>
  filter(
    type == "Referrals",
    period_id != min(period_id)
  ) |>
  distinct(
    trust,
    trust_name,
    specialty,
    period_id,
    value
  ) |>
  arrange(
    trust,
    trust_name,
    specialty,
    period_id
  ) |>
  select(!c("period_id")) |>
  tidyr::nest(
    referrals = value
  )

validation_capacity <- validation_period |>
  filter(
    type == "Complete",
    period_id != min(period_id)
  ) |>
  summarise(
    count = sum(value),
    .by = c(
      trust,
      trust_name,
      specialty,
      period_id
    )
  ) |>
  arrange(
    trust,
    trust_name,
    specialty,
    period_id
  ) |>
  select(!c("period_id")) |>
  tidyr::nest(
    completes = count
  )

incompletes_at_t0 <- validation_period |>
  filter(
    type == "Incomplete",
    period_id == min(period_id)
  ) |>
  distinct(
    trust,
    trust_name,
    specialty,
    months_waited_id,
    value
  ) |>
  rename(incompletes = value) |>
  tidyr::nest(
    incompletes = c(months_waited_id, incompletes)
  )

# combine validation period data

all_validation_data <- validation_capacity |>
  left_join(
    validation_referrals,
    by = join_by(trust, trust_name, specialty)
  ) |>
  left_join(
    incompletes_at_t0,
    by = join_by(trust, trust_name, specialty)
  ) |>
  right_join(
    params,
    by = join_by(trust, trust_name, specialty)
  )

# apply parameters to validation period
validation_waiting_times <- all_validation_data |>
  mutate(
    waiting_times = purrr::pmap(
      .l = list(
        completes,
        referrals,
        incompletes,
        params
      ),
      .f = \(cap_proj, ref_proj, incomp_t0, params) apply_params_to_projections(
        capacity_projections = cap_proj |> pull(count),
        referrals_projections = ref_proj |> pull(value),
        incomplete_pathways = incomp_t0,
        renege_capacity_params = params,
        max_months_waited = max_months_waited
      ) |>
        select(
          period_id,
          months_waited_id,
          estimated_incompletes = incompletes
        )
    )
  ) |>
  select(
    trust,
    trust_name,
    specialty,
    waiting_times
  ) |>
  tidyr::unnest(
    waiting_times
  ) |>
  mutate(
    period_id = period_id + 1 # this aligns period_id with the id in the validation_period data
  )

validation_error <- validation_period |>
  filter(
    type == "Incomplete"
  ) |>
  inner_join(
    validation_waiting_times,
    by = join_by(
      trust,
      trust_name,
      specialty,
      period_id,
      months_waited_id
    )
  ) |>
  rename(
    predicted = estimated_incompletes,
    observed = value
  ) |>
  mutate(
    absolute_error = abs(
      predicted - observed
    ),
    absolute_percentage_error = absolute_error / observed
  ) |>
  summarise(
    across(
      starts_with("absolute"),
      mean,
      .names = "mean_{.col}"
    ),
    .by = c(
      trust, trust_name, specialty
    )
  )


# projections - using tbats -----------------------------------------------
# this projects forward the two years of calibration data

forecast_function <- function(rtt_table, number_timesteps) {
  fcast <- rtt_table |>
    pull(value) |>
    ts(frequency = 12) |>
    forecast::tbats() |>
    forecast::forecast(h = number_timesteps) |>
    tidyr::as_tibble()

  return(fcast)
}

tbats_data <- calibration_period |>
  anti_join(
    combinations_to_remove,
    by = join_by(
      trust, specialty
    )
  )

tbats_referrals <- tbats_data |>
  filter(
    type == "Referrals"
  ) |>
  select(!c("type", "months_waited_id")) |>
  tidyr::nest(
    cal_period = c(period_id, value)
  ) |>
  mutate(
    ref_projections = purrr::map(
      cal_period,
      \(x) forecast_function(
        rtt_table = x,
        number_timesteps = 16
      ) |>
        mutate(
          period_id = dplyr::row_number()
        )
    )
  ) |>
  select(!c("cal_period")) |>
  tidyr::unnest(ref_projections) |>
  select(
    "trust", "trust_name", "specialty",
    "period_id",
    Expected_referrals = "Point Forecast",
    Low_referrals = "Lo 80",
    High_referrals = "Hi 80"
  ) |>
  tidyr::pivot_longer(
    cols = c(Low_referrals, Expected_referrals, High_referrals),
    names_to = "referrals_scenario",
    values_to = "value"
  ) |>
  tidyr::nest(
    ref_projections = c(
      period_id, value
    )
  )

tbats_capacity <-  tbats_data |>
  filter(
    type == "Complete"
  ) |>
  summarise(
    value = sum(value),
    .by = c(
      trust,
      trust_name,
      specialty,
      period_id
    )
  ) |>
  tidyr::nest(
    cal_period = c(period_id, value)
  ) |>
  mutate(
    cap_projections = purrr::map(
      cal_period,
      \(x) forecast_function(
        rtt_table = x,
        number_timesteps = 16
      ) |>
        mutate(
          period_id = dplyr::row_number()
        )
    )
  ) |>
  select(!c("cal_period")) |>
  tidyr::unnest(cap_projections) |>
  select(
    "trust", "trust_name", "specialty",
    "period_id",
    Expected_capacity = "Point Forecast",
    Low_capacity = "Lo 80",
    High_capacity = "Hi 80"
  ) |>
  tidyr::pivot_longer(
    cols = c(Low_capacity, Expected_capacity, High_capacity),
    names_to = "capacity_scenario",
    values_to = "value"
  ) |>
  tidyr::nest(
    cap_projections = c(
      period_id, value
    )
  )

# observed incompletes at the end of the calibration period
tbats_incompletes_at_t0 <- tbats_data |>
  filter(
    type == "Incomplete",
    period_id == max(period_id)
  ) |>
  select(!c("period_id", "type")) |>
  rename(
    incompletes = "value"
  ) |>
  tidyr::nest(
    incompletes_t0 = c(months_waited_id, incompletes)
  )


all_tbats_data <- tbats_capacity |>
  left_join(
    tbats_referrals,
    by = join_by(
      trust, trust_name, specialty
    ),
    relationship = "many-to-many"
  ) |>
  left_join(
    tbats_incompletes_at_t0,
    by = join_by(
      trust, trust_name, specialty
    )
  ) |>
  left_join(
    params,
    by = join_by(
      trust, trust_name, specialty
    )
  )

# create period lookup
period_lkp <- dplyr::tibble(
  period_id = 1:(max(calibration_period$period_id) + 16),
  period = seq(
    from = min(monthly_rtt$period),
    to = as.Date("2026-03-01"),
    by = "months"
  )
)

# create projections
future_projections <- all_tbats_data |>
  # # the following groups have challenges redistributing incompletes
  # anti_join(
  #   tibble(
  #     trust_name = "UNIVERSITY HOSPITALS PLYMOUTH NHS TRUST",
  #     specialty = "Cardiothoracic Surgery"
  #   ),
  #   by = join_by(
  #     trust_name,
  #     specialty
  #   )
  # ) |>
  mutate(
    waiting_times = purrr::pmap(
      .l = list(
        cap_projections,
        ref_projections,
        incompletes_t0,
        params
      ),
      .f = \(cap_proj, ref_proj, incomp_t0, params) apply_params_to_projections(
        capacity_projections = cap_proj |> pull(value),
        referrals_projections = ref_proj |> pull(value),
        incomplete_pathways = incomp_t0,
        renege_capacity_params = params,
        max_months_waited = max_months_waited
      ) |>
        select(
          period_id,
          months_waited_id,
          estimated_incompletes = incompletes
        )
    )
  ) |>
  select(
    trust,
    trust_name,
    specialty,
    capacity_scenario,
    referrals_scenario,
    waiting_times
  ) |>
  tidyr::unnest(
    waiting_times
  ) |>
  mutate(
    period_id = period_id + 31,
    value_type = "projected"
  ) |>
  rename(
    value = estimated_incompletes
  ) |>
  left_join(
    period_lkp,
    by = join_by(period_id)
  )

final_data <- monthly_rtt |>
  filter(
    type == "Incomplete"
  ) |>
  mutate(
    value_type = "observed"
  ) |>
  select(!c("type")) |>
  bind_rows(
    future_projections
  ) |>
  arrange(
    trust,
    trust_name,
    specialty,
    period,
    months_waited_id
  ) |>
  mutate(
    scenario = paste(
      capacity_scenario,
      referrals_scenario,
      sep = " - "
    ),
    scenario = case_when(
      scenario == "NA - NA" ~ "Observed data",
      .default = scenario
    ),
    .keep = "unused"
  ) |>
  select(
    trust,
    trust_name,
    specialty,
    period,
    period_id,
    months_waited_id,
    incomplete_counts = value,
    value_type,
    scenario
  )


write.csv(
  final_data,
  "SW_waiting_times.csv",
  row.names = F
)

write.csv(
  validation_error,
  "SW_model_errors.csv",
  row.names = FALSE
)
