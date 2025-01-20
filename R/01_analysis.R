source("R/00_libraries.R")

# configuration

calibration_start <- as.Date("2023-12-01")
calibration_end <- as.Date("2024-11-30")

# validate on the 6 months prior to the calibration period
validation_end <- calibration_start - 1
validation_start <- lubridate::floor_date(
  validation_end, unit = "months"
) %m-% months(6)

# start date for tbats
tbats_start <- as.Date("2022-12-01")
tbats_end <- calibration_end

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
  date_start = tbats_start,
  date_end = calibration_end,
  show_progress = TRUE
)

all_incomplete <- NHSRtt::get_rtt_data(
  type = "incomplete",
  date_start = tbats_start,
  date_end = calibration_end,
  show_progress = TRUE
)

all_referral <- NHSRtt::get_rtt_data(
  type = "referral",
  date_start = tbats_start,
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

# calculate SW regional values
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

# calculate model parameters --------------------------------------------

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
        full_breakdown = FALSE
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


reorder_vector <- function(x, vals) {
  first_part <- x[!(x %in% vals)]

  reordered <- c(first_part, vals)

  return(reordered)
}

p <- validation_error |>
  mutate(
    specialty = factor(
      specialty,
      levels = reorder_vector(
        unique(specialty),
        c("Other", "Total")
      )
    )
  ) |>
  ggplot(

    aes(
      x = specialty,
      y = mean_absolute_percentage_error
    )
  ) +
  geom_boxplot() +
  theme_bw() +
  labs(
    title = "Mean absolute percentage error for SW trusts by specialty",
    subtitle = paste0(
      "Calibration period: ",
      calibration_start,
      " - ",
      calibration_end,
      "\nValidation period: ",
      validation_start,
      " - ",
      validation_end
    ),
    y = "MAPE",
    x = "Specialty"
  ) +
  theme(
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      vjust = 0.5
    )
  )

ggsave(
  "outputs/mape_by_specialty.png",
  p,
  width = 8,
  height = 7,
  units = "in",
  bg = "white"
)

# projections -----------------------------------------------

# referrals are projected forward using tbats with two years of monthly data

forecast_function <- function(rtt_table, number_timesteps) {
  fcast <- rtt_table |>
    pull(value) |>
    ts(frequency = 12) |>
    forecast::tbats() |>
    forecast::forecast(h = number_timesteps) |>
    tidyr::as_tibble()

  return(fcast)
}

tbats_data <- monthly_rtt |>
  filter(
    between(
      period,
      tbats_start,
      tbats_end
    )
  ) |>
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
    cal_period = c(period, period_id, value)
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
    # "period_id",
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
      value
    )
  )

# the first month for capacity data is based on a linear extrapolation from the
# previous 12 months of observed data. An optimiser is then used to calculate
# the annual increase in capacity required to achieve a target performance

projection_capacity <-  monthly_rtt |>
  filter(
    type == "Complete",
    between(
      period,
      (calibration_end + 1) %m-% months(12),
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
      period
    )
  ) |>
  anti_join(
    combinations_to_remove,
    by = join_by(
      trust, specialty
    )
  ) |>
  mutate(
    mean_val = mean(value),
    median_val = median(value),
    .by = c(
      trust,
      trust_name,
      specialty
    )
  ) |>
  tidyr::nest(
    cal_period = c(period, period_id, value)
  ) |>
  mutate(
    lm_fit = purrr::map(
      cal_period,
      ~ lm(value ~ period, data = .)
    ),
    pval = purrr::map_dbl(
      lm_fit,
      \(x) broom::tidy(x) |> filter(term == "period") |> pull(p.value)
    ),
    lm_val = purrr::map_dbl(
      lm_fit,
      ~ predict(
        object = .x,
        newdata = data.frame(period = prediction_start)
      )
    ),
    t_1_capacity = case_when(
      pval <= 0.05 ~ lm_val,
      .default = mean_val
    )
  ) |>
  dplyr::select(
    "trust", "trust_name", "specialty", "t_1_capacity"
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


all_projection_data <- projection_capacity |>
  left_join(
    tbats_referrals,
    by = join_by(
      trust, trust_name, specialty
    ),
    relationship = "one-to-many"
  ) |>
  left_join(
    tbats_incompletes_at_t0,
    by = join_by(
      trust, trust_name, specialty
    ),
    relationship = "many-to-one"
  ) |>
  left_join(
    params,
    by = join_by(
      trust, trust_name, specialty
    ),
    relationship = "many-to-one"
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
optimised_projections <- all_projection_data |>
  mutate(
    annual_linear_uplift = purrr::pmap_dbl(
      .l = list(
        t_1_capacity,
        ref_projections,
        incompletes_t0,
        params
      ),
      .f = \(t_1_cap, ref_proj, incomp_t0, params) optimise_capacity(
        t_1_capacity = t_1_cap,
        referrals_projections = ref_proj |> pull(value),
        incomplete_pathways = incomp_t0,
        renege_capacity_params = params,
        target = "~-5%",
        target_bin = max_months_waited,
        tolerance = 0.005,
        max_iterations = 25
      )
    )
  ) |>
  select(
    trust,
    trust_name,
    specialty,
    referrals_scenario,
    annual_linear_uplift
  ) |>
  mutate(
    trust_name = factor(
      trust_name,
      levels = rev(
        reorder_vector(
          sort(unique(trust_name)),
          "SW Total"
        )
      )
    ),
    specialty = factor(
      specialty,
      levels = reorder_vector(
        sort(unique(specialty)),
        c("Other", "Total")
      )
    )
  )

# heatmap of capacity change
p <- optimised_projections |>
  filter(
    referrals_scenario == "Expected_referrals"
  ) |>
  ggplot(
    aes(
      x = specialty,
      y = trust_name
    )
  ) +
  geom_tile(
    aes(fill = annual_linear_uplift)
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      vjust = 0.5
    )
  ) +
  scale_fill_viridis_c(

  ) +
  labs(
    title = "Required 12 months uplift in activity to achieve 5% relative reduction in 4 month wait times by the end of March 2026",
    x = "",
    y = "",
    fill = "Activity scaling factor*",
    caption = "* Activity scaling factor is the scale of the change in activity over a year, applied evenly over each month"
  )

ggsave(
  filename = "outputs/activity_requirements.png",
  plot = p,
  width = 16,
  height = 7,
  units = "in",
  bg = "white"
)

optimised_projections |>
  filter(
    referrals_scenario == "Expected_referrals"
  ) |>
  write.csv(
    "outputs/activity_requirements_summarised.csv",
    row.names = FALSE
  )

# calculate projections activity profile
full_activity_projections <- optimised_projections |>
  filter(
    referrals_scenario == "Expected_referrals"
  ) |>
  inner_join(
    projection_capacity,
    by = join_by(
      trust, trust_name, specialty
    )
  ) |>
  mutate(
    t_13_capacity = t_1_capacity * annual_linear_uplift
  ) |>
  filter(
    !is.na(annual_linear_uplift)
  ) |>
  tidyr::pivot_longer(
    cols = starts_with("t_"),
    names_to = "period_id",
    values_to = "capacity"
  ) |>
  mutate(
    period_id = as.numeric(str_extract(period_id, "[[:digit:]]+"))
  ) |>
  tidyr::nest(
    cap_data = c(period_id, capacity)
  ) |>
  mutate(
    lm_fit = purrr::map(
      cap_data,
      \(x) lm(capacity ~ period_id, data = x)

    ),
    projections = purrr::map(
      lm_fit,
      \(x) tibble(required_activity = predict(object = x, newdata = tibble(period_id = 1:16))) |>
        mutate(
          period = seq(
            from = as.Date("2024-12-01"),
            to = as.Date("2026-03-01"),
            by = "months"
          )
        )
    )
  ) |>
  select(
    trust, trust_name, specialty, referrals_scenario, projections
  ) |>
  tidyr::unnest(projections) |>
  rename(
    activity = required_activity
  ) |>
  mutate(
    type = "projection"
  ) |>
  bind_rows(
    tbats_data |>
      filter(
        type == "Complete"
      ) |>
      summarise(
        activity = sum(value),
        .by = c(trust, trust_name, specialty, period)
      ) |>
      mutate(
        type = "observed",
        referrals_scenario = "Observed_referrals"
      )
  ) |>
  relocate(period, .before = activity) |>
  arrange(
    trust_name, specialty, period
  ) |>
  filter(
    any(type == "projection"),
    .by = c(trust, specialty)
  )

write.csv(
  full_activity_projections,
  "outputs/activity_requirements.csv",
  row.names = FALSE
)

activity_plot <- full_activity_projections |>
  mutate(
    trust_name = gsub(" FOUNDATION| TRUST| NHS", "", trust_name),
    trust_name = abbreviate(trust_name, 28),
    trust_name = case_when(
      trust_name != "SW Total" ~ str_to_title(trust_name),
      .default = "SW Total"
    ),
    trust_name = factor(
      trust_name,
      levels = reorder_vector(
        unique(trust_name),
        "SW Total"
      )
    ),
    specialty = factor(
      specialty,
      levels = reorder_vector(
        unique(specialty),
        c("Other", "Total")
      )
    )
  ) |>
  ggplot(
    aes(
      x = period,
      y = activity
    )
  ) +
  geom_line(
    aes(colour = type),
    show.legend = FALSE
  ) +
  facet_wrap(
    facet = vars(trust_name, specialty),
    scales = "free_y"
  ) +
  theme_bw() +
  labs(
    y = "Activity",
    x = ""
  ) +
  scale_colour_manual(
    values = c(
      "observed" = "black",
      "projection" = "gold"
    )
  )

ggsave(
  filename = "outputs/activity_plot.png",
  plot = activity_plot,
  width = 30,
  height = 12,
  units = "in",
  bg = "white"

)

write.csv(
  validation_error,
  "outputs/SW_model_errors.csv",
  row.names = FALSE
)

# testing the optimised projections
optimised_projection_data <- all_projection_data |>
  select(!c("t_1_capacity")) |>
  inner_join(
    full_activity_projections |>
      rename(
        value = activity
      ) |>
      tidyr::nest(capacity = c(period, value)),
    by = join_by(
      trust, trust_name, specialty, referrals_scenario
    )
  )

future_projections <- optimised_projection_data |>
  mutate(
    waiting_times = purrr::pmap(
      .l = list(
        capacity,
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
    # capacity_scenario,
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
  filter(period_id == max(period_id)) |>
  mutate(
    apr_26_incomplete_proportion = value / sum(value),
    .by = c(trust, trust_name, specialty)
  ) |>
  filter(
    months_waited_id == max(months_waited_id)
  ) |>
  select(!c("period_id", "months_waited_id", "value", "value_type")) |>
  left_join(
    tbats_incompletes_at_t0 |>
      mutate(
        nov_11_incomplete_proportion = purrr::map(
          incompletes_t0,
          \(x) x |> mutate(prop = incompletes / sum(incompletes)) |>
            filter(months_waited_id == max(months_waited_id)) |>
            pull(prop)
        )
      ) |>
      select(
        trust, trust_name, specialty, nov_11_incomplete_proportion
      ),
    by = join_by(trust, trust_name, specialty)
  )
