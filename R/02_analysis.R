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


max_months_waited <- 12 # I am only interested in waiting time bins up to 12 months

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
    y = "MAPE (on log10 scale)",
    x = "Specialty"
  ) +
  theme(
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      vjust = 0.5
    )
  ) +
  scale_y_log10()

ggsave(
  "outputs/v2/mape_by_specialty.png",
  p,
  width = 8,
  height = 7,
  units = "in",
  bg = "white"
)

# projections -----------------------------------------------

# three different modelling scenarios:

# scenario 1: referrals and activity are projected forwards using timeseries
# methods (TBATS) based on two years of data

# scenario 2: a target of 18 week wait times is introduced where the greater of
# the following two options are used: Option 1 - a 5% improvement on Nov 24
# 18 week wait times, Option 2 - 60% treatments prior to 18 weeks wait

# scenario 3: 52 week wait times come down to less than 1%



# setting up the data -----------------------------------------------------

# REFERRALS

# all scenarios will use the same referrals data. Referrals are projected
# forward using tbats with two years of monthly data

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
    Expected_referrals = "Point Forecast",
    Low_referrals = "Lo 80",
    High_referrals = "Hi 80"
  ) |>
  tidyr::pivot_longer(
    cols = c(Low_referrals, Expected_referrals, High_referrals),
    names_to = "referrals_scenario",
    values_to = "value"
  ) |>
  # adjust referrals that are less than 0 to 0
  mutate(
    value = case_when(
      value < 0 ~ 0,
      .default = value
    )
  ) |>
  tidyr::nest(
    ref_projections = c(
      value
    )
  )

# CAPACITY

# scenario 1 will use a timeseries projection for activity
tbats_capacity <- tbats_data |>
  filter(
    type == "Complete"
  ) |>
  select(!c("type", "months_waited_id")) |>
  tidyr::nest(
    cal_period = c(period, period_id, value)
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
    Expected_capacity = "Point Forecast",
    Low_capacity = "Lo 80",
    High_capacity = "Hi 80"
  ) |>
  tidyr::pivot_longer(
    cols = c(Low_capacity, Expected_capacity, High_capacity),
    names_to = "capacity_scenario",
    values_to = "value"
  ) |>
  # adjust capacity that are less than 0 to 0
  mutate(
    value = case_when(
      value < 0 ~ 0,
      .default = value
    )
  ) |>
  tidyr::nest(
    cap_projections = c(
      value
    )
  )


# scenario 2 and 3 will use the first month for capacity data is based on a
# linear extrapolation from the previous 12 months of observed data. An
# optimiser is then used to calculate the annual increase in capacity required
# to achieve a target performance

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


# INCOMPLETES at t = 0

# All three scenarios use the observed incompletes from Nov 2024

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

# COMBINING DATA

# scenario 1

all_scenario_1_data <- tbats_capacity |>
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

# scenario 2 and 3 data

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


# Projections -------------------------------------------------------------

# create period lookup
period_lkp <- dplyr::tibble(
  period_id = 1:(max(calibration_period$period_id) + 16),
  period = seq(
    from = min(monthly_rtt$period),
    to = as.Date("2026-03-01"),
    by = "months"
  )
)

# scenario 1 projections

scenario_1 <- all_scenario_1_data |>
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
    trust, trust_name, specialty, referrals_scenario, capacity_scenario,
    waiting_times
  ) |>
  unnest(
    cols = waiting_times
  ) |>
  mutate(
    period_id = period_id + max(monthly_rtt$period_id)
  ) |>
  left_join(
    period_lkp,
    by = join_by(period_id)
  ) |>
  select(!c("period_id")) |>
  relocate(period, .before = months_waited_id)

write.csv(
  scenario_1,
  "outputs/v2/unoptimised_scenarios.csv",
  row.names = FALSE
)

# Scenario 2 projections

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

# calculate targets by trust
# scenario 2 wants to optimise at 18 weeks, which is roughly bin 4 to 5 months

target_bin <- 4
trust_targets <- all_calibration_data |>
  filter(
    specialty == "Total"
  ) |>
  select(
    "trust", "trust_name", "incompletes_data"
  ) |>
  unnest(
    cols = "incompletes_data"
  ) |>
  filter(
    period_id == max(period_id)
  ) |>
  mutate(
    bin = case_when(
      months_waited_id >= target_bin ~ "Above",
      .default = "Below"
    )
  ) |>
  summarise(
    total_incompletes = sum(incompletes),
    .by = c(
      trust, trust_name, bin
    )
  ) |>
  mutate(
    proportion = total_incompletes / sum(total_incompletes),
    .by = c(
      trust, trust_name
    )
  ) |>
  filter(
    bin == "Below"
  ) |>
  mutate(
    relative_change_target = case_when(
      proportion + 0.05 < 0.6 ~ 0.6 - proportion,
      .default = 0.05
    ),
    # the function takes the target as the relative change in or above the bin of
    # interest, so I need to calculate the the negative version
    relative_change_target = - round(relative_change_target * 100, 1),
    relative_change_target = paste0(
      "~",
      relative_change_target,
      "%"
    )
  ) |>
  select(
    "trust", "relative_change_target"
  )


# create projections for scenario 2

scenario_2_projections <- all_projection_data |>
  filter(
    referrals_scenario == "Expected_referrals"
  ) |>
  left_join(
    trust_targets,
    by = join_by(trust),
    relationship = "many-to-one"
  ) |>
  mutate(
    annual_linear_uplift = purrr::pmap_dbl(
      .l = list(
        t_1_capacity,
        ref_projections,
        incompletes_t0,
        params,
        relative_change_target
      ),
      .f = \(t_1_cap, ref_proj, incomp_t0, params, target) optimise_capacity(
        t_1_capacity = t_1_cap,
        referrals_projections = ref_proj |> pull(value),
        incomplete_pathways = incomp_t0,
        renege_capacity_params = params,
        target = target,
        target_bin = target_bin,
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
    t_1_capacity,
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
    ),
    monthly_cap_change = ((t_1_capacity * annual_linear_uplift) - t_1_capacity) / 12,
    mar_26_capacity = t_1_capacity + (15 * monthly_cap_change),
    percentage_change = (mar_26_capacity / t_1_capacity) - 1,
    chart_label = case_when(
      is.nan(annual_linear_uplift) ~ "*",
      annual_linear_uplift == Inf ~ "*",
      is.na(annual_linear_uplift) ~ "+",
      .default = scales::label_percent(accuracy = 1)(percentage_change)
    )
  )

# create projections for scenario 3

scenario_3_projections <- all_projection_data |>
  filter(
    referrals_scenario == "Expected_referrals"
  ) |>
  mutate(
    annual_linear_uplift = purrr::pmap_dbl(
      .l = list(
        t_1_capacity,
        ref_projections,
        incompletes_t0,
        params
      ),
      .f = \(t_1_cap, ref_proj, incomp_t0, params, target) optimise_capacity(
        t_1_capacity = t_1_cap,
        referrals_projections = ref_proj |> pull(value),
        incomplete_pathways = incomp_t0,
        renege_capacity_params = params,
        target = "1%",
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
    t_1_capacity,
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
    ),
    monthly_cap_change = ((t_1_capacity * annual_linear_uplift) - t_1_capacity) / 12,
    mar_26_capacity = t_1_capacity + (15 * monthly_cap_change),
    percentage_change = (mar_26_capacity / t_1_capacity) - 1,
    chart_label = case_when(
      is.nan(annual_linear_uplift) ~ "*",
      annual_linear_uplift == Inf ~ "*",
      is.na(annual_linear_uplift) ~ "+",
      .default = scales::label_percent(accuracy = 1)(percentage_change)
    )
  )

# NA means no calibration counts
# Inf means not possible to optimise based on calibration treatment profile
# NaN means hasn't managed to converge

# heatmap of capacity change
p_18 <- scenario_2_projections |>
  filter(
    referrals_scenario == "Expected_referrals"
  ) |>
  mutate(
    text_colour = case_when(
      percentage_change > quantile(
        percentage_change[percentage_change != Inf], 0.95,
        na.rm = TRUE) ~ "black",
      is.na(percentage_change) ~ "black",
      percentage_change == Inf ~ "black",
      .default = "white"
    )
  ) |>
  heatmap(
    title = "Required activity change March 2026 relative to Dec 2024 to meet 18ww targets"
  )



ggsave(
  filename = "outputs/v2/activity_requirements_18ww.png",
  plot = p_18,
  width = 16,
  height = 7,
  units = "in",
  bg = "white"
)

p_52 <- scenario_3_projections |>
  filter(
    referrals_scenario == "Expected_referrals"
  ) |>
  mutate(
    text_colour = case_when(
      percentage_change > quantile(
        percentage_change[percentage_change != Inf], 0.95,
        na.rm = TRUE) ~ "black",
      is.na(percentage_change) ~ "black",
      percentage_change == Inf ~ "black",
      .default = "white"
    )
  ) |>
  heatmap(
    title = "Required activity change March 2026 relative to Dec 2024 to meet 52ww targets"
  )

ggsave(
  filename = "outputs/v2/activity_requirements_52ww.png",
  plot = p_52,
  width = 16,
  height = 7,
  units = "in",
  bg = "white"
)

list(
  scenario_2 = scenario_2_projections,
  scenario_3 = scenario_3_projections
) |>
  iwalk(
    \(x, idx) x  |>
      write.csv(
        paste0("outputs/v2/", idx, "_activity_requirements_summarised.csv"),
        row.names = FALSE
      )
    )


# calculate projections activity profile
scenario_activity_projections <-
  list(
    scenario_2 = scenario_2_projections,
    scenario_3 = scenario_3_projections
  ) |>
  imap(
    \(x, idx) x |>
      select(
        trust, trust_name, specialty, referrals_scenario, t_1_capacity, annual_linear_uplift
      ) |>
      mutate(
        t_13_capacity = t_1_capacity * annual_linear_uplift
      ) |>
      filter(
        !is.na(annual_linear_uplift),
        annual_linear_uplift != Inf
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
      ) |>
      setNames(nm = idx)
  )

# write to csv
iwalk(
  scenario_activity_projections,
  \(x, idx) write.csv(
    x,
  paste0("outputs/v2/", idx, "_activity_requirements.csv"),
  row.names = FALSE
  )
)


# activity_plot <- full_activity_projections |>
#   mutate(
#     trust_name = gsub(" FOUNDATION| TRUST| NHS", "", trust_name),
#     trust_name = abbreviate(trust_name, 28),
#     trust_name = case_when(
#       trust_name != "SW Total" ~ str_to_title(trust_name),
#       .default = "SW Total"
#     ),
#     trust_name = factor(
#       trust_name,
#       levels = reorder_vector(
#         unique(trust_name),
#         "SW Total"
#       )
#     ),
#     specialty = factor(
#       specialty,
#       levels = reorder_vector(
#         unique(specialty),
#         c("Other", "Total")
#       )
#     )
#   ) |>
#   ggplot(
#     aes(
#       x = period,
#       y = activity
#     )
#   ) +
#   geom_line(
#     aes(colour = type),
#     show.legend = FALSE
#   ) +
#   facet_wrap(
#     facet = vars(trust_name, specialty),
#     scales = "free_y"
#   ) +
#   theme_bw() +
#   labs(
#     y = "Activity",
#     x = ""
#   ) +
#   scale_colour_manual(
#     values = c(
#       "observed" = "black",
#       "projection" = "gold"
#     )
#   )
#
# ggsave(
#   filename = "outputs/activity_plot.png",
#   plot = activity_plot,
#   width = 30,
#   height = 12,
#   units = "in",
#   bg = "white"
#
# )

write.csv(
  validation_error,
  "outputs/v2/SW_model_errors.csv",
  row.names = FALSE
)

# # testing the optimised projections
# optimised_projection_data <- all_projection_data |>
#   select(!c("t_1_capacity")) |>
#   inner_join(
#     full_activity_projections |>
#       rename(
#         value = activity
#       ) |>
#       tidyr::nest(capacity = c(period, value)),
#     by = join_by(
#       trust, trust_name, specialty, referrals_scenario
#     )
#   )
#
# future_projections <- optimised_projection_data |>
#   mutate(
#     waiting_times = purrr::pmap(
#       .l = list(
#         capacity,
#         ref_projections,
#         incompletes_t0,
#         params
#       ),
#       .f = \(cap_proj, ref_proj, incomp_t0, params) apply_params_to_projections(
#         capacity_projections = cap_proj |> pull(value),
#         referrals_projections = ref_proj |> pull(value),
#         incomplete_pathways = incomp_t0,
#         renege_capacity_params = params,
#         max_months_waited = max_months_waited
#       ) |>
#         select(
#           period_id,
#           months_waited_id,
#           estimated_incompletes = incompletes
#         )
#     )
#   ) |>
#   select(
#     trust,
#     trust_name,
#     specialty,
#     # capacity_scenario,
#     referrals_scenario,
#     waiting_times
#   ) |>
#   tidyr::unnest(
#     waiting_times
#   ) |>
#   mutate(
#     period_id = period_id + 31,
#     value_type = "projected"
#   ) |>
#   rename(
#     value = estimated_incompletes
#   )
#
# year_on_comparison <- future_projections |>
#   filter(period_id == max(period_id)) |>
#   mutate(
#     apr_26_incomplete_proportion = value / sum(value),
#     .by = c(trust, trust_name, specialty, period_id)
#   ) |>
#   filter(
#     months_waited_id == max(months_waited_id)
#   ) |>
#   select(!c("period_id", "months_waited_id", "value", "value_type")) |>
#   left_join(
#     tbats_incompletes_at_t0 |>
#       mutate(
#         nov_23_incomplete_proportion = purrr::map(
#           incompletes_t0,
#           \(x) x |> mutate(prop = incompletes / sum(incompletes)) |>
#             filter(months_waited_id == max(months_waited_id)) |>
#             pull(prop)
#         )
#       ) |>
#       select(
#         trust, trust_name, specialty, nov_23_incomplete_proportion
#       ),
#     by = join_by(trust, trust_name, specialty)
#   )
