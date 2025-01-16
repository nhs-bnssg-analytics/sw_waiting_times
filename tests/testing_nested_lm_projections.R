# testing the lm and mean process

row_to_use <- 58

df_all <- projection_capacity |>
  slice(row_to_use)

df_all$cal_period[[1]] |>
  ggplot(
    aes(x = period,
        y = value)
  ) +
  geom_point() +
  theme_bw() +
  geom_abline(
    slope = df_all$lm_fit[[1]]$coefficients["period"],
    intercept = df_all$lm_fit[[1]]$coefficients["(Intercept)"]
  ) +
  geom_point(
    data = tibble(
      y = c(df_all$lm_val, df_all$mean_val),
      x = prediction_start,
      type = c("lm", "mean")
    ),
    aes(
      x = x,
      y = y,
      colour = type
    )
  ) +
  labs(title = df_all$pval)
