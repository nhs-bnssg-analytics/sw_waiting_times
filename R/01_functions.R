heatmap <- function(data, title) {
  p <- data |>
    ggplot(
      aes(
        x = specialty,
        y = trust_name
      )
    ) +
    geom_tile(
      aes(fill = percentage_change)
    ) +
    geom_text(
      aes(
        label = chart_label,
        colour = text_colour
      ),
      show.legend = FALSE
    ) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      legend.key.width=unit(1.5,"cm"),
      axis.text.x = element_text(
        angle = 90,
        hjust = 1,
        vjust = 0.5
      )
    ) +
    scale_fill_viridis_c(
      labels = scales::label_percent()
    ) +
    scale_colour_manual(
      values = c(
        "black" = "black",
        "white" = "white"
      )
    ) +
    labs(
      title = title,
      subtitle = "Assuming a linear change in activity between the two periods",
      x = "",
      y = "",
      fill = "Relative activity change (Mar 2026 compared with Dec 2024)",
      captions = paste(
        "* Unable to optimise because of treatment profile in calibration period",
        "+ No treatments during calibration period",
        sep = "\n"
      )
    ) +
    guides(
      fill = guide_colourbar(
        title.position = "top",
        title.hjust = 0.5
      )
    )
}
