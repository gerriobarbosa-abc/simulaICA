# =============================================================================
# FUNÇÕES DE GRÁFICOS
# =============================================================================

grafico_simulacao_ica <- function(
    resumo,
    largura_barra = 0.50,
    alpha_barra = 0.96,
    size_rotulo = 20.2,
    vjust_rotulo = -0.55,
    base_size = 40,
    axis_text_x_size = 36,
    axis_text_y_size = 36,
    axis_title_y_size = 40,
    plot_margin = ggplot2::margin(18, 24, 14, 16)
) {

  dados_plot <- tibble::tibble(
    cenario = factor(
      c("Observado", "Simulado", "Meta"),
      levels = c("Observado", "Simulado", "Meta")
    ),
    valor = c(
      resumo$ica_observado,
      resumo$ica_simulado,
      resumo$meta_estado
    )
  )

  cores_barras <- c(
    "Observado" = "#5f83ad",
    "Simulado"  = "#4f9a75",
    "Meta"      = "#dc6b76"
  )

  ggplot2::ggplot(
    dados_plot,
    ggplot2::aes(x = cenario, y = valor, fill = cenario)
  ) +
    ggplot2::geom_col(
      width = largura_barra,
      alpha = alpha_barra,
      show.legend = FALSE
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = scales::percent(
          valor,
          accuracy = 0.1,
          decimal.mark = ","
        )
      ),
      vjust = vjust_rotulo,
      size = size_rotulo,
      fontface = "bold",
      family = "nunito",
      color = "#292820"
    ) +
    ggplot2::scale_fill_manual(
      values = cores_barras
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, 1.05),
      breaks = seq(0, 1, by = 0.25),
      labels = scales::percent_format(
        accuracy = 1,
        decimal.mark = ","
      ),
      expand = ggplot2::expansion(mult = c(0, 0.02))
    ) +
    ggplot2::labs(
      x = NULL,
      y = "ICA estadual"
    ) +
    ggplot2::theme_minimal(
      base_size = base_size,
      base_family = "nunito"
    ) +
    ggplot2::theme(
      text = ggplot2::element_text(
        family = "nunito",
        color = "#292820"
      ),
      axis.text.x = ggplot2::element_text(
        size = axis_text_x_size,
        face = "bold",
        color = "#696a67",
        margin = ggplot2::margin(t = 8)
      ),
      axis.text.y = ggplot2::element_text(
        size = axis_text_y_size,
        color = "#696a67"
      ),
      axis.title.y = ggplot2::element_text(
        size = axis_title_y_size,
        face = "bold",
        color = "#696a67",
        margin = ggplot2::margin(r = 12)
      ),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(
        color = "#e8ecf1",
        linewidth = 0.55
      ),
      plot.margin = plot_margin
    )
}
