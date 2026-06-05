
# =============================================================================
# FUNÇÕES DE INTERFACE
# =============================================================================

label_com_tooltip <- function(label, texto) {

  htmltools::tagList(
    htmltools::span(label),
    bslib::tooltip(
      trigger = shiny::icon("circle-info", class = "icone-info"),
      texto,
      placement = "right"
    )
  )
}


card_resultado <- function(titulo,
                           output_id,
                           classe = "",
                           tooltip = NULL) {

  # ---------------------------------------------------------------------------
  # 1. Definir a cor do tooltip
  # ---------------------------------------------------------------------------
  # A cor do tooltip acompanha a classe visual do card.
  # Se o card não tiver classe, usamos o azul como padrão.

  tooltip_cor <- switch(
    classe,
    "verde" = "tooltip-card-verde",
    "laranja" = "tooltip-card-laranja",
    "vermelho" = "tooltip-card-vermelho",
    "cinza" = "tooltip-card-cinza",
    "tooltip-card-azul"
  )


  # ---------------------------------------------------------------------------
  # 2. Criar o rótulo do card
  # ---------------------------------------------------------------------------
  # Se não houver tooltip, o título aparece normal.
  # Se houver tooltip, aparece o título + ícone de informação.

  label_card <- if (is.null(tooltip)) {

    htmltools::div(
      class = "resultado-label",
      titulo
    )

  } else {

    htmltools::div(
      class = "resultado-label resultado-label-com-tooltip",

      htmltools::span(titulo),

      bslib::tooltip(
        trigger = shiny::icon(
          "circle-info",
          class = "card-info-icon"
        ),
        tooltip,
        placement = "top",
        options = list(
          customClass = paste("tooltip-card", tooltip_cor)
        )
      )
    )
  }


  # ---------------------------------------------------------------------------
  # 3. Criar o card completo
  # ---------------------------------------------------------------------------

  htmltools::div(
    class = paste("resultado-card", classe),

    label_card,

    htmltools::div(
      class = "resultado-valor",
      shiny::textOutput(output_id)
    )
  )
}

box_conteudo <- function(titulo,
                         subtitulo = NULL,
                         conteudo) {

  htmltools::div(
    class = "box-grafico",
    htmltools::h4(titulo),
    if (!is.null(subtitulo)) {
      htmltools::p(
        class = "box-subtitulo",
        subtitulo
      )
    },
    conteudo
  )
}
