# =============================================================================
# FUNÇÕES DE DADOS E CÁLCULO
# =============================================================================

preparar_dados_ica <- function(dados) {

  dados |>
    dplyr::mutate(
      ano = as.integer(ano),
      sigla_uf = as.character(sigla_uf),
      co_municipio = as.character(co_municipio),
      no_municipio = as.character(no_municipio),
      regional = as.character(regional),
      ica = as.numeric(ica),
      matricula = as.numeric(matricula),
      avaliados = as.numeric(avaliados)
    )
}


calcular_media_ponderada <- function(data, var_valor, var_peso = "avaliados") {

  valor <- data[[var_valor]]
  peso  <- data[[var_peso]]

  soma_peso <- sum(peso, na.rm = TRUE)

  if (is.na(soma_peso) || soma_peso == 0) {
    return(NA_real_)
  }

  sum(valor * peso, na.rm = TRUE) / soma_peso
}


ajustar_escala_meta <- function(meta) {

  if (is.na(meta)) {
    return(NA_real_)
  }

  if (meta > 1) {
    meta / 100
  } else {
    meta
  }
}


fmt_pct <- function(x, accuracy = 0.1) {

  if (is.na(x) || is.nan(x)) {
    return("-")
  }

  scales::percent(
    x,
    accuracy = accuracy,
    decimal.mark = ","
  )
}


fmt_pp <- function(x, digits = 2) {

  if (is.na(x) || is.nan(x)) {
    return("-")
  }

  paste0(
    format(
      round(x * 100, digits),
      decimal.mark = ",",
      nsmall = digits
    ),
    " p.p."
  )
}


criar_tabela_municipio_simulado <- function(base,
                                            municipio,
                                            novo_ica) {

  total_avaliados_estado <- sum(base$avaliados, na.rm = TRUE)

  base |>
    dplyr::filter(co_municipio == municipio) |>
    dplyr::mutate(
      ica_observado = ica,
      ica_simulado = novo_ica / 100,
      diferenca_municipio = (ica_simulado - ica_observado) * 100,
      peso_estado = avaliados / total_avaliados_estado,
      impacto_estado = diferenca_municipio * peso_estado
    ) |>
    dplyr::transmute(
      Município = no_municipio,
      Regional = regional,
      Matrículas = round(matricula, 0),
      Avaliados = round(avaliados, 0),
      `ICA observado` = ica_observado,
      `ICA simulado` = ica_simulado,
      `Diferença no município` = diferenca_municipio,
      `Peso no estado` = peso_estado,
      `Impacto no estado` = impacto_estado
    )
}


# =============================================================================
# FUNÇÕES PARA SIMULAÇÃO COM MÚLTIPLOS MUNICÍPIOS
# =============================================================================

aplicar_simulacao_municipios <- function(data,
                                         municipios,
                                         valor_simulacao,
                                         modo_simulacao = "incrementar",
                                         var_id = "co_municipio",
                                         var_ica = "ica") {

  if (length(municipios) == 0 || is.null(municipios)) {
    return(data)
  }

  data_simulada <- data

  idx_municipios <- data_simulada[[var_id]] %in% municipios

  if (modo_simulacao == "substituir") {

    data_simulada[[var_ica]][idx_municipios] <- valor_simulacao / 100

  } else if (modo_simulacao == "incrementar") {

    data_simulada[[var_ica]][idx_municipios] <- data_simulada[[var_ica]][idx_municipios] +
      valor_simulacao / 100

    data_simulada[[var_ica]][idx_municipios] <- pmin(
      pmax(data_simulada[[var_ica]][idx_municipios], 0),
      1
    )
  }

  data_simulada
}


criar_tabela_municipios_simulados <- function(base,
                                              municipios,
                                              valor_simulacao,
                                              modo_simulacao = "incrementar") {

  total_avaliados_recorte <- sum(base$avaliados, na.rm = TRUE)

  tabela <- base |>
    dplyr::filter(co_municipio %in% municipios) |>
    dplyr::mutate(
      ica_observado = ica
    )

  if (modo_simulacao == "substituir") {

    tabela <- tabela |>
      dplyr::mutate(
        ica_simulado = valor_simulacao / 100
      )

  } else if (modo_simulacao == "incrementar") {

    tabela <- tabela |>
      dplyr::mutate(
        ica_simulado = pmin(
          pmax(ica + valor_simulacao / 100, 0),
          1
        )
      )

  } else {

    tabela <- tabela |>
      dplyr::mutate(
        ica_simulado = ica
      )
  }

  tabela |>
    dplyr::mutate(
      diferenca_municipio = ica_simulado - ica_observado,
      peso_recorte = avaliados / total_avaliados_recorte,
      impacto_recorte = diferenca_municipio * peso_recorte
    ) |>
    dplyr::arrange(dplyr::desc(abs(impacto_recorte))) |>
    dplyr::transmute(
      UF = sigla_uf,
      Município = no_municipio,
      Regional = regional,
      Matrículas = round(matricula, 0),
      Avaliados = round(avaliados, 0),
      `ICA observado` = ica_observado,
      `ICA simulado` = ica_simulado,
      `Diferença no município (p.p.)` = diferenca_municipio * 100,
      `Peso no recorte` = peso_recorte,
      `Impacto no recorte (p.p.)` = impacto_recorte * 100
    )
}
