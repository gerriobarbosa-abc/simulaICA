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


calcular_media_ponderada <- function(data,
                                     var_valor,
                                     var_peso = "avaliados") {

  valor <- as.numeric(data[[var_valor]])
  peso  <- as.numeric(data[[var_peso]])

  validos <- is.finite(valor) & is.finite(peso) & peso > 0

  if (!any(validos)) {
    return(NA_real_)
  }

  sum(valor[validos] * peso[validos]) /
    sum(peso[validos])
}


ajustar_escala_meta <- function(meta) {

  if (length(meta) == 0 || is.na(meta) || is.nan(meta)) {
    return(NA_real_)
  }

  if (meta > 1) {
    meta / 100
  } else {
    meta
  }
}


fmt_pct <- function(x, accuracy = 0.1) {

  if (length(x) == 0 || is.na(x) || is.nan(x)) {
    return("-")
  }

  scales::percent(
    x,
    accuracy = accuracy,
    decimal.mark = ","
  )
}


fmt_pp <- function(x, digits = 2) {

  if (length(x) == 0 || is.na(x) || is.nan(x)) {
    return("-")
  }

  paste0(
    format(
      round(x * 100, digits),
      decimal.mark = ",",
      nsmall = digits,
      trim = TRUE
    ),
    " p.p."
  )
}


# =============================================================================
# SIMULAÇÃO COM MÚLTIPLOS MUNICÍPIOS
# =============================================================================

aplicar_simulacao_municipios <- function(data,
                                         municipios,
                                         valor_simulacao,
                                         modo_simulacao = "incrementar",
                                         var_id = "co_municipio",
                                         var_ica = "ica") {

  municipios <- as.character(
    unlist(municipios, use.names = FALSE)
  )

  if (length(municipios) == 0 || is.null(municipios)) {
    return(data)
  }

  data_simulada <- data

  data_simulada[[var_id]] <- as.character(
    data_simulada[[var_id]]
  )

  data_simulada[[var_ica]] <- as.numeric(
    data_simulada[[var_ica]]
  )

  idx_municipios <- data_simulada[[var_id]] %in% municipios

  if (modo_simulacao == "substituir") {

    data_simulada[[var_ica]][idx_municipios] <-
      as.numeric(valor_simulacao) / 100

  } else if (modo_simulacao == "incrementar") {

    data_simulada[[var_ica]][idx_municipios] <-
      data_simulada[[var_ica]][idx_municipios] +
      as.numeric(valor_simulacao) / 100

    data_simulada[[var_ica]][idx_municipios] <- pmin(
      pmax(
        data_simulada[[var_ica]][idx_municipios],
        0
      ),
      1
    )
  }

  data_simulada
}


# =============================================================================
# RESUMO DO GRUPO DE MUNICÍPIOS SELECIONADOS
# =============================================================================

calcular_ica_grupo <- function(data,
                               var_valor = "ica",
                               var_peso = "avaliados",
                               var_id = "co_municipio") {

  if (nrow(data) == 0) {
    return(NA_real_)
  }

  # Com um único município, a média ponderada é exatamente o ICA dele.
  # Com vários municípios, resulta no ICA agregado ponderado pelos avaliados.
  calcular_media_ponderada(
    data = data,
    var_valor = var_valor,
    var_peso = var_peso
  )
}


calcular_resumo_grupo <- function(base_observada,
                                  base_simulada) {

  ica_observado <- calcular_ica_grupo(
    data = base_observada,
    var_valor = "ica",
    var_peso = "avaliados"
  )

  ica_simulado <- calcular_ica_grupo(
    data = base_simulada,
    var_valor = "ica",
    var_peso = "avaliados"
  )

  tibble::tibble(
    ica_observado = ica_observado,
    ica_simulado = ica_simulado,
    mudanca = ica_simulado - ica_observado
  )
}


# =============================================================================
# TABELA DOS MUNICÍPIOS SIMULADOS
# =============================================================================

criar_tabela_municipios_simulados <- function(
    base,
    municipios,
    valor_simulacao,
    modo_simulacao = "incrementar"
) {

  municipios <- as.character(
    unlist(municipios, use.names = FALSE)
  )

  nomes_saida <- c(
    "UF",
    "Município",
    "Regional",
    "Matrículas",
    "Avaliados",
    "ICA observado",
    "ICA simulado",
    "Diferença (p.p.)",
    "Peso na UF",
    "Impacto na UF (p.p.)"
  )

  if (length(municipios) == 0) {

    resultado_vazio <- data.frame(
      UF = character(),
      Município = character(),
      Regional = character(),
      Matrículas = numeric(),
      Avaliados = numeric(),
      `ICA observado` = numeric(),
      `ICA simulado` = numeric(),
      `Diferença (p.p.)` = numeric(),
      `Peso na UF` = numeric(),
      `Impacto na UF (p.p.)` = numeric(),
      check.names = FALSE
    )

    return(resultado_vazio)
  }

  base <- base |>
    dplyr::mutate(
      co_municipio = as.character(co_municipio),
      sigla_uf = as.character(sigla_uf),
      no_municipio = as.character(no_municipio),
      regional = as.character(regional),
      matricula = as.numeric(matricula),
      avaliados = as.numeric(avaliados),
      ica = as.numeric(ica)
    )

  total_avaliados_recorte <- sum(
    base$avaliados,
    na.rm = TRUE
  )

  tabela_calculo <- base |>
    dplyr::filter(
      co_municipio %in% municipios
    ) |>
    dplyr::mutate(
      ica_observado = ica
    )

  if (nrow(tabela_calculo) == 0) {

    stop(
      "Nenhum município selecionado foi encontrado no recorte atual.",
      call. = FALSE
    )
  }

  tabela_calculo <- aplicar_simulacao_municipios(
    data = tabela_calculo,
    municipios = municipios,
    valor_simulacao = valor_simulacao,
    modo_simulacao = modo_simulacao,
    var_id = "co_municipio",
    var_ica = "ica"
  ) |>
    dplyr::rename(
      ica_simulado = ica
    ) |>
    dplyr::mutate(
      diferenca = ica_simulado - ica_observado,

      peso_na_uf = if (
        is.finite(total_avaliados_recorte) &&
        total_avaliados_recorte > 0
      ) {
        avaliados / total_avaliados_recorte
      } else {
        NA_real_
      },

      impacto_na_uf = diferenca * peso_na_uf
    )

  linhas_municipios <- tabela_calculo |>
    dplyr::arrange(
      dplyr::desc(abs(impacto_na_uf))
    ) |>
    dplyr::transmute(
      UF = as.character(sigla_uf),
      Município = as.character(no_municipio),
      Regional = dplyr::coalesce(
        as.character(regional),
        "-"
      ),
      Matrículas = as.numeric(round(matricula, 0)),
      Avaliados = as.numeric(round(avaliados, 0)),
      `Peso na UF` = as.numeric(peso_na_uf), # peso dos avaliados
      `ICA observado` = as.numeric(ica_observado),
      `ICA simulado` = as.numeric(ica_simulado),
      `Diferença (p.p.)` = as.numeric(diferenca * 100),

      `Impacto na UF (p.p.)` =
        as.numeric(impacto_na_uf * 100)
    )

  ica_observado_grupo <- calcular_media_ponderada(
    data = tabela_calculo,
    var_valor = "ica_observado",
    var_peso = "avaliados"
  )

  ica_simulado_grupo <- calcular_media_ponderada(
    data = tabela_calculo,
    var_valor = "ica_simulado",
    var_peso = "avaliados"
  )

  matriculas_grupo <- sum(
    tabela_calculo$matricula,
    na.rm = TRUE
  )

  avaliados_grupo <- sum(
    tabela_calculo$avaliados,
    na.rm = TRUE
  )

  diferenca_grupo <-
    ica_simulado_grupo -
    ica_observado_grupo

  peso_grupo <- if (
    is.finite(total_avaliados_recorte) &&
    total_avaliados_recorte > 0
  ) {
    avaliados_grupo / total_avaliados_recorte
  } else {
    NA_real_
  }

  impacto_grupo <- sum(
    tabela_calculo$impacto_na_uf,
    na.rm = TRUE
  )

  linha_grupo <- tibble::tibble(
    UF = "Grupo",
    Município = "Grupo",
    Regional = "-",
    Matrículas = as.numeric(round(matriculas_grupo, 0)),
    Avaliados = as.numeric(round(avaliados_grupo, 0)),
    `Peso na UF` = as.numeric(peso_grupo), # peso dos avaliados no grupo
    `ICA observado` = as.numeric(ica_observado_grupo),
    `ICA simulado` = as.numeric(ica_simulado_grupo),
    `Diferença (p.p.)` =
      as.numeric(diferenca_grupo * 100),
    `Impacto na UF (p.p.)` =
      as.numeric(impacto_grupo * 100)
  )

  # ---------------------------------------------------------------------------
  # Juntar a linha do grupo com as linhas dos municípios
  # ---------------------------------------------------------------------------

  colunas_saida <- c(
    "UF",
    "Município",
    "Regional",
    "Matrículas",
    "Avaliados",
    "Peso na UF", # peso dos avaliados
    "ICA observado",
    "ICA simulado",
    "Diferença (p.p.)",
    "Impacto na UF (p.p.)"
  )

  resultado <- dplyr::bind_rows(
    linha_grupo,
    linhas_municipios
  ) |>
    dplyr::select(
      dplyr::all_of(colunas_saida)
    )

  # Interrompe o cálculo caso a estrutura não tenha exatamente 10 colunas.
  stopifnot(
    identical(names(resultado), colunas_saida),
    ncol(resultado) == 10L
  )

  # Converter o tibble para data.frame simples antes de enviar ao DT
  resultado <- as.data.frame(
    resultado,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  # Retorno final da função
  resultado
}
