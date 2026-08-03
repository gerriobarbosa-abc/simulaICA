
# =============================================================================
# APP - SIMULADOR DO ICA
# =============================================================================

# -----------------------------------------------------------------------------
# 1. Pacotes
# -----------------------------------------------------------------------------

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(scales)
library(tibble)
library(DT)
library(purrr)
# library(writexl)
# requireNamespace("curl", quietly = TRUE)

# -----------------------------------------------------------------------------
# 2. Arquivos auxiliares
# -----------------------------------------------------------------------------

source("R/00_config.R")
source("R/01_funcoes_dados.R")
source("R/02_funcoes_ui.R")
source("R/03_funcoes_graficos.R")


# -----------------------------------------------------------------------------
# 3. Leitura e preparação da base
# -----------------------------------------------------------------------------

dados <- readRDS("data/dados_ica.rds") |>
  dplyr::mutate(regional = '-')

dados <- preparar_dados_ica(dados)


# -----------------------------------------------------------------------------
# 4. Interface
# -----------------------------------------------------------------------------

ui <- page_navbar(

  title = div(
    class = "navbar-brand-custom",
    span(class = "navbar-brand-titulo", "Simulador ICA")
  ),

  theme = tema_app,

  header = tags$head(
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "custom.css"
    )
  ),

  nav_panel(

    title = div(
      class = "nav-tab-label",
      icon("city", class = "nav-tab-icon"),
      span("Simulação - Municípios")
    ),

    layout_sidebar(

      sidebar = sidebar(

        width = 340,

        div(
          class = "sidebar-card",

          selectizeInput(
            inputId = "ufs",
            label = label_com_tooltip(
              "Estado(s)",
              "Selecione um ou mais estados. Se selecionar todos os estados, o recorte passa a representar o país."
            ),
            choices = sort(unique(dados$sigla_uf)),
            selected = sort(unique(dados$sigla_uf))[1],
            multiple = TRUE,
            options = list(
              plugins = list("remove_button"),
              placeholder = "Escolha um ou mais estados"
            )
          ),

          div(
            class = "botoes-filtro",
            actionButton(
              inputId = "selecionar_todos_ufs",
              label = "Todos os Estados",
              class = "btn-filtro"
            ),
            actionButton(
              inputId = "limpar_ufs",
              label = "Limpar",
              class = "btn-filtro-secundario"
            )
          ),

          selectInput(
            inputId = "ano_ica",
            label = label_com_tooltip(
              "Ano do ICA observado",
              "Ano usado como ponto de partida da simulação."
            ),
            choices = sort(unique(dados$ano)),
            selected = max(dados$ano, na.rm = TRUE)
          ),

          selectInput(
            inputId = "ano_meta",
            label = label_com_tooltip(
              "Ano da meta",
              "Ano da meta usada para comparar o resultado observado e simulado do recorte."
            ),
            choices = 2025:2030,
            selected = 2030
          ),

          selectizeInput(
            inputId = "municipios",
            label = label_com_tooltip(
              "Município(s)",
              "Selecione um ou mais municípios. O app altera apenas os municípios escolhidos."
            ),
            choices = NULL,
            selected = NULL,
            multiple = TRUE,
            options = list(
              plugins = list("remove_button"),
              placeholder = "Escolha um ou mais municípios"
            )
          ),

          div(
            class = "botoes-filtro",
            actionButton(
              inputId = "selecionar_todos_municipios",
              label = "Todos os Municípios",
              class = "btn-filtro"
            ),
            actionButton(
              inputId = "limpar_municipios",
              label = "Limpar",
              class = "btn-filtro-secundario"
            )
          ),

          radioButtons(
            inputId = "modo_simulacao",
            label = label_com_tooltip(
              "Tipo de simulação",
              "Escolha se deseja atribuir um novo ICA aos municípios ou aumentar/reduzir pontos percentuais."
            ),
            choices = c(
              "Aumentar/reduzir p.p." = "incrementar",
              "Atribuir novo ICA" = "substituir"
            ),
            selected = "incrementar"
          ),

          uiOutput("slider_valor_simulacao")
        )
      ),

      div(
        class = "conteudo-principal",

        div(
          class = "bloco-orientacao",

          div(
            class = "bloco-orientacao-icone",
            icon("circle-info")
          ),

          div(
            class = "bloco-orientacao-texto",
            div(
              class = "bloco-orientacao-titulo",
              "Como interpretar a simulação"
            ),
            div(
              class = "bloco-orientacao-descricao",
              "Selecione uma UF e, em seguida, escolha os municípios que deseja incluir na simulação. ",
              "O app recalcula o ICA da rede municipal para o grupo selecionado e mostra, de forma simples, como os valores simulados alteram o resultado desses municípios e quanto essa mudança influencia o ICA geral da UF."
            )
          )
        ),

        # ADD 3 cartoes - grupo

        layout_columns(
          col_widths = c(4, 4, 4),

          card_resultado(
            titulo = "ICA Observado - GRUPO",
            output_id = "card_grupo_ica_observado",
            tooltip = paste(
              "ICA estimado para o grupo de municípios selecionados antes da simulação (rede municipal).",
              "Quando houver apenas um município, aparecerá o valor do seu ICA (rede municipal)."
            )
          ),

          card_resultado(
            titulo = "ICA Simulado - GRUPO",
            output_id = "card_grupo_ica_simulado",
            classe = "verde",
            tooltip = paste(
              "ICA estimado para o grupo de municípios selecionados após a simulação (rede municipal).",
              "Quando houver apenas um município, aparecerá o valor do seu ICA (rede municipal)."
            )
          ),

          card_resultado(
            titulo = "Mudança - GRUPO",
            output_id = "card_grupo_mudanca",
            classe = "laranja",
            tooltip = "Diferença entre o ICA Simulado-GRUPO e o ICA Observado-GRUPO (em p.p)."
          )
        ),

        br(),

        layout_columns(
          col_widths = c(4, 4, 4),

          card_resultado(
            titulo = "ICA Observado - UF",
            output_id = "card_ica_observado",
            tooltip = "ICA estimado para UF (rede municipal)."
          ),

          card_resultado(
            titulo = "ICA Simulado - UF",
            output_id = "card_ica_simulado",
            classe = "verde",
            tooltip = "ICA estimado para UF (rede municipal) após simulação para municípios selecionados."
          ),

          card_resultado(
            titulo = "Mudança - UF",
            output_id = "card_mudanca",
            classe = "laranja",
            tooltip = "Diferença entre o ICA Simulado-UF e o ICA Observado-UF (em p.p)."
          )
        ),

        br(),

        layout_columns(
          col_widths = c(4, 4, 4),

          card_resultado(
            titulo = "Meta - UF",
            output_id = "card_meta",
            classe = "vermelho",
            tooltip = "Meta agregada do recorte para o ano escolhido, usando a mesma ponderação por alunos avaliados."
          ),

          card_resultado(
            titulo = "Distância - Antes da Simulação",
            output_id = "card_dist_antes",
            classe = "cinza",
            tooltip = "Diferença entre a Meta-UF e o ICA Observado-UF. Valor positivo indica o quanto falta para UF atingir a meta; valor negativo indica o quanto a UF já superou a meta."
          ),

          card_resultado(
            titulo = "Distância - Após a Simulação",
            output_id = "card_dist_depois",
            classe = "cinza",
            tooltip = "Diferença entre a Meta-UF e o ICA Simulado-UF. Valor positivo indica o quanto falta para UF atingir a meta; valor negativo indica o quanto a UF já superou a meta."
          )
        ),

        br(),

        box_conteudo(
          titulo = "ICA observado, simulado e meta da UF",
          #subtitulo = "O",
          conteudo = tagList(
            plotOutput("grafico_simulacao", height = "430px"),

            br(),

            div(
              class = "area-download",
              downloadButton(
                outputId = "baixar_grafico_png",
                label = "Baixar gráfico em PNG",
                class = "btn-download"
              )
            )
          )
        ),

        br(),

        box_conteudo(
          titulo = "Municípios simulados",
          subtitulo = paste(
            "A primeira linha apresenta o resultado consolidado do grupo de municípios selecionados.",
            "As demais linhas mostram os resultados de cada município, como seu peso no número de crianças avaliadas na UF",
            "e o impacto gerado pela simulação."
          ),
          conteudo = tagList(

            div(
              class = "barra-exportacao-tabela",

              downloadButton(
                outputId = "baixar_tabela_xlsx",
                label = "Baixar tabela em XLSX",
                class = "btn-download"
              ),

              div(
                id = "botoes_dt_tabela",
                class = "botoes-dt-tabela"
              )
            ),

            DTOutput("tabela_municipio")
          )
        )
      )
    )
  ),

  nav_spacer(),

  nav_item(
    div(
      class = "navbar-logo-custom",
      tags$img(
        src = "logo_app.png",
        alt = "Logo"
      )
    )
  )
)


# -----------------------------------------------------------------------------
# 5. Servidor
# -----------------------------------------------------------------------------

server <- function(input, output, session) {


  # ---------------------------------------------------------------------------
  # 5.1 Base filtrada por estado(s) e ano
  # ---------------------------------------------------------------------------

  dados_recorte_ano <- reactive({

    req(input$ufs)
    req(input$ano_ica)

    validate(
      need(length(input$ufs) > 0, "Selecione pelo menos um estado.")
    )

    dados |>
      filter(
        sigla_uf %in% input$ufs,
        ano == as.integer(input$ano_ica)
      )
  })


  # ---------------------------------------------------------------------------
  # 5.2 Botões dos estados
  # ---------------------------------------------------------------------------

  observeEvent(input$selecionar_todos_ufs, {

    updateSelectizeInput(
      session = session,
      inputId = "ufs",
      selected = sort(unique(dados$sigla_uf))
    )
  })

  observeEvent(input$limpar_ufs, {

    updateSelectizeInput(
      session = session,
      inputId = "ufs",
      selected = character(0)
    )
  })


  # ---------------------------------------------------------------------------
  # 5.3 Atualizar municípios quando estado(s) ou ano mudarem
  # ---------------------------------------------------------------------------

  observeEvent(dados_recorte_ano(), {

    base <- dados_recorte_ano()

    municipios <- base |>
      arrange(sigla_uf, no_municipio) |>
      distinct(sigla_uf, co_municipio, no_municipio) |>
      mutate(
        nome_exibicao = paste0(no_municipio, " (", sigla_uf, ")")
      )

    updateSelectizeInput(
      session = session,
      inputId = "municipios",
      choices = stats::setNames(
        municipios$co_municipio,
        municipios$nome_exibicao
      ),
      selected = municipios$co_municipio[1],
      server = TRUE
    )
  })


  # ---------------------------------------------------------------------------
  # 5.4 Botões dos municípios
  # ---------------------------------------------------------------------------

  observeEvent(input$selecionar_todos_municipios, {

    base <- dados_recorte_ano()

    municipios <- base |>
      distinct(co_municipio) |>
      pull(co_municipio)

    updateSelectizeInput(
      session = session,
      inputId = "municipios",
      selected = municipios
    )
  })

  observeEvent(input$limpar_municipios, {

    updateSelectizeInput(
      session = session,
      inputId = "municipios",
      selected = character(0)
    )
  })



  # ---------------------------------------------------------------------------
  # 5.5 Slider dinâmico conforme tipo de simulação
  # ---------------------------------------------------------------------------

  output$slider_valor_simulacao <- renderUI({

    req(input$modo_simulacao)

    if (input$modo_simulacao == "incrementar") {

      sliderInput(
        inputId = "valor_simulacao",
        label = label_com_tooltip(
          "Aumento/redução no ICA",
          "Informe quantos pontos percentuais serão somados ou subtraídos do ICA dos municípios selecionados."
        ),
        min = -20,
        max = 20,
        value = 5,
        step = 0.1,
        post = " p.p."
      )

    } else {

      sliderInput(
        inputId = "valor_simulacao",
        label = label_com_tooltip(
          "Novo ICA dos municípios selecionados",
          "Informe o novo valor de ICA que será atribuído aos municípios selecionados."
        ),
        min = 0,
        max = 100,
        value = 70,
        step = 0.1,
        post = "%"
      )
    }
  })


  # ---------------------------------------------------------------------------
  # 5.6 No modo "Atribuir novo ICA", iniciar slider no ICA médio do grupo
  # ---------------------------------------------------------------------------

  observeEvent(input$municipios, {

    req(input$municipios)
    req(input$modo_simulacao)

    if (input$modo_simulacao == "substituir") {

      base <- dados_recorte_ano() |>
        filter(co_municipio %in% input$municipios)

      ica_medio_grupo <- calcular_media_ponderada(
        data = base,
        var_valor = "ica",
        var_peso = "avaliados"
      )

      if (!is.na(ica_medio_grupo)) {
        updateSliderInput(
          session = session,
          inputId = "valor_simulacao",
          value = round(ica_medio_grupo * 100, 1)
        )
      }
    }
  })

  # ---------------------------------------------------------------------------
  # 5.7 Nome da coluna de meta
  # ---------------------------------------------------------------------------

  nome_meta <- reactive({
    paste0("meta_final_", input$ano_meta)
  })


  # ---------------------------------------------------------------------------
  # 5.8 Base simulada
  # ---------------------------------------------------------------------------

  dados_simulados <- reactive({

    req(input$municipios)
    req(input$valor_simulacao)
    req(input$modo_simulacao)

    validate(
      need(length(input$municipios) > 0, "Selecione pelo menos um município para simular.")
    )

    aplicar_simulacao_municipios(
      data = dados_recorte_ano(),
      municipios = input$municipios,
      valor_simulacao = input$valor_simulacao,
      modo_simulacao = input$modo_simulacao
    )
  })

  # add simulacao - por grupo
  # ---------------------------------------------------------------------------
  # Resumo dos municípios selecionados
  # ---------------------------------------------------------------------------

  resumo_grupo <- reactive({

    req(input$municipios)

    validate(
      need(
        length(input$municipios) > 0,
        "Selecione pelo menos um município."
      )
    )

    # Municípios selecionados antes da simulação
    base_grupo_observada <- dados_recorte_ano() |>
      dplyr::filter(
        co_municipio %in% input$municipios
      )

    # Os mesmos municípios após a simulação
    base_grupo_simulada <- dados_simulados() |>
      dplyr::filter(
        co_municipio %in% input$municipios
      )

    validate(
      need(
        nrow(base_grupo_observada) > 0,
        "Não há dados para os municípios selecionados."
      )
    )

    calcular_resumo_grupo(
      base_observada = base_grupo_observada,
      base_simulada = base_grupo_simulada
    )
  })


  # ---------------------------------------------------------------------------
  # 5.9 Resumo da simulação
  # ---------------------------------------------------------------------------

  resumo <- reactive({

    req(input$municipios)
    req(input$ano_meta)

    base_obs <- dados_recorte_ano()
    base_sim <- dados_simulados()

    validate(
      need(nrow(base_obs) > 0, "Não há dados para esse recorte e ano.")
    )

    validate(
      need(
        nome_meta() %in% names(base_obs),
        paste0("A coluna ", nome_meta(), " não existe na base.")
      )
    )

    ica_observado <- calcular_media_ponderada(
      data = base_obs,
      var_valor = "ica",
      var_peso = "avaliados"
    )

    ica_simulado <- calcular_media_ponderada(
      data = base_sim,
      var_valor = "ica",
      var_peso = "avaliados"
    )

    meta_recorte <- calcular_media_ponderada(
      data = base_obs,
      var_valor = nome_meta(),
      var_peso = "avaliados"
    )

    meta_recorte <- ajustar_escala_meta(meta_recorte)

    tibble(
      ica_observado = ica_observado,
      ica_simulado = ica_simulado,
      mudanca_estado = ica_simulado - ica_observado,
      meta_estado = meta_recorte,
      distancia_antes = meta_recorte - ica_observado,
      distancia_depois = meta_recorte - ica_simulado
    )
  })


  # ---------------------------------------------------------------------------
  # 5.10 Cards
  # ---------------------------------------------------------------------------

  # ---------------------------------------------------------------------------
  # Cards do grupo de municípios selecionados
  # ---------------------------------------------------------------------------
  # add - por grupos
  output$card_grupo_ica_observado <- renderText({

    fmt_pct(
      resumo_grupo()$ica_observado
    )
  })


  output$card_grupo_ica_simulado <- renderText({

    fmt_pct(
      resumo_grupo()$ica_simulado
    )
  })


  output$card_grupo_mudanca <- renderText({

    fmt_pp(
      resumo_grupo()$mudanca
    )
  })

  ###

  output$card_ica_observado <- renderText({
    fmt_pct(resumo()$ica_observado)
  })

  output$card_ica_simulado <- renderText({
    fmt_pct(resumo()$ica_simulado)
  })

  output$card_mudanca <- renderText({
    fmt_pp(resumo()$mudanca_estado)
  })

  output$card_meta <- renderText({
    fmt_pct(resumo()$meta_estado)
  })

  output$card_dist_antes <- renderText({
    fmt_pp(resumo()$distancia_antes)
  })

  output$card_dist_depois <- renderText({
    fmt_pp(resumo()$distancia_depois)
  })


  # ---------------------------------------------------------------------------
  # 5.11 Gráfico
  # ---------------------------------------------------------------------------

  grafico_pronto <- reactive({
    grafico_simulacao_ica(
      resumo = resumo(),
      size_rotulo = 12,
      base_size = 16,
      axis_text_x_size = 20,
      axis_text_y_size = 16,
      axis_title_y_size = 20,
      plot_margin = ggplot2::margin(18, 24, 14, 16)
    )
  })

  grafico_pronto_png <- reactive({
    grafico_simulacao_ica(
      resumo = resumo(),
      size_rotulo = 20,
      base_size = 46,
      axis_text_x_size = 44,
      axis_text_y_size = 42,
      axis_title_y_size = 46,
      plot_margin = ggplot2::margin(28, 32, 20, 22)
    )
  })

  output$grafico_simulacao <- renderPlot({
    grafico_pronto()
  })


  # ---------------------------------------------------------------------------
  # 5.12 Download do gráfico em PNG
  # ---------------------------------------------------------------------------

  output$baixar_grafico_png <- downloadHandler(

    filename = function() {

      ufs_nome <- if (length(input$ufs) > 3) {
        paste0(length(input$ufs), "_ufs")
      } else {
        paste(input$ufs, collapse = "_")
      }

      paste0(
        "grafico_ica_",
        ufs_nome, "_",
        input$ano_ica, "_meta_",
        input$ano_meta,
        ".png"
      )
    },

    content = function(file) {

      ggplot2::ggsave(
        filename = file,
        plot = grafico_pronto_png(),
        width = 10,
        height = 6,
        dpi = 300,
        units = "in",
        bg = "white"
      )
    }
  )


  # ---------------------------------------------------------------------------
  # 5.13 Tabela DT dos municípios simulados
  # ---------------------------------------------------------------------------

  # -----------------------------------------------------
  # 5.13.1 Tabela reativa dos municípios simulados
  # -----------------------------------------------------

  tabela_municipios_simulados <- reactive({

    req(input$municipios)
    req(input$valor_simulacao)
    req(input$modo_simulacao)

    # Garante que um ou vários municípios sejam sempre
    # tratados como um vetor simples de caracteres.
    municipios_selecionados <- as.character(
      unlist(
        input$municipios,
        use.names = FALSE
      )
    )

    validate(
      need(
        length(municipios_selecionados) > 0,
        "Selecione pelo menos um município."
      )
    )

    criar_tabela_municipios_simulados(
      base = dados_recorte_ano(),
      municipios = municipios_selecionados,
      valor_simulacao = as.numeric(input$valor_simulacao),
      modo_simulacao = input$modo_simulacao
    )
  })

  # ---------------------------------------------------------------------------
  # 5.13 Tabela DT dos municípios simulados
  # ---------------------------------------------------------------------------

  output$tabela_municipio <- DT::renderDT({

    tabela <- tabela_municipios_simulados()

    colunas_esperadas <- c(
      "UF",
      "Município",
      "Regional",
      "Matrículas",
      "Avaliados",
      "Peso na UF",
      "ICA observado",
      "ICA simulado",
      "Diferença (p.p.)",
      "Impacto na UF (p.p.)"
    )

    validate(
      need(
        identical(names(tabela), colunas_esperadas),
        paste0(
          "Estrutura incorreta da tabela. Colunas recebidas: ",
          paste(names(tabela), collapse = " | ")
        )
      )
    )

    validate(
      need(
        nrow(tabela) > 0,
        "Selecione pelo menos um município com dados disponíveis."
      )
    )

    # Segurança: a tabela enviada ao DataTables deve conter apenas
    # vetores atômicos, nunca colunas-lista.
    colunas_lista <- vapply(
      tabela,
      is.list,
      logical(1)
    )

    validate(
      need(
        !any(colunas_lista),
        paste0(
          "A tabela possui coluna do tipo lista: ",
          paste(
            names(tabela)[colunas_lista],
            collapse = ", "
          )
        )
      )
    )

    formato_exportacao_br <- DT::JS(
      "
      function(data, row, column, node) {
        return $(node).text().trim();
      }
      "
    )

    formatar_inteiro <- DT::JS(
      "
      function(data, type, row, meta) {
        if (type !== 'display') return data;
        if (data === null || data === '' || isNaN(Number(data))) return '-';

        return new Intl.NumberFormat(
          'pt-BR',
          {
            minimumFractionDigits: 0,
            maximumFractionDigits: 0
          }
        ).format(Number(data));
      }
      "
    )

    formatar_percentual <- DT::JS(
      "
      function(data, type, row, meta) {
        if (type !== 'display') return data;
        if (data === null || data === '' || isNaN(Number(data))) return '-';

        return new Intl.NumberFormat(
          'pt-BR',
          {
            minimumFractionDigits: 2,
            maximumFractionDigits: 2
          }
        ).format(Number(data) * 100) + '%';
      }
      "
    )

    formatar_pp <- DT::JS(
      "
      function(data, type, row, meta) {
        if (type !== 'display') return data;
        if (data === null || data === '' || isNaN(Number(data))) return '-';

        return new Intl.NumberFormat(
          'pt-BR',
          {
            minimumFractionDigits: 2,
            maximumFractionDigits: 2
          }
        ).format(Number(data));
      }
      "
    )

    DT::datatable(
      data = tabela,
      rownames = FALSE,
      extensions = c("Buttons", "Responsive"),
      class = "stripe hover compact nowrap",
      escape = TRUE,
      width = '100%',

      callback = DT::JS(
        "
        table.on('init.dt', function() {

          var areaBotoesDT =
            $(table.table().container()).find('.dt-buttons');

          var destino = $('#botoes_dt_tabela');

          if (areaBotoesDT.length && destino.length) {
            destino.empty();
            areaBotoesDT.detach().appendTo(destino);
          }

        });
        "
      ),


      options = list(
        dom = "Bfrtip",

        buttons = list(
          list(
            extend = "copyHtml5",
            text = '<i class=\"fa fa-copy\"></i> Copy',
            className = "btn-tabela-export",
            exportOptions = list(
              format = list(
                body = formato_exportacao_br
              )
            )
          ),
          list(
            extend = "csvHtml5",
            text = '<i class=\"fa fa-file-text\"></i> CSV',
            className = "btn-tabela-export",
            fieldSeparator = ";",
            bom = TRUE,
            exportOptions = list(
              format = list(
                body = formato_exportacao_br
              )
            )
          )
        ),

        pageLength = 20,

        lengthMenu = list(
          c(20, 50, 100),
          c("20", "50", "100")
        ),

        searching = TRUE,
        ordering = TRUE,
        responsive = TRUE,
        autoWidth = FALSE,

        createdRow = DT::JS(
          "
          function(row, data, dataIndex) {

            if (data[0] === 'Grupo' && data[1] === 'Grupo') {
              $(row).addClass('linha-grupo');
            }

          }
          "
        ),

        language = list(
          url = paste0(
            "//cdn.datatables.net/plug-ins/",
            "1.13.6/i18n/pt-BR.json"
          )
        ),

        columnDefs = list(
          # UF, Município e Regional
          list(
            targets = c(0, 1, 2),
            className = "dt-left"
          ),

          # Matrículas e Avaliados
          list(
            targets = c(3, 4),
            className = "dt-center",
            render = formatar_inteiro
          ),

          # Peso na UF e ICA observado
          list(
            targets = c(5, 6),
            className = "dt-center",
            render = formatar_percentual
          ),

          # ICA simulado
          list(
            targets = 7,
            className = "dt-center coluna-ica-simulado",
            render = formatar_percentual
          ),

          # Diferença
          list(
            targets = 8,
            className = "dt-center",
            render = formatar_pp
          ),

          # Impacto na UF
          list(
            targets = 9,
            className = "dt-center coluna-impacto",
            render = formatar_pp
          )
        )
      )
    )

  }, server = FALSE)



  # ---------------------------------------------------------------------------
  # 5.14 Download profissional da tabela em XLSX
  # ---------------------------------------------------------------------------

  output$baixar_tabela_xlsx <- downloadHandler(

    filename = function() {

      ufs_nome <- if (length(input$ufs) > 3) {
        paste0(length(input$ufs), "_ufs")
      } else {
        paste(input$ufs, collapse = "_")
      }

      paste0(
        "municipios_simulados_ica_",
        ufs_nome, "_",
        input$ano_ica, "_meta_",
        input$ano_meta,
        ".xlsx"
      )
    },

    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",

    content = function(file) {

      tabela <- tabela_municipios_simulados()

      writexl::write_xlsx(
        x = list(
          "Municipios simulados" = tabela
        ),
        path = file
      )
    }
  )
}
# -----------------------------------------------------------------------------
# 6. Rodar app
# -----------------------------------------------------------------------------

shinyApp(ui = ui, server = server)
