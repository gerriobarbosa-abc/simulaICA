
# =============================================================================
# CONFIGURAÇÕES GERAIS DO APP
# =============================================================================

# -----------------------------------------------------------------------------
# Cores da marca
# -----------------------------------------------------------------------------

cor_vermelho <- "#d44856"
cor_laranja  <- "#f8b221"
cor_verde    <- "#1f7d55"
cor_azul     <- "#396496"
cor_cinza    <- "#696a67"

cor_fundo    <- "#f7f8fa"
cor_texto    <- "#292820"
cor_branco   <- "#ffffff"


# -----------------------------------------------------------------------------
# Tema visual com bslib
# -----------------------------------------------------------------------------

tema_app <- bslib::bs_theme(
  version = 5,
  bg = cor_fundo,
  fg = cor_texto,
  primary = cor_azul,
  secondary = cor_cinza,
  success = cor_verde,
  warning = cor_laranja,
  danger = cor_vermelho,
  base_font = bslib::font_google("Nunito")
)


# -----------------------------------------------------------------------------
# Fonte Nunito para gráficos ggplot2
# -----------------------------------------------------------------------------

if (requireNamespace("sysfonts", quietly = TRUE) &&
    requireNamespace("showtext", quietly = TRUE)) {

  sysfonts::font_add_google(
    name = "Nunito",
    family = "nunito"
  )

  showtext::showtext_auto()
}
