# =============================================================================
# WorknoiseR - Aplicación unificada bilingüe v0.5
# Integra:
#   - Cálculos de exposición al ruido (base v06)
#   - Mapas de ruido laboral (base v14.7)
# =============================================================================

.wn_boot_lang <- tolower(Sys.getenv("WORKNOISER_LANG", "es"))
if (!.wn_boot_lang %in% c("es", "en")) .wn_boot_lang <- "es"

.required_packages <- c(
  "shiny", "rhandsontable", "ggplot2", "plotly", "reshape2",
  "viridisLite", "scales", "sf"
)

.missing_packages <- .required_packages[!vapply(
  .required_packages, requireNamespace, logical(1), quietly = TRUE
)]

if (length(.missing_packages)) {
  .prefix <- if (.wn_boot_lang == "en") "Required packages are missing to run WorknoiseR: " else "Faltan paquetes obligatorios para ejecutar WorknoiseR: "
  .install <- if (.wn_boot_lang == "en") ".\nInstall them with:\ninstall.packages(c(" else ".\nInstálelos con:\ninstall.packages(c("
  stop(
    paste0(
      .prefix, paste(.missing_packages, collapse = ", "), .install,
      paste(sprintf('"%s"', .missing_packages), collapse = ", "), "))"
    ), call. = FALSE
  )
}

library(shiny)

options(shiny.maxRequestSize = 50 * 1024^2)

# Los dos desarrollos se cargan en entornos independientes. De esta forma
# conservan sus funciones y reactivos originales sin mezclarlos entre sí.
calc_env <- new.env(parent = globalenv())
map_env  <- new.env(parent = globalenv())

# Capa computacional independiente de Shiny. Se precarga en ambos entornos;
# los módulos conservan sus nombres y reactivos originales para mantener
# compatibilidad durante la transición a paquete R.
.core_files <- c(
  "00_constants.R", "05_i18n.R", "10_exposure.R", "20_protection.R",
  "30_spatial.R", "40_import.R"
)
for (.f in .core_files) {
  .p <- file.path("R", .f)
  if (!file.exists(.p)) stop("Falta el componente del motor WorknoiseR: ", .p, call. = FALSE)
  sys.source(.p, envir = calc_env)
  sys.source(.p, envir = map_env)
}

sys.source(file.path("modules", "calculos_app.R"), envir = calc_env)
sys.source(file.path("modules", "mapas_app.R"), envir = map_env)

if (!exists("ui", envir = calc_env) || !exists("server", envir = calc_env)) {
  stop("No se pudo cargar el módulo de cálculos de ruido.", call. = FALSE)
}
if (!exists("ui", envir = map_env) || !exists("server", envir = map_env)) {
  stop("No se pudo cargar el módulo de mapas de ruido.", call. = FALSE)
}


.wn_initial_lang <- getOption("worknoiser.language", Sys.getenv("WORKNOISER_LANG", "es"))
.wn_initial_lang <- tolower(as.character(.wn_initial_lang)[1])
if (!.wn_initial_lang %in% c("es", "en")) .wn_initial_lang <- "es"

home_ui <- div(
  class = "wn-home",
  div(
    class = "wn-home-head",
    img(src = "worknoiser_logo.png", class = "wn-logo", alt = "WorknoiseR"),
    div(
      h1("WorknoiseR"),
      p("Occupational Noise Exposure — Assessment & Spatial Mapping")
    )
  ),
  div(
    class = "wn-intro",
    h3("¿Qué desea realizar?"),
    p("Seleccione el módulo de trabajo. Ambos forman parte de la misma aplicación y comparten la identidad WorknoiseR.")
  ),
  fluidRow(
    column(
      6,
      div(
        class = "wn-choice-card",
        div(class = "wn-icon", HTML("&#8721;")),
        h2("Cálculos de ruido"),
        p("Evaluación de exposición, LAeq,T, LAeq,d, exposición semanal, dosis, TMA, bandas de octava, protectores auditivos y campañas masivas de trabajadores."),
        actionButton("go_calculos", "Entrar en Cálculos", class = "btn-primary btn-lg wn-main-button")
      )
    ),
    column(
      6,
      div(
        class = "wn-choice-card",
        div(class = "wn-icon", HTML("&#9678;")),
        h2("Mapas de ruido"),
        p("Diseño o importación de planos, DWG/DXF, geometría espacial, fuentes, receptores, paredes, reverberación, mapas 2D, isófonas y representación 3D."),
        actionButton("go_mapas", "Entrar en Mapas", class = "btn-primary btn-lg wn-main-button")
      )
    )
  ),
  div(
    class = "wn-footer-note",
    "WorknoiseR integra cálculo higiénico y modelización espacial en una única interfaz Shiny."
  )
)

ui <- fluidPage(
  tags$head(
    tags$title("WorknoiseR"),
    tags$script(src = "worknoiser_i18n.js"),
    tags$script(HTML(sprintf("document.addEventListener(\'DOMContentLoaded\', function(){ if(window.WorknoiseRI18n){ WorknoiseRI18n.init(\'%s\'); } });", .wn_initial_lang))),
    tags$style(HTML("\n      body { background:#f5f7fa; }\n      .wn-shell { max-width: 1800px; margin: 0 auto; }\n      .wn-home { max-width: 1180px; margin: 28px auto; padding: 22px; }\n      .wn-home-head { display:flex; align-items:center; gap:22px; margin-bottom:20px; }\n      .wn-logo { width:130px; height:130px; object-fit:contain; }\n      .wn-home-head h1 { margin:0; font-size:46px; font-weight:750; color:#072f63; }\n      .wn-home-head p { margin:5px 0 0; color:#277e8d; font-size:18px; }\n      .wn-intro { background:white; border:1px solid #e1e7ef; border-radius:14px; padding:18px 22px; margin-bottom:22px; box-shadow:0 2px 8px rgba(16,24,40,.04); }\n      .wn-intro h3 { margin-top:0; color:#172b4d; }\n      .wn-choice-card { min-height:315px; background:white; border:1px solid #dde5ee; border-radius:16px; padding:28px; margin-bottom:20px; box-shadow:0 4px 15px rgba(16,24,40,.06); }\n      .wn-choice-card h2 { color:#07366c; font-weight:700; margin-top:10px; }\n      .wn-choice-card p { color:#526071; font-size:16px; line-height:1.55; min-height:100px; }\n      .wn-icon { width:58px; height:58px; display:flex; align-items:center; justify-content:center; border-radius:14px; background:#e9f7f8; color:#168a99; font-size:34px; font-weight:700; }\n      .wn-main-button { min-width:210px; font-weight:650; }\n      .wn-module-toolbar { background:#ffffff; border-bottom:1px solid #dfe5ec; padding:10px 15px; margin:0 -15px 14px -15px; position:sticky; top:0; z-index:1000; }\n      .wn-module-toolbar .btn { font-weight:600; }\n      .wn-module-title { display:inline-block; margin-left:12px; font-size:17px; font-weight:650; color:#344054; vertical-align:middle; }\n      .wn-footer-note { color:#667085; text-align:center; margin-top:18px; }\n      .wn-language-bar { display:flex; justify-content:flex-end; align-items:center; gap:8px; padding:10px 15px 0 15px; }\n      .wn-language-bar .form-group { margin-bottom:0; }\n      @media (max-width: 768px) {\n        .wn-home-head { flex-direction:column; text-align:center; }\n        .wn-home-head h1 { font-size:36px; }\n        .wn-choice-card { min-height:0; }\n      }\n    "))
  ),

  div(
    class = "wn-shell",
    div(class = "wn-language-bar", selectInput("wn_language", "Idioma / Language", choices = c("Español" = "es", "English" = "en"), selected = .wn_initial_lang, width = "170px")),
    tabsetPanel(
      id = "worknoiser_mode",
      type = "hidden",
      selected = "inicio",

      tabPanel(
        title = "Inicio",
        value = "inicio",
        home_ui
      ),

      tabPanel(
        title = "Cálculos",
        value = "calculos",
        div(
          class = "wn-module-toolbar",
          actionButton("home_from_calculos", "← Inicio", class = "btn-default"),
          span(class = "wn-module-title", "WorknoiseR · Cálculos de ruido")
        ),
        calc_env$ui
      ),

      tabPanel(
        title = "Mapas",
        value = "mapas",
        div(
          class = "wn-module-toolbar",
          actionButton("home_from_mapas", "← Inicio", class = "btn-default"),
          span(class = "wn-module-title", "WorknoiseR · Mapas de ruido")
        ),
        map_env$ui
      )
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$wn_language, {
    if (input$wn_language %in% c("es", "en")) options(worknoiser.language = input$wn_language)
  }, ignoreInit = FALSE)

  # Arranca ambos motores dentro de la misma sesión Shiny. Los scripts se han
  # aislado en entornos independientes y el único ID común se ha renombrado
  # en el módulo de mapas para evitar colisiones.
  calc_env$server(input, output, session)
  map_env$server(input, output, session)

  observeEvent(input$go_calculos, {
    updateTabsetPanel(session, "worknoiser_mode", selected = "calculos")
  })

  observeEvent(input$go_mapas, {
    updateTabsetPanel(session, "worknoiser_mode", selected = "mapas")
  })

  observeEvent(input$home_from_calculos, {
    updateTabsetPanel(session, "worknoiser_mode", selected = "inicio")
  })

  observeEvent(input$home_from_mapas, {
    updateTabsetPanel(session, "worknoiser_mode", selected = "inicio")
  })
}

shinyApp(ui = ui, server = server)
