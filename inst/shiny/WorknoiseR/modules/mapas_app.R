# ============================================================================
# Generador de mapas de ruido - v14.7
# Aplicación Shiny autocontenida. Plano importado opcional.
# v14.7: añade reinicio seguro de plano/mapa para cargar sucesivos planos sin arrastrar estado reactivo.
# ============================================================================

.required_packages <- c(
  "shiny", "ggplot2", "plotly", "reshape2",
  "viridisLite", "scales", "rhandsontable", "sf"
)

.missing_packages <- .required_packages[!vapply(
  .required_packages,
  requireNamespace,
  logical(1),
  quietly = TRUE
)]

if (length(.missing_packages)) {
  stop(
    paste0(
      "Faltan paquetes obligatorios para ejecutar la aplicación: ",
      paste(.missing_packages, collapse = ", "),
      ".\nInstálelos con:\ninstall.packages(c(",
      paste(sprintf('\"%s\"', .missing_packages), collapse = ", "),
      "))"
    ),
    call. = FALSE
  )
}

library(shiny)
library(ggplot2)
library(plotly)
library(reshape2)
library(viridisLite)
library(scales)
library(rhandsontable)
library(sf)

# Permite planos de tamaño moderado en la interfaz Shiny (50 MB).
options(shiny.maxRequestSize = 50 * 1024^2)

message("Mapa de ruido v14.7 cargado correctamente.")

# -----------------------------------------------------------------------------
# Funciones auxiliares
# -----------------------------------------------------------------------------

db_sum <- function(x) db_add(x)

# Devuelve un valor de entrada Shiny cuando existe y es válido; en caso
# contrario usa un valor por defecto. Es esencial para controles dinámicos
# situados en pestañas todavía no visitadas.
input_or <- function(x, default) {
  if (is.null(x) || length(x) == 0) return(default)
  if (is.numeric(default)) {
    y <- suppressWarnings(as.numeric(x[1]))
    if (!is.finite(y)) return(default)
    return(y)
  }
  y <- as.character(x[1])
  if (is.na(y) || !nzchar(y)) return(default)
  y
}

level_at_distance_free_field <- function(level_1m, distance_m) {
  if (length(level_1m) != length(distance_m) && length(level_1m) != 1 && length(distance_m) != 1) return(NA_real_)
  if (any(!is.finite(level_1m)) || any(!is.finite(distance_m)) || any(distance_m <= 0)) return(NA_real_)
  level_1m - 20 * log10(distance_m)
}

distance_for_level_free_field <- function(level_1m, target_level) {
  if (any(!is.finite(level_1m)) || !is.finite(target_level)) return(rep(NA_real_, length(level_1m)))
  pmax(1, 10^((level_1m - target_level) / 20))
}


# -----------------------------------------------------------------------------
# Funciones auxiliares para importación de planos
# -----------------------------------------------------------------------------

normalizar_nombres <- function(x) {
  x <- tolower(trimws(x))
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  gsub("[^a-z0-9_]+", "_", x)
}


# Clasificación heurística de capas CAD para una vista arquitectónica limpia.
# No elimina ninguna capa: solo decide qué conviene mostrar inicialmente.
es_capa_auxiliar <- function(capas) {
  n <- normalizar_nombres(capas)
  patron <- paste(c(
    "cota", "acot", "dim", "dimension", "text", "texto", "annotation", "anotacion",
    "electric", "electr", "fontan", "sanitar", "hatch", "sombreado", "cajetin", "title",
    "letrero", "furniture", "mobiliario", "equipo", "equipos", "campana", "toldo",
    "inst_", "instal", "control", "_kti", "barra", "booth", "lounge"
  ), collapse = "|")
  grepl(patron, n, ignore.case = TRUE)
}

es_capa_arquitectonica <- function(capas) {
  n <- normalizar_nombres(capas)
  patron <- paste(c(
    "muro", "wall", "perimet", "columna", "column", "puerta", "door", "ventana", "window",
    "cancel", "herreria", "cerramiento", "tablaroca", "tablar_roca", "cto_frio"
  ), collapse = "|")
  grepl(patron, n, ignore.case = TRUE) & !es_capa_auxiliar(capas)
}

seleccion_capas_recomendada <- function(capas) {
  arq <- capas[es_capa_arquitectonica(capas)]
  if (length(arq)) return(arq)
  base <- capas[!es_capa_auxiliar(capas)]
  if (length(base)) return(base)
  capas
}

# Capas que se ocultan inicialmente solo por ser anotaciones/documentacion.
# A diferencia de es_capa_auxiliar(), aqui NO se excluyen mobiliario, equipos,
# barras, booths, etc.: forman parte del plano que el usuario espera ver.
es_capa_solo_anotacion <- function(capas) {
  n <- normalizar_nombres(capas)
  patron <- paste(c(
    "cota", "acot", "dim", "dimension", "cajetin", "title", "control", "_kti"
  ), collapse = "|")
  grepl(patron, n, ignore.case = TRUE)
}

seleccion_capas_visual_recomendada <- function(capas) {
  det <- capas[!es_capa_solo_anotacion(capas)]
  if (length(det)) return(det)
  capas
}

# Capas que, por su nombre, son candidatas a comportarse como barreras
# acústicas. Se excluyen puertas porque pueden constituir huecos; el usuario
# puede añadirlas manualmente si desea modelarlas como cerradas.
es_capa_barrera_acustica <- function(capas) {
  n <- normalizar_nombres(capas)
  patron_barrera <- paste(c(
    "muro", "wall", "perimet", "tablaroca", "tablar_roca", "cto_frio",
    "cerramiento", "partition", "particion"
  ), collapse = "|")
  patron_hueco <- paste(c("puerta", "door", "hueco", "opening"), collapse = "|")
  grepl(patron_barrera, n, ignore.case = TRUE) & !grepl(patron_hueco, n, ignore.case = TRUE)
}

# Convierte paths CAD en segmentos consecutivos.
segmentos_desde_paths <- function(df) {
  if (is.null(df) || !nrow(df)) return(data.frame())
  df <- df[is.finite(df$x) & is.finite(df$y), , drop = FALSE]
  if (!nrow(df)) return(data.frame())
  grupos <- split(df, df$group)
  out <- lapply(grupos, function(g) {
    if (nrow(g) < 2) return(NULL)
    data.frame(
      x1 = g$x[-nrow(g)], y1 = g$y[-nrow(g)],
      x2 = g$x[-1],       y2 = g$y[-1],
      layer = g$layer[-1],
      group = g$group[-1],
      stringsAsFactors = FALSE
    )
  })
  out <- Filter(Negate(is.null), out)
  if (!length(out)) return(data.frame())
  seg <- do.call(rbind, out)
  long <- sqrt((seg$x2 - seg$x1)^2 + (seg$y2 - seg$y1)^2)
  seg <- seg[is.finite(long) & long > 1e-4, , drop = FALSE]
  rownames(seg) <- NULL
  seg
}

# Devuelve las posiciones, medidas sobre el rayo fuente-receptor, donde éste
# corta segmentos de barrera. Las intersecciones muy próximas se agrupan para
# no contar dos veces las dos caras de un mismo muro dibujado con espesor.
intersecciones_rayo_barreras <- function(sx, sy, rx, ry, seg, agrupar_m = 0.30) {
  if (is.null(seg) || !nrow(seg) || !all(is.finite(c(sx, sy, rx, ry)))) return(numeric())
  dx <- rx - sx; dy <- ry - sy
  L <- sqrt(dx^2 + dy^2)
  if (!is.finite(L) || L < 1e-8) return(numeric())

  wx <- seg$x2 - seg$x1
  wy <- seg$y2 - seg$y1
  den <- dx * wy - dy * wx
  okden <- is.finite(den) & abs(den) > 1e-10
  if (!any(okden)) return(numeric())

  qx <- seg$x1 - sx
  qy <- seg$y1 - sy
  t <- rep(NA_real_, nrow(seg))
  u <- rep(NA_real_, nrow(seg))
  t[okden] <- (qx[okden] * wy[okden] - qy[okden] * wx[okden]) / den[okden]
  u[okden] <- (qx[okden] * dy - qy[okden] * dx) / den[okden]

  # Se excluyen contactos exactamente en fuente/receptor y extremos del muro
  # para reducir dobles conteos en vértices compartidos.
  eps <- 1e-6
  hit <- okden & t > eps & t < (1 - eps) & u > eps & u < (1 - eps)
  tt <- sort(t[hit])
  if (!length(tt)) return(numeric())

  d <- tt * L
  agrupar_m <- if (is.finite(agrupar_m) && agrupar_m >= 0) agrupar_m else 0.30
  if (length(d) == 1 || agrupar_m <= 0) return(d)
  grupos <- cumsum(c(TRUE, diff(d) > agrupar_m))
  as.numeric(tapply(d, grupos, mean))
}

contar_cruces_barrera <- function(sx, sy, rx, ry, seg, agrupar_m = 0.30) {
  length(intersecciones_rayo_barreras(sx, sy, rx, ry, seg, agrupar_m))
}


# Detalle de los segmentos realmente intersectados por un rayo. Se conserva
# un representante por grupo de intersecciones próximas para evitar contar
# dos caras paralelas del mismo muro como dos obstáculos independientes.
intersecciones_rayo_barreras_detalle <- function(sx, sy, rx, ry, seg, agrupar_m = 0.30) {
  if (is.null(seg) || !nrow(seg) || !all(is.finite(c(sx, sy, rx, ry)))) return(data.frame())
  dx <- rx - sx; dy <- ry - sy
  L <- sqrt(dx^2 + dy^2)
  if (!is.finite(L) || L < 1e-8) return(data.frame())

  wx <- seg$x2 - seg$x1
  wy <- seg$y2 - seg$y1
  den <- dx * wy - dy * wx
  okden <- is.finite(den) & abs(den) > 1e-10
  if (!any(okden)) return(data.frame())

  qx <- seg$x1 - sx
  qy <- seg$y1 - sy
  tt <- rep(NA_real_, nrow(seg))
  uu <- rep(NA_real_, nrow(seg))
  tt[okden] <- (qx[okden] * wy[okden] - qy[okden] * wx[okden]) / den[okden]
  uu[okden] <- (qx[okden] * dy - qy[okden] * dx) / den[okden]

  eps <- 1e-6
  hit <- okden & tt > eps & tt < (1 - eps) & uu > eps & uu < (1 - eps)
  idx <- which(hit)
  if (!length(idx)) return(data.frame())

  d <- tt[idx] * L
  ord <- order(d)
  idx <- idx[ord]
  d <- d[ord]

  agrupar_m <- if (is.finite(agrupar_m) && agrupar_m >= 0) agrupar_m else 0.30
  if (length(d) > 1 && agrupar_m > 0) {
    grupos <- cumsum(c(TRUE, diff(d) > agrupar_m))
    keep <- vapply(split(seq_along(d), grupos), function(ii) ii[ceiling(length(ii) / 2)], integer(1))
    idx <- idx[keep]
    d <- d[keep]
  }

  data.frame(
    idx = idx,
    d = d,
    x_int = sx + (d / L) * dx,
    y_int = sy + (d / L) * dy,
    stringsAsFactors = FALSE
  )
}

# Atenuación acumulada por varios cerramientos sin permitir que el modelo
# produzca caídas artificialmente ilimitadas. El primer cerramiento domina;
# los adicionales aportan una penalización menor hasta un máximo configurable.
perdida_pared_saturada <- function(n, primera = 12, adicional = 6, maxima = 30) {
  if (!is.finite(n) || n <= 0) return(0)
  primera <- max(0, primera)
  adicional <- max(0, adicional)
  maxima <- max(primera, maxima)
  min(maxima, primera + adicional * max(0, n - 1))
}

# Aproximación geométrica del camino que rodea el extremo de un cerramiento.
# No pretende sustituir a un ray-tracing de sala: evita el salto binario
# "visible/bloqueado" del modelo por cruces y representa una contribución
# difractada/de borde que cambia suavemente con la posición del receptor.
nivel_borde_aproximado <- function(sx, sy, rx, ry, Li, seg, hits,
                                   perdida_borde = 6,
                                   penalizacion_otras = 3) {
  if (is.null(hits) || !nrow(hits) || is.null(seg) || !nrow(seg)) return(NA_real_)
  r0 <- sqrt((rx - sx)^2 + (ry - sy)^2)
  if (!is.finite(r0) || r0 <= 0) return(NA_real_)

  # Se prueban los extremos de los tres primeros cerramientos interceptados.
  ids <- unique(hits$idx[seq_len(min(3L, nrow(hits)))])
  candidatos <- numeric()
  for (j in ids) {
    extremos <- rbind(
      c(seg$x1[j], seg$y1[j]),
      c(seg$x2[j], seg$y2[j])
    )
    for (k in seq_len(nrow(extremos))) {
      ex <- extremos[k, 1]; ey <- extremos[k, 2]
      d1 <- sqrt((ex - sx)^2 + (ey - sy)^2)
      d2 <- sqrt((rx - ex)^2 + (ry - ey)^2)
      camino <- d1 + d2
      if (!is.finite(camino) || camino < 1) next

      # El exceso de trayecto introduce una pérdida progresiva, sin generar
      # cuñas infinitamente profundas detrás del extremo del muro.
      exceso <- max(0, camino - r0)
      perdida_exceso <- 10 * log10(1 + exceso / 0.25)
      extra <- penalizacion_otras * max(0, nrow(hits) - 1)
      candidatos <- c(
        candidatos,
        Li - 20 * log10(camino) - max(0, perdida_borde) - perdida_exceso - extra
      )
    }
  }
  if (!length(candidatos)) return(NA_real_)
  max(candidatos, na.rm = TRUE)
}

# Suavizado exclusivamente visual en dominio energético. No modifica los
# niveles usados para trabajadores, dosis, estimación inversa ni tablas.
suavizar_campo_energia <- function(puntos, iteraciones = 1L) {
  if (is.null(puntos) || !nrow(puntos) || !"Lp" %in% names(puntos)) return(puntos)
  iteraciones <- max(0L, min(4L, as.integer(iteraciones)))
  if (iteraciones == 0L) {
    puntos$Lp_plot <- puntos$Lp
    return(puntos)
  }

  xs <- sort(unique(puntos$x))
  ys <- sort(unique(puntos$y))
  if (length(xs) < 2 || length(ys) < 2) {
    puntos$Lp_plot <- puntos$Lp
    return(puntos)
  }

  iz <- match(puntos$y, ys)
  jz <- match(puntos$x, xs)
  M <- matrix(NA_real_, nrow = length(ys), ncol = length(xs))
  M[cbind(iz, jz)] <- 10^(puntos$Lp / 10)

  # Kernel gaussiano discreto 3x3.
  K <- matrix(c(1,2,1, 2,4,2, 1,2,1), nrow = 3, byrow = TRUE)
  K <- K / sum(K)

  for (it in seq_len(iteraciones)) {
    num <- matrix(0, nrow(M), ncol(M))
    den <- matrix(0, nrow(M), ncol(M))
    for (di in -1:1) {
      for (dj in -1:1) {
        w <- K[di + 2, dj + 2]
        r_src <- seq_len(nrow(M))
        c_src <- seq_len(ncol(M))
        r_dst <- r_src + di
        c_dst <- c_src + dj
        okr <- r_dst >= 1 & r_dst <= nrow(M)
        okc <- c_dst >= 1 & c_dst <= ncol(M)
        if (!any(okr) || !any(okc)) next
        rs <- r_src[okr]; rd <- r_dst[okr]
        cs <- c_src[okc]; cd <- c_dst[okc]
        block <- M[rs, cs, drop = FALSE]
        valid <- is.finite(block)
        num[rd, cd] <- num[rd, cd] + w * ifelse(valid, block, 0)
        den[rd, cd] <- den[rd, cd] + w * valid
      }
    }
    M <- ifelse(den > 0, num / den, NA_real_)
  }

  E <- M[cbind(iz, jz)]
  puntos$Lp_plot <- ifelse(is.finite(E) & E > 0, 10 * log10(E), puntos$Lp)
  puntos
}

# Detecta el conjunto espacial principal del dibujo arquitectónico sin borrar
# entidades del CAD. El algoritmo trabaja a nivel de grupos y usa proximidad
# entre cajas envolventes; así evita que unas pocas entidades aisladas fuercen
# el encuadre y las dimensiones del modelo.
detectar_edificio_principal <- function(df) {
  if (is.null(df) || !nrow(df)) return(NULL)

  d <- df
  capas <- unique(d$layer)
  arq <- capas[es_capa_arquitectonica(capas)]
  if (length(arq)) d <- d[d$layer %in% arq, , drop = FALSE]

  if ("geom_type" %in% names(d)) {
    dl <- d[!toupper(d$geom_type) %in% c("POINT", "MULTIPOINT"), , drop = FALSE]
    if (nrow(dl)) d <- dl
  }
  d <- d[is.finite(d$x) & is.finite(d$y) & !is.na(d$group), , drop = FALSE]
  if (!nrow(d)) return(NULL)

  sp <- split(d, d$group)
  stats <- do.call(rbind, lapply(names(sp), function(g) {
    z <- sp[[g]]
    data.frame(
      group = g,
      xmin = min(z$x, na.rm = TRUE),
      xmax = max(z$x, na.rm = TRUE),
      ymin = min(z$y, na.rm = TRUE),
      ymax = max(z$y, na.rm = TRUE),
      n = nrow(z),
      stringsAsFactors = FALSE
    )
  }))
  if (is.null(stats) || !nrow(stats)) return(NULL)

  if (nrow(stats) == 1L) {
    bb <- c(xmin = stats$xmin, xmax = stats$xmax, ymin = stats$ymin, ymax = stats$ymax)
    return(list(groups = stats$group, bbox = bb, n_groups = 1L, excluded_groups = 0L, score = stats$n))
  }

  # Distancia mínima entre cajas de cada grupo.
  n <- nrow(stats)
  dm <- matrix(Inf, n, n)
  for (i in seq_len(n - 1L)) {
    for (j in (i + 1L):n) {
      gx <- max(0, max(stats$xmin[i], stats$xmin[j]) - min(stats$xmax[i], stats$xmax[j]))
      gy <- max(0, max(stats$ymin[i], stats$ymin[j]) - min(stats$ymax[i], stats$ymax[j]))
      dij <- sqrt(gx^2 + gy^2)
      dm[i, j] <- dij
      dm[j, i] <- dij
    }
  }

  nn <- apply(dm, 1, min, na.rm = TRUE)
  nn_pos <- nn[is.finite(nn) & nn > 0]
  xr <- range(c(stats$xmin, stats$xmax), na.rm = TRUE)
  yr <- range(c(stats$ymin, stats$ymax), na.rm = TRUE)
  diag_global <- sqrt(diff(xr)^2 + diff(yr)^2)

  base <- if (length(nn_pos)) stats::median(nn_pos) else 0
  umbral <- max(base * 4, diag_global * 0.008, .Machine$double.eps)
  if (is.finite(diag_global) && diag_global > 0) {
    umbral <- min(umbral, diag_global * 0.08)
  }

  adj <- dm <= umbral
  diag(adj) <- TRUE
  visitado <- rep(FALSE, n)
  componentes <- list()

  for (i in seq_len(n)) {
    if (visitado[i]) next
    cola <- i
    comp <- integer()
    visitado[i] <- TRUE
    while (length(cola)) {
      k <- cola[1]
      cola <- cola[-1]
      comp <- c(comp, k)
      vecinos <- which(adj[k, ] & !visitado)
      if (length(vecinos)) {
        visitado[vecinos] <- TRUE
        cola <- c(cola, vecinos)
      }
    }
    componentes[[length(componentes) + 1L]] <- comp
  }

  # Prima el componente con mayor cantidad de vértices; en empate, más grupos.
  puntuacion <- vapply(componentes, function(ix) sum(stats$n[ix]), numeric(1))
  ngr <- vapply(componentes, length, integer(1))
  best <- order(puntuacion, ngr, decreasing = TRUE)[1]
  idx <- componentes[[best]]
  grupos <- stats$group[idx]

  core <- d[d$group %in% grupos, , drop = FALSE]
  bb <- c(
    xmin = min(core$x, na.rm = TRUE),
    xmax = max(core$x, na.rm = TRUE),
    ymin = min(core$y, na.rm = TRUE),
    ymax = max(core$y, na.rm = TRUE)
  )

  # Salvaguarda robusta si el componente espacial queda excesivamente pequeño.
  if (sum(stats$n[idx]) < 0.35 * sum(stats$n) && nrow(stats) >= 8) {
    cx <- (stats$xmin + stats$xmax) / 2
    cy <- (stats$ymin + stats$ymax) / 2
    qx <- stats::quantile(cx, c(0.10, 0.90), na.rm = TRUE, names = FALSE)
    qy <- stats::quantile(cy, c(0.10, 0.90), na.rm = TRUE, names = FALSE)
    ix2 <- which(cx >= qx[1] & cx <= qx[2] & cy >= qy[1] & cy <= qy[2])
    if (length(ix2)) {
      grupos2 <- stats$group[ix2]
      core2 <- d[d$group %in% grupos2, , drop = FALSE]
      if (nrow(core2)) {
        grupos <- grupos2
        core <- core2
        bb <- c(
          xmin = min(core$x, na.rm = TRUE),
          xmax = max(core$x, na.rm = TRUE),
          ymin = min(core$y, na.rm = TRUE),
          ymax = max(core$y, na.rm = TRUE)
        )
      }
    }
  }

  list(
    groups = unique(grupos),
    bbox = bb,
    n_groups = length(unique(grupos)),
    excluded_groups = nrow(stats) - length(unique(grupos)),
    score = nrow(core),
    threshold = umbral
  )
}


# Amplía el núcleo arquitectónico para recuperar la planta completa útil sin
# reincorporar entidades CAD remotas. Se trabaja a nivel de grupos completos,
# de modo que una mesa, un muro o un bloque nunca queda cortado por el filtro.
filtrar_plano_completo_util_df <- function(df, core_groups, factor_margen = 0.75) {
  if (is.null(df) || !nrow(df) || is.null(core_groups) || !length(core_groups)) return(df)
  d <- df[is.finite(df$x) & is.finite(df$y) & !is.na(df$group), , drop = FALSE]
  if (!nrow(d)) return(df)
  ref <- d[d$group %in% core_groups, , drop = FALSE]
  if (!nrow(ref)) return(df)

  bb <- c(
    xmin = min(ref$x, na.rm = TRUE), xmax = max(ref$x, na.rm = TRUE),
    ymin = min(ref$y, na.rm = TRUE), ymax = max(ref$y, na.rm = TRUE)
  )
  w <- bb["xmax"] - bb["xmin"]
  h <- bb["ymax"] - bb["ymin"]
  diag_core <- sqrt(w^2 + h^2)
  if (!is.finite(diag_core) || diag_core <= 0) return(df)

  sp <- split(d, d$group)
  stats <- do.call(rbind, lapply(names(sp), function(g) {
    z <- sp[[g]]
    data.frame(
      group = g,
      xmin = min(z$x, na.rm = TRUE), xmax = max(z$x, na.rm = TRUE),
      ymin = min(z$y, na.rm = TRUE), ymax = max(z$y, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }))
  if (is.null(stats) || !nrow(stats)) return(df)

  gx <- pmax(0, pmax(stats$xmin, bb["xmin"]) - pmin(stats$xmax, bb["xmax"]))
  gy <- pmax(0, pmax(stats$ymin, bb["ymin"]) - pmin(stats$ymax, bb["ymax"]))
  dist_bb <- sqrt(gx^2 + gy^2)
  umbral <- max(2, factor_margen * diag_core)
  grupos_ok <- stats$group[is.finite(dist_bb) & dist_bb <= umbral]
  if (!length(grupos_ok)) return(df)

  out <- df[df$group %in% grupos_ok, , drop = FALSE]
  if (nrow(out)) out else df
}

# Lectura de INSUNITS desde un DWG mediante LibreDWG. Si el campo no existe
# o el dibujo es "unitless", se mantiene la confirmación manual.
detectar_unidad_dwg <- function(path) {
  dwgread <- encontrar_ejecutable("dwgread")
  if (!nzchar(dwgread) || !file.exists(path)) return(NULL)

  tmp <- tempfile(fileext = ".json")
  on.exit(try(unlink(tmp), silent = TRUE), add = TRUE)

  res <- tryCatch(
    system2(
      dwgread,
      args = c("-v0", "-O", "JSON", "-o", shQuote(tmp), shQuote(path)),
      stdout = TRUE,
      stderr = TRUE
    ),
    error = function(e) structure(character(), status = 999L)
  )
  st <- attr(res, "status")
  if (is.null(st)) st <- 0L
  if (!identical(as.integer(st), 0L) || !file.exists(tmp) || file.info(tmp)$size <= 0) return(NULL)

  contenido <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
  patrones <- c(
    '"INSUNITS"\\s*:\\s*([0-9]+)',
    '"\\$INSUNITS"\\s*:\\s*([0-9]+)',
    'INSUNITS[^0-9]{0,40}([0-9]+)'
  )
  codigo <- NA_integer_
  for (pat in patrones) {
    m <- regexec(pat, contenido, perl = TRUE, ignore.case = TRUE)
    r <- regmatches(contenido, m)[[1]]
    if (length(r) >= 2) {
      codigo <- suppressWarnings(as.integer(r[2]))
      if (is.finite(codigo)) break
    }
  }
  if (!is.finite(codigo)) return(NULL)

  tabla <- list(
    `0`  = list(label = "Sin unidades / unitless", shiny = "unknown", units_per_meter = NA_real_),
    `1`  = list(label = "Pulgadas", shiny = "in", units_per_meter = 39.3700787401575),
    `2`  = list(label = "Pies", shiny = "ft", units_per_meter = 3.28083989501312),
    `4`  = list(label = "Milímetros", shiny = "mm", units_per_meter = 1000),
    `5`  = list(label = "Centímetros", shiny = "cm", units_per_meter = 100),
    `6`  = list(label = "Metros", shiny = "m", units_per_meter = 1),
    `7`  = list(label = "Kilómetros", shiny = "custom", units_per_meter = 0.001),
    `10` = list(label = "Yardas", shiny = "custom", units_per_meter = 1.09361329833771),
    `14` = list(label = "Decímetros", shiny = "custom", units_per_meter = 10),
    `15` = list(label = "Decámetros", shiny = "custom", units_per_meter = 0.1),
    `16` = list(label = "Hectómetros", shiny = "custom", units_per_meter = 0.01),
    `21` = list(label = "Pies US survey", shiny = "custom", units_per_meter = 3.28083333333333)
  )
  info <- tabla[[as.character(codigo)]]
  if (is.null(info)) {
    info <- list(label = paste0("Código INSUNITS ", codigo), shiny = "unknown", units_per_meter = NA_real_)
  }
  c(list(code = codigo), info)
}

# Diagnóstico de escala. No fuerza unidades: presenta dimensiones candidatas
# para que el técnico confirme las unidades reales del DWG/DXF.
diagnosticar_escala_cad <- function(df) {
  if (is.null(df) || !nrow(df)) return(NULL)
  d <- df
  capas <- unique(d$layer)
  arq <- capas[es_capa_arquitectonica(capas)]
  if (length(arq)) d <- d[d$layer %in% arq, , drop = FALSE]
  # Excluye puntos/bloques para que INSERT o textos alejados no condicionen la extensión.
  if ("geom_type" %in% names(d)) {
    dl <- d[!toupper(d$geom_type) %in% c("POINT", "MULTIPOINT"), , drop = FALSE]
    if (nrow(dl)) d <- dl
  }
  xr <- range(d$x, na.rm = TRUE); yr <- range(d$y, na.rm = TRUE)
  sx <- diff(xr); sy <- diff(yr)
  if (!all(is.finite(c(sx, sy)))) return(NULL)
  list(
    bruto_x = sx, bruto_y = sy,
    m = c(sx, sy),
    cm = c(sx / 100, sy / 100),
    mm = c(sx / 1000, sy / 1000)
  )
}

# Salvaguarda de escala CAD para planos de edificios. Algunos conversores DWG
# (especialmente la ruta DWG -> GeoJSON) pueden devolver coordenadas ya
# normalizadas en unidades métricas aunque el encabezado conserve INSUNITS.
# Aplicar de nuevo mm/cm produciría una doble conversión (p.ej. 35 m -> 0.035 m).
# Si la geometría bruta tiene dimensiones plausibles para un edificio y la
# conversión declarada produciría una planta físicamente implausible (< 2 m),
# se conserva la geometría como metros. El usuario puede cambiarlo manualmente.
ajustar_unidad_cad_por_plausibilidad <- function(plano) {
  if (is.null(plano) || is.null(plano$kind) || plano$kind != "vector" ||
      is.null(plano$data) || !nrow(plano$data) || is.null(plano$unit_info)) return(plano)

  info <- plano$unit_info
  if (is.null(info$units_per_meter) || !is.finite(info$units_per_meter) || info$units_per_meter <= 0) return(plano)

  core <- detectar_edificio_principal(plano$data)
  if (is.null(core) || is.null(core$bbox)) return(plano)

  sx <- as.numeric(core$bbox["xmax"] - core$bbox["xmin"])
  sy <- as.numeric(core$bbox["ymax"] - core$bbox["ymin"])
  if (!all(is.finite(c(sx, sy))) || sx <= 0 || sy <= 0) return(plano)

  bruto_max <- max(sx, sy)
  bruto_min <- min(sx, sy)
  convertido <- c(sx, sy) / info$units_per_meter
  conv_max <- max(convertido)

  # Contexto de la aplicación: plantas de centros de trabajo/naves.
  # 5-1000 unidades brutas es compatible con coordenadas ya expresadas en m;
  # una planta resultante menor de 2 m es incompatible con ese uso.
  doble_conversion_probable <-
    info$units_per_meter > 1 &&
    bruto_max >= 5 && bruto_max <= 1000 && bruto_min >= 2 &&
    conv_max < 2

  if (isTRUE(doble_conversion_probable)) {
    plano$unit_info_original <- info
    plano$unit_info <- list(
      code = info$code,
      label = "Metros (escala corregida automáticamente)",
      shiny = "m",
      units_per_meter = 1
    )
    plano$scale_note <- paste0(
      "INSUNITS declara ", info$label, ", pero aplicado a la geometría importada daría ",
      round(convertido[1], 4), " × ", round(convertido[2], 4),
      " m. La geometría bruta del edificio es ", round(sx, 3), " × ", round(sy, 3),
      " unidades y es coherente con metros; se evita una doble conversión."
    )
  }

  plano
}

valor_num <- function(x, default = 0) {
  if (is.null(x) || length(x) != 1 || !is.finite(x)) return(default)
  as.numeric(x)
}

leer_csv_auto <- function(path) {
  d1 <- tryCatch(read.csv(path, check.names = FALSE), error = function(e) NULL)
  if (!is.null(d1) && ncol(d1) > 1) return(d1)
  d2 <- tryCatch(read.csv2(path, check.names = FALSE), error = function(e) NULL)
  if (!is.null(d2)) return(d2)
  d1
}

cerrar_poligono_df <- function(df) {
  if (is.null(df) || nrow(df) < 3) return(NULL)
  df <- df[is.finite(df$x) & is.finite(df$y), c("x", "y"), drop = FALSE]
  if (nrow(df) < 3) return(NULL)
  if (!isTRUE(all.equal(c(df$x[1], df$y[1]), c(df$x[nrow(df)], df$y[nrow(df)])))) {
    df <- rbind(df, df[1, , drop = FALSE])
  }
  df
}

area_poligono_xy <- function(x, y) {
  if (length(x) < 4 || length(y) != length(x)) return(0)
  0.5 * abs(sum(x[-length(x)] * y[-1] - x[-1] * y[-length(y)]))
}

rotar_xy <- function(x, y, grados = 0) {
  theta <- grados * pi / 180
  list(
    x = x * cos(theta) - y * sin(theta),
    y = x * sin(theta) + y * cos(theta)
  )
}

leer_tabla_plan <- function(df, capa_default = "PLANO") {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  names(df) <- normalizar_nombres(names(df))

  # Esquema 1: x, y[, layer/capa][, group/grupo][, type/tipo]
  if (all(c("x", "y") %in% names(df))) {
    capa_col <- intersect(c("layer", "capa"), names(df))
    grupo_col <- intersect(c("group", "grupo", "id"), names(df))
    tipo_col <- intersect(c("type", "tipo"), names(df))
    texto_col <- intersect(c("text", "texto", "label", "etiqueta", "nombre"), names(df))

    out <- data.frame(
      x = suppressWarnings(as.numeric(df$x)),
      y = suppressWarnings(as.numeric(df$y)),
      layer = if (length(capa_col)) as.character(df[[capa_col[1]]]) else capa_default,
      group = if (length(grupo_col)) as.character(df[[grupo_col[1]]]) else "1",
      geom_type = if (length(tipo_col)) toupper(as.character(df[[tipo_col[1]]])) else "PATH",
      label = if (length(texto_col)) as.character(df[[texto_col[1]]]) else NA_character_,
      stringsAsFactors = FALSE
    )
    out <- out[is.finite(out$x) & is.finite(out$y), , drop = FALSE]
    out$layer[is.na(out$layer) | !nzchar(out$layer)] <- capa_default
    out$group[is.na(out$group) | !nzchar(out$group)] <- "1"
    out$group <- paste(out$layer, out$group, sep = "__")
    return(out)
  }

  # Esquema 2: x1, y1, x2, y2[, layer/capa][, id]
  if (all(c("x1", "y1", "x2", "y2") %in% names(df))) {
    capa_col <- intersect(c("layer", "capa"), names(df))
    id_col <- intersect(c("id", "group", "grupo"), names(df))
    rows <- lapply(seq_len(nrow(df)), function(i) {
      capa <- if (length(capa_col)) as.character(df[[capa_col[1]]][i]) else capa_default
      gid <- if (length(id_col)) as.character(df[[id_col[1]]][i]) else as.character(i)
      data.frame(
        x = suppressWarnings(as.numeric(c(df$x1[i], df$x2[i]))),
        y = suppressWarnings(as.numeric(c(df$y1[i], df$y2[i]))),
        layer = capa,
        group = paste(capa, gid, sep = "__"),
        geom_type = "LINESTRING",
        label = NA_character_,
        stringsAsFactors = FALSE
      )
    })
    out <- do.call(rbind, rows)
    return(out[is.finite(out$x) & is.finite(out$y), , drop = FALSE])
  }

  NULL
}

sf_a_plan_df <- function(obj, capa_default = "CAD") {
  if (is.null(obj) || nrow(obj) == 0) return(NULL)
  nms <- normalizar_nombres(names(obj))
  names(obj) <- nms
  layer_attr <- intersect(c("layer", "capa", "layername", "level"), nms)
  text_attr <- intersect(c("text", "texto", "label", "contents", "content", "string", "value"), nms)

  res <- list()
  k <- 0L
  for (i in seq_len(nrow(obj))) {
    geom <- sf::st_geometry(obj)[i]
    crd <- tryCatch(sf::st_coordinates(geom), error = function(e) NULL)
    if (is.null(crd) || nrow(crd) == 0 || !all(c("X", "Y") %in% colnames(crd))) next

    capa <- if (length(layer_attr)) as.character(obj[[layer_attr[1]]][i]) else capa_default
    if (is.na(capa) || !nzchar(capa)) capa <- capa_default

    # En geometrías multipartes, L1/L2/L3 permiten no unir partes independientes.
    idx_cols <- intersect(c("L1", "L2", "L3"), colnames(crd))
    if (length(idx_cols)) {
      subgrp <- apply(crd[, idx_cols, drop = FALSE], 1, paste, collapse = "_")
    } else {
      subgrp <- rep("1", nrow(crd))
    }

    gtype <- as.character(sf::st_geometry_type(geom, by_geometry = TRUE))[1]
    etiqueta <- if (length(text_attr)) as.character(obj[[text_attr[1]]][i]) else NA_character_
    if (!length(etiqueta) || is.na(etiqueta) || !nzchar(trimws(etiqueta))) etiqueta <- NA_character_
    tmp <- data.frame(
      x = as.numeric(crd[, "X"]),
      y = as.numeric(crd[, "Y"]),
      layer = capa,
      group = paste(capa, i, subgrp, sep = "__"),
      geom_type = gtype,
      label = rep(etiqueta, nrow(crd)),
      stringsAsFactors = FALSE
    )
    k <- k + 1L
    res[[k]] <- tmp
  }

  if (!length(res)) return(NULL)
  do.call(rbind, res)
}

leer_vector_gdal <- function(path) {
  # Para DXF/DWG pedimos a GDAL que expanda INSERT/BLOCK. Esto es esencial
  # para recuperar mobiliario, equipos y simbolos del plano en la vista.
  vars <- c("DXF_INLINE_BLOCKS", "DXF_MERGE_BLOCK_GEOMETRIES",
            "DXF_FEATURE_LIMIT_PER_BLOCK", "DWG_INLINE_BLOCKS")
  old_env <- Sys.getenv(vars, unset = NA_character_)
  on.exit({
    for (i in seq_along(vars)) {
      if (is.na(old_env[i])) {
        Sys.unsetenv(vars[i])
      } else {
        do.call(Sys.setenv, setNames(list(old_env[i]), vars[i]))
      }
    }
  }, add = TRUE)
  Sys.setenv(
    DXF_INLINE_BLOCKS = "TRUE",
    DXF_MERGE_BLOCK_GEOMETRIES = "FALSE",
    DXF_FEATURE_LIMIT_PER_BLOCK = "-1",
    DWG_INLINE_BLOCKS = "TRUE"
  )

  capas <- tryCatch(sf::st_layers(path)$name, error = function(e) character())
  if (!length(capas)) {
    obj <- tryCatch(sf::st_read(path, quiet = TRUE), error = function(e) NULL)
    return(sf_a_plan_df(obj, "PLANO"))
  }

  partes <- lapply(capas, function(lyr) {
    obj <- tryCatch(sf::st_read(path, layer = lyr, quiet = TRUE), error = function(e) NULL)
    sf_a_plan_df(obj, lyr)
  })
  partes <- Filter(Negate(is.null), partes)
  if (!length(partes)) return(NULL)
  do.call(rbind, partes)
}

# Busca ejecutables también en las rutas habituales de Homebrew en macOS.
encontrar_ejecutable <- function(nombre) {
  p <- Sys.which(nombre)
  if (length(p) && nzchar(p[[1]])) return(unname(p[[1]]))

  candidatos <- c(
    file.path("/opt/homebrew/bin", nombre),
    file.path("/usr/local/bin", nombre),
    file.path("/usr/bin", nombre)
  )
  candidatos <- candidatos[file.exists(candidatos)]
  if (length(candidatos)) return(candidatos[[1]])
  ""
}

# Ejecuta un comando LibreDWG conservando un diagnostico util.
ejecutar_libredwg <- function(exe, args, salida = NULL) {
  errfile <- tempfile(fileext = ".log")
  on.exit(unlink(errfile), add = TRUE)

  status <- tryCatch({
    if (is.null(salida)) {
      system2(exe, args = args, stdout = TRUE, stderr = TRUE)
    } else {
      system2(exe, args = args, stdout = FALSE, stderr = errfile)
    }
  }, error = function(e) structure(character(), status = 999L, errmsg = conditionMessage(e)))

  if (is.null(salida)) {
    log <- as.character(status)
    st <- attr(status, "status")
    if (is.null(st)) st <- 0L
  } else {
    st <- status
    if (is.null(st) || !length(st)) st <- 0L
    log <- if (file.exists(errfile)) readLines(errfile, warn = FALSE) else character()
  }

  list(
    status = as.integer(st)[1],
    log = log,
    output_exists = !is.null(salida) && file.exists(salida),
    output_size = if (!is.null(salida) && file.exists(salida)) unname(file.info(salida)$size) else 0
  )
}

# Importacion DWG robusta (v14.2).
# LibreDWG devuelve una mascara de avisos/errores. Valores como
# NOTYETSUPPORTED (2), UNHANDLEDCLASS (4), INVALIDTYPE (8), etc. pueden ser
# no criticos y el programa puede haber generado una salida perfectamente
# aprovechable. Por ello NO se exige status == 0: se valida la salida real.
# Solo se descarta un candidato cuando no existe, esta vacio o no contiene
# geometria utilizable.

ejecutar_conversion_dwg <- function(exe, args, salida, metodo) {
  res <- tryCatch(
    system2(exe, args = args, stdout = TRUE, stderr = TRUE),
    error = function(e) structure(character(), status = 999L, errmsg = conditionMessage(e))
  )
  st <- attr(res, "status")
  if (is.null(st)) st <- 0L
  tam <- if (file.exists(salida)) unname(file.info(salida)$size) else 0
  list(
    method = metodo,
    status = as.integer(st)[1],
    log = as.character(res),
    path = salida,
    size = tam,
    exists = file.exists(salida)
  )
}

# Valida que una conversion tenga geometria suficiente para ser usada.
validar_geometria_cad <- function(vec) {
  if (is.null(vec) || !is.data.frame(vec) || !nrow(vec)) return(FALSE)
  if (!all(c("x", "y") %in% names(vec))) return(FALSE)
  ok <- is.finite(vec$x) & is.finite(vec$y)
  if (sum(ok) < 2) return(FALSE)
  xr <- range(vec$x[ok], na.rm = TRUE)
  yr <- range(vec$y[ok], na.rm = TRUE)
  # Al menos alguna extension espacial; un unico punto no es un plano.
  is.finite(diff(xr)) && is.finite(diff(yr)) && (diff(xr) > 0 || diff(yr) > 0)
}

convertir_dwg_libredwg_candidatos <- function(path) {
  dwgread <- encontrar_ejecutable("dwgread")
  dwg2dxf <- encontrar_ejecutable("dwg2dxf")
  candidatos <- list()

  if (nzchar(dwgread)) {
    salida_geo <- tempfile(fileext = ".geojson")
    candidatos[[length(candidatos) + 1L]] <- ejecutar_conversion_dwg(
      dwgread,
      c("-v1", "-O", "GeoJSON", "-o", shQuote(salida_geo), shQuote(path)),
      salida_geo,
      "LibreDWG/dwgread GeoJSON"
    )
  }

  if (nzchar(dwgread)) {
    salida_dxf <- tempfile(fileext = ".dxf")
    candidatos[[length(candidatos) + 1L]] <- ejecutar_conversion_dwg(
      dwgread,
      c("-v1", "-O", "DXF", "-o", shQuote(salida_dxf), shQuote(path)),
      salida_dxf,
      "LibreDWG/dwgread DXF"
    )
  }

  if (nzchar(dwg2dxf)) {
    salida_dxf2 <- tempfile(fileext = ".dxf")
    candidatos[[length(candidatos) + 1L]] <- ejecutar_conversion_dwg(
      dwg2dxf,
      c("-v1", "-y", "-o", shQuote(salida_dxf2), shQuote(path)),
      salida_dxf2,
      "LibreDWG/dwg2dxf"
    )
  }

  # Ultimo recurso: DXF minimo. Puede salvar dibujos con tablas/clases
  # problematicas conservando las entidades geometricas esenciales.
  if (nzchar(dwg2dxf)) {
    salida_min <- tempfile(fileext = ".dxf")
    candidatos[[length(candidatos) + 1L]] <- ejecutar_conversion_dwg(
      dwg2dxf,
      c("-v1", "-y", "--minimal", "-o", shQuote(salida_min), shQuote(path)),
      salida_min,
      "LibreDWG/dwg2dxf minimal"
    )
  }

  list(
    candidates = candidatos,
    available = c(dwgread = nzchar(dwgread), dwg2dxf = nzchar(dwg2dxf))
  )
}

formatear_diagnostico_dwg <- function(res) {
  intentos <- if (!is.null(res$candidates)) res$candidates else res$attempts
  if (is.null(intentos) || !length(intentos)) return("Sin diagnostico adicional de LibreDWG.")
  bloques <- vapply(intentos, function(a) {
    logtxt <- paste(a$log, collapse = " | ")
    if (nchar(logtxt) > 900) logtxt <- paste0(substr(logtxt, 1, 900), "...")
    paste0(
      a$method, ": estado=", a$status,
      ", salida=", ifelse(is.null(a$size), 0, a$size), " bytes",
      if (a$status != 0 && a$status < 128) " [aviso no critico: se intenta recuperar]" else "",
      if (nzchar(logtxt)) paste0("; ", logtxt) else ""
    )
  }, character(1))
  paste(bloques, collapse = "\n")
}

# Puntua cuanta informacion grafica recupera cada ruta de conversion.
# Se prima linework, numero de grupos y numero de capas. De este modo un DXF
# con bloques expandidos gana a un GeoJSON que solo contenga los muros.
puntuacion_geometria_cad <- function(vec) {
  if (!validar_geometria_cad(vec)) return(-Inf)
  np <- if ("geom_type" %in% names(vec)) {
    sum(!toupper(vec$geom_type) %in% c("POINT", "MULTIPOINT"), na.rm = TRUE)
  } else nrow(vec)
  ng <- if ("group" %in% names(vec)) length(unique(vec$group)) else 1L
  nl <- if ("layer" %in% names(vec)) length(unique(vec$layer)) else 1L
  log1p(np) * 100 + log1p(ng) * 250 + nl * 500
}

leer_dwg_con_fallback <- function(path) {
  opciones <- list()
  diagnosticos <- character()

  # 1) Intento directo con GDAL, si el build dispone de driver DWG.
  vec <- tryCatch(leer_vector_gdal(path), error = function(e) NULL)
  if (validar_geometria_cad(vec)) {
    opciones[[length(opciones) + 1L]] <- list(
      data = vec, method = "GDAL DWG directo", status = 0L,
      score = puntuacion_geometria_cad(vec)
    )
    diagnosticos <- c(diagnosticos, paste0(
      "GDAL DWG directo: geometria valida; capas=", length(unique(vec$layer)),
      ", grupos=", length(unique(vec$group)), ", vertices=", nrow(vec)
    ))
  }

  # 2) LibreDWG. Se prueban TODOS los candidatos. No se usa el primero que
  # funcione: se selecciona el que recupere el dibujo mas rico.
  conv <- convertir_dwg_libredwg_candidatos(path)
  paths_tmp <- character()
  on.exit(if (length(paths_tmp)) try(unlink(paths_tmp), silent = TRUE), add = TRUE)

  if (length(conv$candidates)) {
    for (cand in conv$candidates) {
      paths_tmp <- c(paths_tmp, cand$path)
      logtxt <- paste(cand$log, collapse = " | ")
      if (nchar(logtxt) > 500) logtxt <- paste0(substr(logtxt, 1, 500), "...")

      if (!isTRUE(cand$exists) || !is.finite(cand$size) || cand$size < 50) {
        diagnosticos <- c(diagnosticos, paste0(cand$method, ": sin salida util; estado=", cand$status))
        next
      }

      vec2 <- tryCatch(leer_vector_gdal(cand$path), error = function(e) NULL)
      if (validar_geometria_cad(vec2)) {
        sc <- puntuacion_geometria_cad(vec2)
        opciones[[length(opciones) + 1L]] <- list(
          data = vec2, method = cand$method, status = cand$status, score = sc
        )
        diagnosticos <- c(diagnosticos, paste0(
          cand$method, ": geometria valida; estado=", cand$status,
          ", capas=", length(unique(vec2$layer)),
          ", grupos=", length(unique(vec2$group)),
          ", vertices=", nrow(vec2)
        ))
      } else {
        diagnosticos <- c(diagnosticos, paste0(
          cand$method, ": salida generada pero sin geometria util; estado=", cand$status,
          if (nzchar(logtxt)) paste0("; ", logtxt) else ""
        ))
      }
    }
  }

  if (length(opciones)) {
    scores <- vapply(opciones, function(z) z$score, numeric(1))
    best <- opciones[[which.max(scores)]]
    aviso <- if (!is.null(best$status) && best$status != 0) {
      paste0(" Importado pese a aviso LibreDWG (codigo ", best$status,
             "); se han omitido las entidades que no pudieron interpretarse.")
    } else ""
    return(list(
      data = best$data,
      method = best$method,
      diagnostic = paste0(
        "Se ha seleccionado la conversion con mayor riqueza grafica: ", best$method, ".\n",
        paste(diagnosticos, collapse = "\n"), aviso
      )
    ))
  }

  list(
    data = NULL,
    method = NULL,
    diagnostic = if (length(diagnosticos)) paste(diagnosticos, collapse = "\n") else "No se genero ninguna geometria recuperable."
  )
}

leer_raster_plan <- function(path, ext) {
  ext <- tolower(ext)

  if (ext == "png") {
    if (!requireNamespace("png", quietly = TRUE)) stop("Falta el paquete opcional 'png'.")
    img <- png::readPNG(path)
  } else if (ext %in% c("jpg", "jpeg")) {
    if (!requireNamespace("jpeg", quietly = TRUE)) stop("Falta el paquete opcional 'jpeg'.")
    img <- jpeg::readJPEG(path)
  } else if (ext %in% c("tif", "tiff")) {
    if (!requireNamespace("tiff", quietly = TRUE)) stop("Falta el paquete opcional 'tiff'.")
    img <- tiff::readTIFF(path)
  } else if (ext == "pdf") {
    if (!requireNamespace("pdftools", quietly = TRUE) || !requireNamespace("png", quietly = TRUE)) {
      stop("Para usar un PDF como fondo se necesitan los paquetes opcionales 'pdftools' y 'png'.")
    }
    tmp <- tempfile(fileext = ".png")
    pdftools::pdf_convert(path, format = "png", pages = 1, dpi = 150, filenames = tmp, verbose = FALSE)
    img <- png::readPNG(tmp)
    unlink(tmp)
  } else {
    stop("Formato raster no compatible.")
  }

  list(
    raster = as.raster(img),
    width_px = dim(img)[2],
    height_px = dim(img)[1]
  )
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML("\n      body { background-color: #f7f9fb; }\n      .app-header { margin-bottom: 14px; }\n      .app-header h2 { margin-bottom: 3px; font-weight: 650; }\n      .app-subtitle { color: #667085; margin-bottom: 0; }\n      .config-card {\n        background: #ffffff; border: 1px solid #e3e8ef; border-radius: 10px;\n        padding: 16px 18px; margin-bottom: 16px; box-shadow: 0 1px 3px rgba(0,0,0,.04);\n      }\n      .config-card h4 { margin-top: 0; font-weight: 650; }\n      .program-intro {\n        background: #eef5fb; border-left: 4px solid #337ab7; border-radius: 6px;\n        padding: 11px 14px; margin: 8px 0 16px 0;\n      }\n      .plan-source .radio {\n        border: 1px solid #d9e1ea; border-radius: 8px; padding: 11px 12px 11px 34px;\n        margin: 8px 0; background: #fff; transition: .15s ease;\n      }\n      .plan-source .radio:hover { border-color: #337ab7; background: #f4f8fc; }\n      .plan-source label { font-weight: 600; }\n      .source-note { color: #667085; font-size: 13px; line-height: 1.45; }\n      .format-badge {\n        display: inline-block; padding: 3px 7px; border-radius: 5px; margin: 2px 3px 2px 0;\n        background: #eef2f6; color: #344054; font-size: 12px;\n      }\n      .nav-tabs > li > a { font-size: 16px; }\n      .nav-pills > li > a { font-weight: 600; }\n      .tab-content { padding-top: 14px; }\n      .section-help { color: #667085; font-size: 13px; margin-top: 4px; }\n      .btn-primary { font-weight: 600; }\n      .app-header-row { display:flex; align-items:flex-start; justify-content:space-between; gap:16px; }\n      .app-header-actions { padding-top:4px; white-space:nowrap; }\n      .btn-new-map { font-weight:650; border-color:#d0d5dd; background:#fff; color:#344054; }\n      .btn-new-map:hover { background:#f2f4f7; border-color:#98a2b3; color:#1d2939; }\n    ")),
    tags$script(HTML("\n      Shiny.addCustomMessageHandler('worknoise_reset_map_client', function(id) {\n        try {\n          var el = document.getElementById(id);\n          if (el) {\n            el.value = '';\n            var group = el.closest('.form-group');\n            if (group) {\n              var txt = group.querySelector('input[type=\"text\"]');\n              if (txt) txt.value = '';\n              var prog = group.querySelector('.progress');\n              if (prog) prog.style.display = 'none';\n            }\n            if (window.jQuery) $(el).trigger('change');\n          }\n          if (window.Plotly) {\n            document.querySelectorAll('.js-plotly-plot').forEach(function(node) {\n              try { Plotly.purge(node); } catch(e) {}\n            });\n          }\n        } catch(e) { console.warn('WorknoiseR reset client:', e); }\n      });\n    "))
  ),

  div(
    class = "app-header",
    div(
      class = "app-header-row",
      div(
        h2("Generador de mapas de ruido laboral"),
        p(class = "app-subtitle", "Configuración, modelización espacial y evaluación de exposición en una única interfaz.")
      ),
      div(
        class = "app-header-actions",
        actionButton("nuevo_mapa", "↻ Nuevo mapa / reiniciar", class = "btn btn-new-map")
      )
    )
  ),

  tabsetPanel(
    id = "main_tabs",
    selected = "Programación",

    tabPanel(
      "Programación",
      div(
        class = "program-intro",
        tags$b("Flujo de trabajo: "),
        "defina primero la geometría, después las fuentes y receptores, y finalmente las condiciones de propagación. Los resultados se consultan en las pestañas superiores."
      ),

      tabsetPanel(
        id = "program_tabs",
        type = "pills",

        tabPanel(
          "1. Plano y geometría",
          fluidRow(
            column(
              4,
              div(
                class = "config-card",
                h4("Origen del plano"),
                p(class = "section-help", "Seleccione una única vía de definición. La aplicación mostrará solo los controles necesarios."),
                div(
                  class = "plan-source",
                  radioButtons(
                    "origen_plano",
                    label = NULL,
                    choices = c(
                      "Diseño manual / nave básica" = "manual",
                      "CAD / DWG-DXF / GIS" = "cad",
                      "Excel / CSV de coordenadas" = "tabla",
                      "PDF / imagen calibrable" = "raster"
                    ),
                    selected = "manual"
                  )
                ),
                div(
                  style = "display:none;",
                  checkboxInput("usar_plano_importado", "Usar plano importado", FALSE),
                  selectInput(
                    "modo_importacion_plano", "Modo interno de importación:",
                    choices = c("Automático" = "auto", "Vectorial" = "vector", "Raster" = "raster"),
                    selected = "auto"
                  )
                )
              )
            ),

            column(
              8,
              conditionalPanel(
                "input.origen_plano == 'manual'",
                div(
                  class = "config-card",
                  h4("Diseño manual"),
                  p(class = "source-note", "Use esta opción para una nave rectangular o para definir manualmente un contorno mediante coordenadas."),
                  fluidRow(
                    column(6, numericInput("largo", "Largo de la nave (m):", 100, min = 1)),
                    column(6, numericInput("ancho", "Ancho de la nave (m):", 50, min = 1))
                  ),
                  checkboxInput("usar_contorno", "Definir contorno personalizado", FALSE),
                  conditionalPanel(
                    "input.usar_contorno == true",
                    helpText("Introduzca los vértices del contorno. No es necesario repetir manualmente el primer punto al final."),
                    rHandsontableOutput("tabla_contorno"),
                    fileInput("archivo_contorno", "Subir CSV con coordenadas x, y:", accept = ".csv")
                  )
                )
              ),

              conditionalPanel(
                "input.origen_plano != 'manual'",
                div(
                  class = "config-card",
                  h4("Carga del plano"),
                  conditionalPanel(
                    "input.origen_plano == 'cad'",
                    p(class = "source-note", "Importación vectorial para conservar capas y geometrías CAD/GIS."),
                    div(
                      span(class = "format-badge", "DWG"), span(class = "format-badge", "DXF"),
                      span(class = "format-badge", "GeoJSON"), span(class = "format-badge", "GPKG"),
                      span(class = "format-badge", "PDF vectorial")
                    )
                  ),
                  conditionalPanel(
                    "input.origen_plano == 'tabla'",
                    p(class = "source-note", "Importe coordenadas x/y o segmentos x1/y1/x2/y2. En Excel, cada hoja puede convertirse en una capa."),
                    div(span(class = "format-badge", "XLSX"), span(class = "format-badge", "XLS"), span(class = "format-badge", "CSV"))
                  ),
                  conditionalPanel(
                    "input.origen_plano == 'raster'",
                    p(class = "source-note", "Use un plano como fondo y ajuste su escala mediante dimensiones de nave o calibración con dos puntos."),
                    div(
                      span(class = "format-badge", "PDF"), span(class = "format-badge", "PNG"),
                      span(class = "format-badge", "JPG"), span(class = "format-badge", "TIFF")
                    )
                  ),
                  tags$br(),
                  fileInput(
                    "archivo_plano",
                    "Seleccione el archivo:",
                    accept = c(
                      ".csv", ".xlsx", ".xls", ".dxf", ".dwg", ".pdf",
                      ".png", ".jpg", ".jpeg", ".tif", ".tiff",
                      ".geojson", ".json", ".gpkg"
                    )
                  ),
                  actionButton("importar_plano", "Importar y preparar plano", class = "btn-primary"),
                  tags$br(), tags$br(),
                  uiOutput("plan_status_ui")
                ),
                uiOutput("plan_vector_ui"),
                uiOutput("plan_raster_ui")
              )
            )
          ),

          fluidRow(
            column(
              6,
              div(
                class = "config-card",
                h4("Elementos auxiliares"),
                checkboxInput("incluir_puerta", "Indicar una puerta de referencia", FALSE),
                conditionalPanel(
                  "input.incluir_puerta == true",
                  fluidRow(
                    column(6, numericInput("puerta_x", "X:", 0)),
                    column(6, numericInput("puerta_y", "Y:", 0))
                  )
                )
              )
            ),
            column(
              6,
              div(
                class = "config-card",
                h4("Zonas de oficina"),
                checkboxInput("incluir_oficinas", "Definir zona de oficinas", FALSE),
                conditionalPanel(
                  "input.incluir_oficinas == true",
                  fluidRow(
                    column(6, numericInput("ofi_xmin", "X desde:", 10)),
                    column(6, numericInput("ofi_xmax", "X hasta:", 20))
                  ),
                  fluidRow(
                    column(6, numericInput("ofi_ymin", "Y desde:", 10)),
                    column(6, numericInput("ofi_ymax", "Y hasta:", 20))
                  )
                )
              )
            )
          )
        ),

        tabPanel(
          "2. Fuentes",
          fluidRow(
            column(
              7,
              div(
                class = "config-card",
                h4("Fuentes sonoras / máquinas"),
                numericInput("n_maquinas", "Número de máquinas:", 2, min = 1),
                uiOutput("maquinas_ui")
              )
            ),
            column(
              5,
              div(
                class = "config-card",
                h4("Cerramientos de máquinas"),
                checkboxInput("usar_cerramiento", "Aplicar cerramiento a una o varias máquinas", FALSE),
                conditionalPanel("input.usar_cerramiento == true", uiOutput("ui_cerramientos")),
                p(class = "section-help", "El cerramiento de una máquina modifica su emisión antes de calcular la propagación espacial.")
              )
            )
          )
        ),

        tabPanel(
          "3. Receptores",
          fluidRow(
            column(
              7,
              div(
                class = "config-card",
                h4("Trabajadores / receptores"),
                checkboxInput("incluir_trabajadores", "Incluir trabajadores", FALSE),
                conditionalPanel(
                  "input.incluir_trabajadores == true",
                  numericInput("n_trabajadores", "Número de trabajadores:", 1, min = 1),
                  uiOutput("trabajadores_ui")
                )
              )
            ),
            column(
              5,
              div(
                class = "config-card",
                h4("Protección auditiva"),
                checkboxInput("usar_epi", "Evaluar protección auditiva mediante SNR", FALSE),
                conditionalPanel(
                  "input.usar_epi == true",
                  numericInput("snr_valor", "SNR del protector [dB]:", value = 20, min = 0, max = 50),
                  numericInput("map_snr_derating", "Derating adicional definido por el usuario [dB]:", value = 0, min = 0, max = 50),
                  helpText("SNR requiere LCeq,T. El derating es opcional y por defecto vale 0 dB.")
                )
              )
            )
          )
        ),

        tabPanel(
          "4. Propagación",
          fluidRow(
            column(
              7,
              div(
                class = "config-card",
                h4("Campo reverberante"),
                checkboxInput("usar_reverberacion", "Incluir campo reverberante", FALSE),
                conditionalPanel(
                  "input.usar_reverberacion == true",
                  selectInput(
                    "modelo_reverberacion", "Modelo de reverberación:",
                    choices = c(
                      "Por recinto aproximado (recomendado)" = "recinto",
                      "Global sin compartimentación (comparación)" = "global"
                    ),
                    selected = "recinto"
                  ),
                  selectInput(
                    "material_reverberacion", "Coeficiente medio de absorción del recinto (α):",
                    choices = c(
                      "Muy reflectante (α ≈ 0,03)" = 0.03,
                      "Reflectante (α ≈ 0,05)" = 0.05,
                      "Madera barnizada / absorción baja (α ≈ 0,08)" = 0.08,
                      "Superficie mixta / absorción media (α ≈ 0,25)" = 0.25,
                      "Absorbente (α ≈ 0,50)" = 0.50,
                      "Muy absorbente (α ≈ 0,75)" = 0.75,
                      "Tratamiento poroso intenso (α ≈ 0,80)" = 0.80
                    ),
                    selected = 0.08
                  ),
                  numericInput("altura_techo", "Altura del techo (m):", value = 5, min = 2, max = 20, step = 0.5),
                  helpText("En el modo por recinto, el campo reverberante se limita al recinto aproximado de la fuente. La transmisión entre recintos se trata por separado.")
                )
              )
            ),
            column(
              5,
              div(
                class = "config-card",
                h4("Criterios de representación"),
                numericInput(
                  "nivel_objetivo_distancia",
                  "Nivel objetivo para distancia teórica en campo libre [dB(A)]:",
                  value = 85, min = 0, max = 140
                ),
                p(class = "section-help", "Las propiedades de paredes y barreras procedentes de un plano vectorial se configuran en la sección avanzada que aparece tras importarlo."),
                p(class = "section-help", "La distancia teórica es un resultado de campo libre y se presenta separada del modelo interior con cerramientos.")
              )
            )
          )
        ),

        tabPanel(
          "5. Calibración",
          div(
            class = "config-card",
            h4("Estimación de niveles de fuente a partir de mediciones"),
            checkboxInput("usar_estimacion_Li", "Estimar Lᵢ a partir de mediciones", FALSE),
            conditionalPanel(
              "input.usar_estimacion_Li == true",
              helpText("Introduzca las coordenadas de medición y el nivel Lp observado."),
              rHandsontableOutput("tabla_mediciones"),
              actionButton("estimar_Li", "Estimar niveles Lᵢ", class = "btn-primary"),
              tags$br(), tags$br(),
              verbatimTextOutput("resultado_Li")
            )
          )
        )
      )
    ),

    tabPanel(
      "Plano",
      plotOutput("plano_preview", click = "plano_click", brush = brushOpts(id = "plano_brush", resetOnNew = TRUE), height = "650px"),
      tableOutput("tabla_capas_plano")
    ),
    tabPanel("Mapa 2D", plotOutput("mapa_2d", height = "650px")),
    tabPanel("Mapa con isófonas", plotOutput("mapa_isofonas", height = "650px")),
    tabPanel("Mapa 3D", plotlyOutput("mapa_3d", height = "700px")),
    tabPanel("Trabajadores", tableOutput("tabla_trabajadores")),
    tabPanel("Distancias", tableOutput("tabla_distancias"))
  )
)

server <- function(input, output, session) {

  # ---------------------------------------------------------------------------
  # Importación y transformación de planos
  # ---------------------------------------------------------------------------

  plano_raw <- reactiveVal(NULL)
  plano_estado <- reactiveVal("Sin plano importado.")
  clicks_calibracion <- reactiveVal(data.frame(x = numeric(), y = numeric()))
  pixels_por_metro <- reactiveVal(NA_real_)
  ventana_manual_plano <- reactiveVal(NULL)
  unidad_auto_aplicada <- reactiveVal(FALSE)

  # Reinicio controlado del estado asociado al plano. Se conserva, por diseño,
  # la configuración de fuentes y trabajadores para poder reutilizarla con
  # otro plano. Al poner plano_raw a NULL desaparecen también los controles
  # dinámicos dependientes de capas del archivo anterior.
  limpiar_estado_plano <- function(limpiar_archivo = TRUE, mensaje = "Listo para cargar un nuevo plano.") {
    plano_raw(NULL)
    plano_estado(mensaje)
    clicks_calibracion(data.frame(x = numeric(), y = numeric()))
    pixels_por_metro(NA_real_)
    ventana_manual_plano(NULL)
    unidad_auto_aplicada(FALSE)

    # Propiedades dependientes del recinto: no se arrastran al nuevo mapa.
    updateCheckboxInput(session, "usar_reverberacion", value = FALSE)
    updateRadioButtons(session, "modelo_reverberacion", selected = "recinto")
    updateCheckboxInput(session, "usar_paredes_acusticas", value = FALSE)
    updateCheckboxInput(session, "usar_estimacion_Li", value = FALSE)

    updateTabsetPanel(session, "main_tabs", selected = "Programación")
    updateTabsetPanel(session, "program_tabs", selected = "1. Plano y geometría")

    if (isTRUE(limpiar_archivo)) {
      session$sendCustomMessage("worknoise_reset_map_client", "archivo_plano")
    }

    invisible(gc(verbose = FALSE))
  }

  observeEvent(input$nuevo_mapa, {
    showModal(modalDialog(
      title = "Crear un nuevo mapa",
      p("Se eliminará el plano actualmente importado, su selección de capas, la calibración y el encuadre."),
      p("También se reiniciarán reverberación, barreras acústicas y estimación inversa, ya que dependen del recinto."),
      p(tags$b("Se conservarán las fuentes sonoras y los trabajadores"), " para poder reutilizarlos si lo desea."),
      footer = tagList(
        modalButton("Cancelar"),
        actionButton("confirmar_nuevo_mapa", "Reiniciar y cargar otro plano", class = "btn-primary")
      ),
      easyClose = TRUE
    ))
  })

  observeEvent(input$confirmar_nuevo_mapa, {
    removeModal()
    limpiar_estado_plano(limpiar_archivo = TRUE)
    showNotification("Mapa reiniciado. Ya puede seleccionar e importar un nuevo plano.", type = "message", duration = 5)
  })

  # La pestaña Plano es permanente. El selector de Programación determina
  # si el motor usa geometría manual o un plano importado.
  observeEvent(input$origen_plano, {
    modo <- if (is.null(input$origen_plano)) "manual" else input$origen_plano
    importar <- !identical(modo, "manual")
    updateCheckboxInput(session, "usar_plano_importado", value = importar)

    modo_interno <- switch(
      modo,
      cad = "vector",
      tabla = "vector",
      raster = "raster",
      "auto"
    )
    updateSelectInput(session, "modo_importacion_plano", selected = modo_interno)

    if (identical(modo, "manual")) {
      updateTabsetPanel(session, "main_tabs", selected = "Programación")
    }
  }, ignoreInit = FALSE)

  output$plan_status_ui <- renderUI({
    tags$div(
      style = "font-size: 12px; padding: 6px; background: #f5f5f5; border-radius: 4px;",
      plano_estado()
    )
  })

  output$plan_vector_ui <- renderUI({
    p <- plano_raw()
    if (is.null(p) || p$kind != "vector") return(NULL)
    capas <- sort(unique(p$data$layer))
    seleccion_inicial <- seleccion_capas_visual_recomendada(capas)
    unidad_sel <- if (!is.null(p$unit_info) && !is.null(p$unit_info$shiny)) p$unit_info$shiny else "unknown"
    unidad_custom <- if (!is.null(p$unit_info) && is.finite(p$unit_info$units_per_meter)) p$unit_info$units_per_meter else 1
    tagList(
      helpText("La visualización del plano es independiente del modelo acústico. La vista recomendada muestra el plano completo recuperado del archivo; el dominio acústico y las barreras se seleccionan aparte. Así ningún mueble, bloque o elemento gráfico queda recortado por el área de cálculo."),
      fluidRow(
        column(4, actionButton("capas_detalle", "Plano detallado", width = "100%")),
        column(4, actionButton("capas_arquitectura", "Solo arquitectura", width = "100%")),
        column(4, actionButton("capas_todas", "Todas las capas", width = "100%"))
      ),
      checkboxGroupInput("capas_plano", "Capas visibles (solo representacion):", choices = capas, selected = seleccion_inicial),
      checkboxInput("mostrar_puntos_cad", "Mostrar entidades puntuales/bloques CAD", FALSE),
      helpText("Los puntos e INSERT de mobiliario/equipos pueden generar miles de marcas y ampliar artificialmente la vista; por eso se ocultan inicialmente."),
      radioButtons(
        "vista_grafica_plano",
        "Vista grafica:",
        choices = c(
          "Plano completo recuperado (recomendado)" = "todo",
          "Plano detallado dentro del dominio acústico" = "dominio_detalle",
          "Solo geometría del dominio acústico" = "modelo"
        ),
        selected = "todo"
      ),
      radioButtons(
        "encuadre_plano",
        "Dominio para el calculo acustico:",
        choices = c(
          "Plano completo útil (recomendado)" = "completo_auto",
          "Edificio principal detectado" = "principal",
          "Todo el DWG/plano, incluidos outliers" = "todo",
          "Ventana manual" = "manual"
        ),
        selected = "completo_auto"
      ),
      fluidRow(
        column(6, actionButton("usar_brush_ventana", "Usar selección rectangular", width = "100%")),
        column(6, actionButton("limpiar_ventana_manual", "Restaurar plano completo", width = "100%"))
      ),
      helpText("Para una ventana manual, arrastre un rectángulo sobre el plano y pulse 'Usar selección rectangular'. Los elementos fuera de la vista no se borran del CAD."),
      uiOutput("edificio_principal_ui"),
      selectInput("capa_contorno_plano", "Capa a usar como contorno acústico:", choices = c("Ninguna" = "", capas)),
      tags$hr(),
      tags$b("Propagación acústica con paredes"),
      checkboxInput(
        "usar_paredes_acusticas",
        "Usar paredes/cerramientos del CAD como barreras acústicas",
        TRUE
      ),
      conditionalPanel(
        "input.usar_paredes_acusticas == true",
        checkboxGroupInput(
          "capas_barrera_acustica",
          "Capas que actúan como barrera:",
          choices = capas,
          selected = capas[es_capa_barrera_acustica(capas)]
        ),
        selectInput(
          "modelo_paredes",
          "Modelo de propagación con cerramientos:",
          choices = c(
            "Híbrido: transmisión + borde/difracción (recomendado)" = "hibrido",
            "Cruce recto rápido (modelo v13.8)" = "cruce"
          ),
          selected = "hibrido"
        ),
        numericInput(
          "atenuacion_cruce_pared",
          "Pérdida modelizada del primer cerramiento [dB]:",
          value = 12, min = 0, max = 80, step = 1
        ),
        conditionalPanel(
          "input.modelo_paredes == 'hibrido'",
          numericInput(
            "atenuacion_pared_adicional",
            "Pérdida por cerramiento adicional [dB]:",
            value = 6, min = 0, max = 40, step = 1
          ),
          numericInput(
            "atenuacion_pared_max",
            "Pérdida máxima acumulada por transmisión [dB]:",
            value = 30, min = 0, max = 80, step = 1
          ),
          numericInput(
            "perdida_borde",
            "Pérdida base de propagación alrededor de borde [dB]:",
            value = 6, min = 0, max = 30, step = 1
          )
        ),
        numericInput(
          "agrupar_cruces_m",
          "Agrupar intersecciones próximas como un mismo muro (m):",
          value = 0.30, min = 0, max = 2, step = 0.05
        ),
        checkboxInput(
          "suavizado_visual",
          "Suavizado visual energético (no altera los cálculos)",
          TRUE
        ),
        conditionalPanel(
          "input.suavizado_visual == true",
          sliderInput(
            "suavizado_iteraciones",
            "Intensidad del suavizado visual:",
            min = 1, max = 3, value = 1, step = 1
          )
        ),
        helpText(
          "El modelo híbrido suma transmisión a través del cerramiento y una contribución que rodea sus bordes, evitando sombras geométricas excesivamente bruscas. La reverberación se controla aparte y, en el modo recomendado, no cruza paredes mediante una pérdida fija en dB. ",
          "El suavizado se aplica solo a la representación gráfica; trabajadores, tablas y cálculos conservan el campo sin suavizar. ",
          "Los parámetros son de modelización y no equivalen automáticamente a Rw/R de un cerramiento."
        )
      ),
      selectInput(
        "unidades_plano",
        "Unidades del archivo:",
        choices = c(
          "Seleccione / confirme unidades" = "unknown",
          "Metros" = "m",
          "Centímetros" = "cm",
          "Milímetros" = "mm",
          "Pies" = "ft",
          "Pulgadas" = "in",
          "Personalizada" = "custom"
        ),
        selected = unidad_sel
      ),
      uiOutput("unidades_detectadas_ui"),
      conditionalPanel(
        "input.unidades_plano == 'custom'",
        numericInput("unidades_por_metro", "Unidades del plano por metro:", unidad_custom, min = 1e-9)
      ),
      numericInput("rotacion_plano", "Rotación vectorial (grados):", 0, step = 1),
      checkboxInput("origen_minimo_plano", "Trasladar mínimo X/Y a (0,0)", TRUE),
      numericInput("offset_x_plano", "Desplazamiento X final (m):", 0, step = 0.1),
      numericInput("offset_y_plano", "Desplazamiento Y final (m):", 0, step = 0.1),
      checkboxInput("actualizar_dimensiones_plano", "Actualizar largo/ancho según el plano", TRUE),
      checkboxInput("usar_contorno_desde_plano", "Usar la capa seleccionada como dominio de cálculo", FALSE),
      selectInput(
        "modo_contorno_plano",
        "Interpretación del contorno:",
        choices = c("Trayectoria/polígono cerrado" = "closed", "Rectángulo envolvente (bbox)" = "bbox"),
        selected = "closed"
      )
    )
  })

  output$plan_raster_ui <- renderUI({
    p <- plano_raw()
    if (is.null(p) || p$kind != "raster") return(NULL)
    tagList(
      radioButtons(
        "escala_raster_modo",
        "Escala del plano de fondo:",
        choices = c("Ajustar a largo/ancho de la nave" = "fit", "Calibrar con dos puntos" = "calibrate"),
        selected = "fit"
      ),
      conditionalPanel(
        "input.escala_raster_modo == 'calibrate'",
        numericInput("distancia_calibracion_m", "Distancia real entre los dos puntos (m):", 10, min = 0.001),
        actionButton("reiniciar_calibracion", "Reiniciar calibración"),
        helpText("En la pestaña 'Plano importado', haga clic sobre dos puntos cuya distancia real conozca.")
      ),
      selectInput(
        "rotacion_raster",
        "Rotación del fondo:",
        choices = c("0°" = 0, "90°" = 90, "180°" = 180, "270°" = 270),
        selected = 0
      ),
      numericInput("offset_x_raster", "Desplazamiento X (m):", 0, step = 0.1),
      numericInput("offset_y_raster", "Desplazamiento Y (m):", 0, step = 0.1),
      sliderInput("alpha_raster", "Opacidad del plano:", min = 0.1, max = 1, value = 0.55, step = 0.05)
    )
  })

  observeEvent(input$importar_plano, {
    req(input$archivo_plano)
    # Copiamos primero los metadatos del upload. Después se libera el plano
    # anterior para impedir que capas, bbox, calibraciones o superficies 3D
    # de la importación previa queden activas durante la nueva lectura.
    path <- input$archivo_plano$datapath
    nombre_archivo <- input$archivo_plano$name
    ext <- tolower(tools::file_ext(nombre_archivo))
    modo <- input$modo_importacion_plano
    limpiar_estado_plano(limpiar_archivo = FALSE, mensaje = paste0("Importando ", nombre_archivo, "…"))

    plano <- NULL
    error_msg <- NULL

    # Tablas de coordenadas
    if (ext == "csv") {
      df <- leer_csv_auto(path)
      vec <- leer_tabla_plan(df, "CSV")
      if (!is.null(vec)) plano <- list(kind = "vector", data = vec, source = "CSV")
      else error_msg <- "El CSV debe contener x/y o x1/y1/x2/y2."
    }

    if (ext %in% c("xlsx", "xls")) {
      if (!requireNamespace("readxl", quietly = TRUE)) {
        error_msg <- "Para importar Excel se necesita el paquete opcional 'readxl'."
      } else {
        sheets <- tryCatch(readxl::excel_sheets(path), error = function(e) character())
        partes <- lapply(sheets, function(sh) {
          df <- tryCatch(as.data.frame(readxl::read_excel(path, sheet = sh)), error = function(e) NULL)
          leer_tabla_plan(df, sh)
        })
        partes <- Filter(Negate(is.null), partes)
        if (length(partes)) plano <- list(kind = "vector", data = do.call(rbind, partes), source = "Excel")
        else error_msg <- "No se encontraron hojas con coordenadas x/y o x1/y1/x2/y2."
      }
    }

    # CAD/GIS/PDF vectorial. Para DWG se usa primero GDAL y después LibreDWG como fallback.
    if (ext == "dwg") {
      dwg_res <- tryCatch(
        leer_dwg_con_fallback(path),
        error = function(e) list(data = NULL, method = NULL, diagnostic = conditionMessage(e))
      )
      if (!is.null(dwg_res$data) && nrow(dwg_res$data)) {
        plano <- list(
          kind = "vector",
          data = dwg_res$data,
          source = paste0("DWG (", dwg_res$method, ")"),
          unit_info = detectar_unidad_dwg(path)
        )
        plano_estado(paste0("DWG importado correctamente mediante ", dwg_res$method, "."))
      } else {
        tiene_libredwg <- nzchar(encontrar_ejecutable("dwgread")) || nzchar(encontrar_ejecutable("dwg2dxf"))
        if (!tiene_libredwg && Sys.info()[["sysname"]] == "Darwin") {
          error_msg <- paste0(
            "GDAL no dispone de lector DWG y GNU LibreDWG no está instalado. ",
            "En macOS: brew install libredwg."
          )
        } else if (!tiene_libredwg) {
          error_msg <- "No se encontró un lector DWG disponible (GDAL/LibreDWG)."
        } else {
          error_msg <- paste0(
            "No se pudo completar la importación del DWG. Diagnóstico:
",
            dwg_res$diagnostic
          )
        }
      }
    } else if (ext %in% c("dxf", "geojson", "json", "gpkg") || (ext == "pdf" && modo != "raster")) {
      vec <- tryCatch(leer_vector_gdal(path), error = function(e) NULL)
      if (!is.null(vec) && nrow(vec)) {
        plano <- list(kind = "vector", data = vec, source = toupper(ext))
      }
    }

    # PDF o imagen como fondo calibrable. En modo automático, PDF cae aquí si no pudo leerse como vector.
    if (is.null(plano) && ext %in% c("pdf", "png", "jpg", "jpeg", "tif", "tiff") && modo != "vector") {
      rr <- tryCatch(leer_raster_plan(path, ext), error = function(e) {
        error_msg <<- conditionMessage(e)
        NULL
      })
      if (!is.null(rr)) plano <- c(list(kind = "raster", source = toupper(ext)), rr)
    }

    if (is.null(plano)) {
      if (is.null(error_msg)) error_msg <- paste("No se pudo interpretar el archivo", nombre_archivo)
      plano_estado(error_msg)
      showNotification(error_msg, type = "error", duration = 8)
      return()
    }

    # Valida la coherencia física de la unidad antes de construir el modelo.
    # Evita la doble conversión observada en algunos DWG convertidos a GeoJSON.
    plano <- ajustar_unidad_cad_por_plausibilidad(plano)

    plano_raw(plano)
    clicks_calibracion(data.frame(x = numeric(), y = numeric()))
    pixels_por_metro(NA_real_)
    ventana_manual_plano(NULL)
    unidad_auto_aplicada(FALSE)

    if (plano$kind == "vector") {
      capas <- length(unique(plano$data$layer))
      unidad_txt <- ""
      if (!is.null(plano$unit_info)) {
        unidad_txt <- paste0(" INSUNITS detectado: ", plano$unit_info$label, " (código ", plano$unit_info$code, ").")
      }
      nota_escala <- if (!is.null(plano$scale_note)) paste0(" ", plano$scale_note) else ""
      plano_estado(paste0(
        "Plano vectorial importado: ", nombre_archivo,
        ". Capas detectadas: ", capas, ".", unidad_txt, nota_escala
      ))
    } else {
      plano_estado(paste0("Plano de fondo importado: ", nombre_archivo, " (", plano$width_px, " × ", plano$height_px, " px)."))
    }
    updateTabsetPanel(session, "main_tabs", selected = "Plano")
  })

  observeEvent(input$reiniciar_calibracion, {
    clicks_calibracion(data.frame(x = numeric(), y = numeric()))
    pixels_por_metro(NA_real_)
  })

  observeEvent(input$plano_click, {
    p <- plano_raw()
    if (is.null(p) || p$kind != "raster" || !identical(input$escala_raster_modo, "calibrate")) return()
    cl <- clicks_calibracion()
    if (nrow(cl) >= 2) cl <- data.frame(x = numeric(), y = numeric())
    cl <- rbind(cl, data.frame(x = input$plano_click$x, y = input$plano_click$y))
    clicks_calibracion(cl)

    distancia_real <- valor_num(input$distancia_calibracion_m, NA_real_)
    if (nrow(cl) == 2 && is.finite(distancia_real) && distancia_real > 0) {
      dpx <- sqrt((cl$x[2] - cl$x[1])^2 + (cl$y[2] - cl$y[1])^2)
      if (dpx > 0) {
        pixels_por_metro(dpx / distancia_real)
        plano_estado(paste0(
          "Calibración establecida: ", round(dpx, 1), " unidades gráficas = ",
          distancia_real, " m (", round(dpx / distancia_real, 2), " px/m)."
        ))
      }
    }
  })

  edificio_principal_raw <- reactive({
    p <- plano_raw()
    if (is.null(p) || p$kind != "vector") return(NULL)
    detectar_edificio_principal(p$data)
  })

  unidades_por_metro_reactivo <- reactive({
    unidades <- if (is.null(input$unidades_plano)) "unknown" else input$unidades_plano
    upm <- switch(
      unidades,
      unknown = 1,
      m = 1,
      cm = 100,
      mm = 1000,
      ft = 3.28083989501312,
      `in` = 39.3700787401575,
      custom = valor_num(input$unidades_por_metro, 1),
      1
    )
    if (!is.finite(upm) || upm <= 0) upm <- 1
    upm
  })

  plano_vector_transformado <- reactive({
    p <- plano_raw()
    if (is.null(p) || p$kind != "vector") return(NULL)
    df <- p$data
    if (!nrow(df)) return(NULL)

    upm <- unidades_por_metro_reactivo()
    df$x <- df$x / upm
    df$y <- df$y / upm

    core <- edificio_principal_raw()
    core_groups <- if (!is.null(core)) core$groups else unique(df$group)

    # El origen se fija con la planta completa útil. Así alas legítimas del
    # mismo plano (p. ej., un salón separado por una abertura) no quedan con
    # coordenadas negativas, mientras los outliers CAD siguen excluidos.
    ref_util <- filtrar_plano_completo_util_df(df, core_groups)
    ref_groups <- if (!is.null(ref_util) && nrow(ref_util)) unique(ref_util$group) else core_groups

    origen_min <- if (is.null(input$origen_minimo_plano)) TRUE else isTRUE(input$origen_minimo_plano)
    if (origen_min) {
      ref <- df[df$group %in% ref_groups, , drop = FALSE]
      if (!nrow(ref)) ref <- df
      df$x <- df$x - min(ref$x, na.rm = TRUE)
      df$y <- df$y - min(ref$y, na.rm = TRUE)
    }

    r <- rotar_xy(df$x, df$y, valor_num(input$rotacion_plano, 0))
    df$x <- r$x
    df$y <- r$y

    if (origen_min) {
      ref <- df[df$group %in% ref_groups, , drop = FALSE]
      if (!nrow(ref)) ref <- df
      df$x <- df$x - min(ref$x, na.rm = TRUE)
      df$y <- df$y - min(ref$y, na.rm = TRUE)
    }

    df$x <- df$x + valor_num(input$offset_x_plano, 0)
    df$y <- df$y + valor_num(input$offset_y_plano, 0)
    df
  })

  plano_vector_visible <- reactive({
    df <- plano_vector_transformado()
    if (is.null(df)) return(NULL)

    if (!is.null(input$capas_plano)) {
      df <- df[df$layer %in% input$capas_plano, , drop = FALSE]
    }

    mostrar_puntos <- if (is.null(input$mostrar_puntos_cad)) FALSE else isTRUE(input$mostrar_puntos_cad)
    if (!mostrar_puntos && nrow(df)) {
      es_punto <- toupper(df$geom_type) %in% c("POINT", "MULTIPOINT")
      tiene_texto <- if ("label" %in% names(df)) !is.na(df$label) & nzchar(trimws(df$label)) else rep(FALSE, nrow(df))
      # Los puntos de inserción se ocultan, pero las entidades de texto se
      # conservan para que la pestaña Plano pueda parecerse al CAD original.
      df <- df[!es_punto | tiene_texto, , drop = FALSE]
    }
    df
  })


  # Plano completo útil: conserva todos los grupos pertenecientes al dibujo
  # real (incluidos mobiliario, textos y zonas conectadas por huecos/puertas),
  # pero descarta grupos CAD claramente alejados del edificio principal. Esto
  # evita dos problemas opuestos: recortar una planta en varias "islas" y, a
  # la vez, dejar que un punto/outlier remoto multiplique artificialmente el
  # tamaño del dominio acústico.
  plano_vector_completo_util <- reactive({
    df <- plano_vector_transformado()
    if (is.null(df) || !nrow(df)) return(df)
    core <- edificio_principal_raw()
    if (is.null(core) || !length(core$groups)) return(df)
    filtrar_plano_completo_util_df(df, core$groups)
  })

  # La copia que se superpone sobre 2D/isófonas/3D debe corresponder al plano
  # completo útil, no al recorte del dominio principal. Las capas visibles se
  # respetan, pero el cálculo acústico y la visualización permanecen separados.
  plano_overlay_completo <- reactive({
    base <- if (!is.null(input$encuadre_plano) && identical(input$encuadre_plano, "todo")) {
      plano_vector_transformado()
    } else {
      plano_vector_completo_util()
    }
    if (is.null(base) || !nrow(base)) return(base)

    if (!is.null(input$capas_plano)) {
      base <- base[base$layer %in% input$capas_plano, , drop = FALSE]
    }
    if (!nrow(base)) return(base)

    if ("geom_type" %in% names(base)) {
      base <- base[!toupper(base$geom_type) %in% c("POINT", "MULTIPOINT"), , drop = FALSE]
    }
    base
  })

  plano_vector_modelo <- reactive({
    # El dominio acústico NO depende de las capas visibles.
    df <- plano_vector_transformado()
    if (is.null(df) || !nrow(df)) return(df)

    modo <- if (is.null(input$encuadre_plano)) "completo_auto" else input$encuadre_plano
    core <- edificio_principal_raw()

    if (identical(modo, "completo_auto")) {
      d2 <- plano_vector_completo_util()
      if (!is.null(d2) && nrow(d2)) df <- d2
    } else if (identical(modo, "principal") && !is.null(core) && length(core$groups)) {
      d2 <- df[df$group %in% core$groups, , drop = FALSE]
      if (nrow(d2)) df <- d2
    } else if (identical(modo, "manual")) {
      vm <- ventana_manual_plano()
      if (!is.null(vm) && all(is.finite(vm))) {
        dentro <- df$x >= vm["xmin"] & df$x <= vm["xmax"] &
          df$y >= vm["ymin"] & df$y <= vm["ymax"]
        grupos <- unique(df$group[dentro])
        if (length(grupos)) {
          df <- df[df$group %in% grupos, , drop = FALSE]
        }
      } else {
        d2 <- plano_vector_completo_util()
        if (!is.null(d2) && nrow(d2)) df <- d2
      }
    }

    df
  })

  # Geometria usada solo para dibujar el plano. No modifica barreras ni
  # dimensiones acusticas. En el modo recomendado conserva todas las capas
  # visuales pero las recorta al bbox del dominio, evitando outliers CAD.
  plano_vector_display <- reactive({
    vis <- plano_vector_visible()
    if (is.null(vis) || !nrow(vis)) return(vis)
    modo_vista <- if (is.null(input$vista_grafica_plano)) "dominio_detalle" else input$vista_grafica_plano

    if (identical(modo_vista, "todo")) return(vis)
    if (identical(modo_vista, "modelo")) return(plano_vector_modelo())

    mod <- plano_vector_modelo()
    if (is.null(mod) || !nrow(mod)) return(vis)
    xr <- range(mod$x, na.rm = TRUE); yr <- range(mod$y, na.rm = TRUE)
    mx <- max(diff(xr) * 0.015, 0.02); my <- max(diff(yr) * 0.015, 0.02)

    # No se cortan vértices individuales de una polilínea CAD. Se conservan
    # completos los grupos que intersectan el dominio; de lo contrario un
    # mueble, muro o bloque podía quedar visualmente amputado en el borde.
    dentro <- vis$x >= xr[1] - mx & vis$x <= xr[2] + mx &
      vis$y >= yr[1] - my & vis$y <= yr[2] + my
    grupos_dentro <- unique(vis$group[dentro])
    d <- if (length(grupos_dentro)) vis[vis$group %in% grupos_dentro, , drop = FALSE] else vis
    if (nrow(d)) d else vis
  })

  limites_vista_grafica <- reactive({
    df <- plano_vector_display()
    if (is.null(df) || !nrow(df)) return(NULL)
    xr <- range(df$x, na.rm = TRUE); yr <- range(df$y, na.rm = TRUE)
    if (!all(is.finite(c(xr, yr)))) return(NULL)
    mx <- max(diff(xr) * 0.03, 0.01); my <- max(diff(yr) * 0.03, 0.01)
    c(xmin = xr[1] - mx, xmax = xr[2] + mx, ymin = yr[1] - my, ymax = yr[2] + my)
  })

  limites_vista_plano <- reactive({
    modo <- if (is.null(input$encuadre_plano)) "completo_auto" else input$encuadre_plano

    if (identical(modo, "manual")) {
      vm <- ventana_manual_plano()
      if (!is.null(vm) && all(is.finite(vm))) return(vm)
    }

    df <- if (identical(modo, "todo")) plano_vector_visible() else plano_vector_modelo()
    if (is.null(df) || !nrow(df)) return(NULL)

    xr <- range(df$x, na.rm = TRUE)
    yr <- range(df$y, na.rm = TRUE)
    if (!all(is.finite(c(xr, yr)))) return(NULL)

    mx <- max(diff(xr) * 0.04, 0.01)
    my <- max(diff(yr) * 0.04, 0.01)
    c(xmin = xr[1] - mx, xmax = xr[2] + mx, ymin = yr[1] - my, ymax = yr[2] + my)
  })

  observeEvent(input$usar_brush_ventana, {
    b <- input$plano_brush
    if (is.null(b)) {
      showNotification("Seleccione primero una ventana rectangular sobre el plano.", type = "warning")
      return()
    }
    vm <- c(
      xmin = min(b$xmin, b$xmax),
      xmax = max(b$xmin, b$xmax),
      ymin = min(b$ymin, b$ymax),
      ymax = max(b$ymin, b$ymax)
    )
    if (!all(is.finite(vm)) || vm["xmax"] <= vm["xmin"] || vm["ymax"] <= vm["ymin"]) return()
    ventana_manual_plano(vm)
    updateRadioButtons(session, "encuadre_plano", selected = "manual")
  }, ignoreInit = TRUE)

  observeEvent(input$limpiar_ventana_manual, {
    ventana_manual_plano(NULL)
    updateRadioButtons(session, "encuadre_plano", selected = "completo_auto")
  }, ignoreInit = TRUE)

  output$edificio_principal_ui <- renderUI({
    p <- plano_raw()
    if (is.null(p) || p$kind != "vector") return(NULL)
    core <- edificio_principal_raw()
    if (is.null(core)) return(NULL)

    sx <- core$bbox["xmax"] - core$bbox["xmin"]
    sy <- core$bbox["ymax"] - core$bbox["ymin"]
    upm <- unidades_por_metro_reactivo()
    unidad_ok <- !is.null(input$unidades_plano) && !identical(input$unidades_plano, "unknown")

    texto_dim <- if (unidad_ok) {
      paste0(" Dimensiones del núcleo ≈ ", round(sx / upm, 3), " × ", round(sy / upm, 3), " m.")
    } else {
      paste0(" Extensión del núcleo ≈ ", round(sx, 3), " × ", round(sy, 3), " unidades CAD.")
    }

    tags$div(
      style = "font-size:12px;color:#444;background:#f6f8fa;padding:7px;margin:6px 0;border-radius:4px;",
      tags$b("Edificio principal detectado."),
      paste0(
        " Grupos incluidos: ", core$n_groups,
        "; grupos arquitectónicos aislados fuera del núcleo: ", max(0, core$excluded_groups), ".",
        texto_dim
      )
    )
  })

  output$unidades_detectadas_ui <- renderUI({
    p <- plano_raw()
    if (is.null(p) || p$kind != "vector") return(NULL)

    core <- edificio_principal_raw()
    if (is.null(core)) return(NULL)
    sx <- core$bbox["xmax"] - core$bbox["xmin"]
    sy <- core$bbox["ymax"] - core$bbox["ymin"]

    if (!is.null(p$scale_note)) {
      return(tags$div(
        style = "font-size:12px;color:#7a4f00;margin-top:-8px;margin-bottom:8px;background:#fff4d6;padding:6px;border-radius:4px;",
        tags$b("Escala CAD corregida automáticamente. "),
        p$scale_note,
        " Puede modificar manualmente la unidad si conoce otra escala real."
      ))
    }

    if (!is.null(p$unit_info)) {
      info <- p$unit_info
      if (!is.null(info$shiny) && !identical(info$shiny, "unknown") && is.finite(info$units_per_meter)) {
        return(tags$div(
          style = "font-size:12px;color:#385723;margin-top:-8px;margin-bottom:8px;background:#eef7e8;padding:6px;border-radius:4px;",
          tags$b("Unidad detectada en el DWG mediante INSUNITS: "),
          paste0(
            info$label, " (código ", info$code, "). ",
            "Dimensiones estimadas del edificio principal ≈ ",
            round(sx / info$units_per_meter, 3), " × ",
            round(sy / info$units_per_meter, 3), " m. ",
            "Puede modificar la unidad manualmente si la información del archivo CAD no corresponde a la escala real."
          )
        ))
      }

      if (!is.null(info$code) && identical(info$code, 0L)) {
        return(tags$div(
          style = "font-size:12px;color:#7a4f00;margin-top:-8px;margin-bottom:8px;background:#fff4d6;padding:6px;border-radius:4px;",
          tags$b("El DWG declara INSUNITS = 0 (sin unidades). "),
          paste0(
            "Edificio principal ≈ ", round(sx, 3), " × ", round(sy, 3),
            " unidades CAD. Confirme la unidad o use una escala personalizada."
          )
        ))
      }
    }

    tags$div(
      style = "font-size:12px;color:#555;margin-top:-8px;margin-bottom:8px;",
      tags$b("Unidad no declarada o no recuperable del archivo. "),
      paste0(
        "Edificio principal ≈ ", round(sx, 2), " × ", round(sy, 2), " unidades CAD. ",
        "Si son cm: ", round(sx / 100, 2), " × ", round(sy / 100, 2), " m; ",
        "si son mm: ", round(sx / 1000, 2), " × ", round(sy / 1000, 2), " m."
      )
    )
  })

  # Cuando INSUNITS es inequívoco, preselecciona la unidad del DWG. El usuario
  # sigue pudiendo corregirla manualmente.
  observe({
    p <- plano_raw()
    if (isTRUE(unidad_auto_aplicada())) return()
    if (is.null(p) || p$kind != "vector" || is.null(p$unit_info)) return()
    info <- p$unit_info
    if (is.null(info$shiny) || identical(info$shiny, "unknown")) {
      unidad_auto_aplicada(TRUE)
      return()
    }
    if (!is.null(input$unidades_plano)) {
      updateSelectInput(session, "unidades_plano", selected = info$shiny)
      if (identical(info$shiny, "custom") && is.finite(info$units_per_meter)) {
        updateNumericInput(session, "unidades_por_metro", value = info$units_per_meter)
      }
      unidad_auto_aplicada(TRUE)
    }
  })

  observeEvent(input$capas_detalle, {
    p <- plano_raw(); req(p, p$kind == "vector")
    capas <- sort(unique(p$data$layer))
    updateCheckboxGroupInput(session, "capas_plano", selected = seleccion_capas_visual_recomendada(capas))
  }, ignoreInit = TRUE)

  observeEvent(input$capas_arquitectura, {
    p <- plano_raw(); req(p, p$kind == "vector")
    capas <- sort(unique(p$data$layer))
    updateCheckboxGroupInput(session, "capas_plano", selected = seleccion_capas_recomendada(capas))
  }, ignoreInit = TRUE)

  observeEvent(input$capas_todas, {
    p <- plano_raw(); req(p, p$kind == "vector")
    capas <- sort(unique(p$data$layer))
    updateCheckboxGroupInput(session, "capas_plano", selected = capas)
  }, ignoreInit = TRUE)

  observe({
    if (!isTRUE(input$usar_plano_importado) || !isTRUE(input$actualizar_dimensiones_plano)) return()
    if (is.null(input$unidades_plano) || identical(input$unidades_plano, "unknown")) return()

    modo <- if (is.null(input$encuadre_plano)) "completo_auto" else input$encuadre_plano
    if (identical(modo, "manual")) {
      vm <- ventana_manual_plano()
      if (!is.null(vm) && all(is.finite(vm))) {
        lx <- vm["xmax"] - vm["xmin"]
        ly <- vm["ymax"] - vm["ymin"]
      } else {
        lx <- ly <- NA_real_
      }
    } else {
      df <- plano_vector_modelo()
      if (is.null(df) || !nrow(df)) return()
      lx <- diff(range(df$x, na.rm = TRUE))
      ly <- diff(range(df$y, na.rm = TRUE))
    }

    if (is.finite(lx) && lx > 0) updateNumericInput(session, "largo", value = round(unname(lx), 3))
    if (is.finite(ly) && ly > 0) updateNumericInput(session, "ancho", value = round(unname(ly), 3))
  })

  plano_raster_transformado <- reactive({
    p <- plano_raw()
    if (is.null(p) || p$kind != "raster") return(NULL)

    ras <- p$raster
    rot <- valor_num(input$rotacion_raster, 0)
    k <- as.integer(round((rot %% 360) / 90))
    if (k == 1) ras <- t(apply(ras, 2, rev))
    if (k == 2) ras <- ras[nrow(ras):1, ncol(ras):1, drop = FALSE]
    if (k == 3) ras <- t(apply(ras, 1, rev))
    ras <- as.raster(ras)

    alpha_val <- min(1, max(0.05, valor_num(input$alpha_raster, 0.55)))
    cols <- scales::alpha(as.character(ras), alpha_val)
    dim(cols) <- dim(ras)
    ras <- as.raster(cols)

    wpx <- ncol(ras)
    hpx <- nrow(ras)
    modo_escala <- if (is.null(input$escala_raster_modo)) "fit" else input$escala_raster_modo
    ppm <- pixels_por_metro()

    if (identical(modo_escala, "calibrate") && length(ppm) == 1 && is.finite(ppm) && ppm > 0) {
      w <- wpx / ppm
      h <- hpx / ppm
    } else {
      w <- valor_num(input$largo, 100)
      h <- valor_num(input$ancho, 50)
    }

    ox <- valor_num(input$offset_x_raster, 0)
    oy <- valor_num(input$offset_y_raster, 0)
    list(raster = ras, xmin = ox, xmax = ox + w, ymin = oy, ymax = oy + h)
  })

  contorno_desde_plano <- reactive({
    if (!isTRUE(input$usar_plano_importado) || !isTRUE(input$usar_contorno_desde_plano)) return(NULL)
    if (is.null(input$unidades_plano) || identical(input$unidades_plano, "unknown")) return(NULL)
    df <- plano_vector_transformado()
    if (is.null(df) || !nrow(df) || is.null(input$capa_contorno_plano) || !nzchar(input$capa_contorno_plano)) return(NULL)

    modo <- if (is.null(input$encuadre_plano)) "completo_auto" else input$encuadre_plano
    core <- edificio_principal_raw()
    if (identical(modo, "completo_auto")) {
      d2 <- plano_vector_completo_util()
      if (!is.null(d2) && nrow(d2)) df <- d2
    } else if (identical(modo, "principal") && !is.null(core) && length(core$groups)) {
      df <- df[df$group %in% core$groups, , drop = FALSE]
    } else if (identical(modo, "manual")) {
      vm <- ventana_manual_plano()
      if (!is.null(vm) && all(is.finite(vm))) {
        dentro <- df$x >= vm["xmin"] & df$x <= vm["xmax"] &
          df$y >= vm["ymin"] & df$y <= vm["ymax"]
        grupos <- unique(df$group[dentro])
        if (length(grupos)) df <- df[df$group %in% grupos, , drop = FALSE]
      }
    }

    df <- df[df$layer == input$capa_contorno_plano, , drop = FALSE]
    if (nrow(df) < 3) return(NULL)

    modo_contorno <- if (is.null(input$modo_contorno_plano)) "closed" else input$modo_contorno_plano
    if (identical(modo_contorno, "bbox")) {
      xr <- range(df$x, na.rm = TRUE); yr <- range(df$y, na.rm = TRUE)
      return(data.frame(
        x = c(xr[1], xr[2], xr[2], xr[1], xr[1]),
        y = c(yr[1], yr[1], yr[2], yr[2], yr[1])
      ))
    }

    grupos <- split(df, df$group)
    candidatos <- lapply(grupos, function(g) {
      g2 <- cerrar_poligono_df(g)
      if (is.null(g2)) return(NULL)
      if (sqrt((g2$x[1] - g2$x[nrow(g2)])^2 + (g2$y[1] - g2$y[nrow(g2)])^2) > 1e-6) return(NULL)
      list(coords = g2, area = area_poligono_xy(g2$x, g2$y))
    })
    candidatos <- Filter(Negate(is.null), candidatos)
    if (!length(candidatos)) return(NULL)
    candidatos[[which.max(vapply(candidatos, function(z) z$area, numeric(1)))]]$coords
  })

  output$tabla_capas_plano <- renderTable({
    if (!isTRUE(input$usar_plano_importado)) return(NULL)
    p <- plano_raw()
    if (is.null(p) || p$kind != "vector") return(NULL)
    df <- p$data
    agg <- aggregate(group ~ layer, data = unique(df[, c("layer", "group")]), FUN = length)
    names(agg) <- c("Capa", "Elementos")
    agg
  }, striped = TRUE, bordered = TRUE)

  output$plano_preview <- renderPlot({
    p <- plano_raw()

    # En modo manual, la pestaña Plano muestra la geometría básica programada
    # en lugar de pedir un archivo importado.
    if (!isTRUE(input$usar_plano_importado)) {
      L <- valor_num(input$largo, 100)
      A <- valor_num(input$ancho, 50)
      if (!is.finite(L) || L <= 0) L <- 100
      if (!is.finite(A) || A <= 0) A <- 50
      base <- data.frame(x = c(0, L, L, 0, 0), y = c(0, 0, A, A, 0))

      cont <- NULL
      if (isTRUE(input$usar_contorno) && !is.null(input$tabla_contorno)) {
        cont <- tryCatch(hot_to_r(input$tabla_contorno), error = function(e) NULL)
        if (!is.null(cont) && all(c("x", "y") %in% names(cont))) {
          cont <- cont[complete.cases(cont[, c("x", "y")]), c("x", "y"), drop = FALSE]
          cont$x <- suppressWarnings(as.numeric(cont$x)); cont$y <- suppressWarnings(as.numeric(cont$y))
          cont <- cont[is.finite(cont$x) & is.finite(cont$y), , drop = FALSE]
          if (nrow(cont) >= 3 && (cont$x[1] != cont$x[nrow(cont)] || cont$y[1] != cont$y[nrow(cont)])) cont <- rbind(cont, cont[1, , drop = FALSE])
          if (nrow(cont) < 3) cont <- NULL
        } else cont <- NULL
      }

      if (!is.null(cont)) {
        limx <- range(cont$x, finite = TRUE); limy <- range(cont$y, finite = TRUE)
        dx <- max(diff(limx), 1); dy <- max(diff(limy), 1)
        g <- ggplot(cont, aes(x = x, y = y)) +
          geom_path(linewidth = 0.9, color = "#344054") +
          coord_equal(xlim = limx + c(-0.03, 0.03) * dx, ylim = limy + c(-0.03, 0.03) * dy, expand = FALSE) +
          theme_minimal() +
          labs(title = "Contorno programado manualmente", subtitle = "Contorno personalizado/CSV", x = "X (m)", y = "Y (m)")
      } else {
        g <- ggplot(base, aes(x = x, y = y)) +
          geom_path(linewidth = 0.9, color = "#344054") +
          coord_equal(xlim = c(0, L), ylim = c(0, A), expand = FALSE) +
          theme_minimal() +
          labs(title = "Geometría programada manualmente", subtitle = paste0(round(L, 2), " × ", round(A, 2), " m"), x = "X (m)", y = "Y (m)")
      }
      print(g)
      return(invisible(NULL))
    }

    if (is.null(p)) {
      plot.new(); text(0.5, 0.5, "Seleccione un archivo en Programación > Plano y geometría")
      return()
    }

    if (p$kind == "vector") {
      df <- plano_vector_display()
      if (is.null(df) || !nrow(df)) return(NULL)
      es_punto <- toupper(df$geom_type) %in% c("POINT", "MULTIPOINT")
      tiene_texto <- if ("label" %in% names(df)) !is.na(df$label) & nzchar(trimws(df$label)) else rep(FALSE, nrow(df))
      line_df <- df[!es_punto, , drop = FALSE]
      point_df <- df[es_punto & !tiene_texto, , drop = FALSE]
      text_df <- df[tiene_texto, , drop = FALSE]
      if (nrow(text_df)) text_df <- text_df[!duplicated(text_df[, c("group", "label")]), , drop = FALSE]

      g <- ggplot()
      if (nrow(line_df)) g <- g + geom_path(data = line_df, aes(x = x, y = y, group = group, color = layer), linewidth = 0.55)
      if (nrow(point_df)) g <- g + geom_point(data = point_df, aes(x = x, y = y, color = layer), size = 1.4)
      if (nrow(text_df)) g <- g + geom_text(data = text_df, aes(x = x, y = y, label = label, color = layer), size = 2.6, check_overlap = TRUE, show.legend = FALSE)
      unidad_confirmada <- !is.null(input$unidades_plano) && !identical(input$unidades_plano, "unknown")
      etiqueta_eje <- if (unidad_confirmada) "m" else "unidades CAD (sin confirmar)"
      lim <- limites_vista_grafica()
      if (!is.null(lim)) {
        g <- g + coord_equal(
          xlim = c(unname(lim["xmin"]), unname(lim["xmax"])),
          ylim = c(unname(lim["ymin"]), unname(lim["ymax"])),
          expand = FALSE
        )
      } else {
        g <- g + coord_equal()
      }
      g +
        theme_minimal() +
        labs(x = paste0("X (", etiqueta_eje, ")"), y = paste0("Y (", etiqueta_eje, ")"), color = "Capa")
    } else {
      # Para calibrar se usa el sistema de coordenadas original del raster (píxeles/unidades gráficas).
      ras <- p$raster
      cl <- clicks_calibracion()
      g <- ggplot() +
        annotation_raster(ras, xmin = 0, xmax = p$width_px, ymin = 0, ymax = p$height_px) +
        coord_fixed(xlim = c(0, p$width_px), ylim = c(0, p$height_px), expand = FALSE) +
        theme_void()
      if (nrow(cl)) g <- g + geom_point(data = cl, aes(x = x, y = y), size = 3)
      if (nrow(cl) == 2) g <- g + geom_segment(data = data.frame(x = cl$x[1], y = cl$y[1], xend = cl$x[2], yend = cl$y[2]), aes(x = x, y = y, xend = xend, yend = yend), linewidth = 1)
      g
    }
  })

  # ---------------------------------------------------------------------------
  # Entradas dinámicas
  # ---------------------------------------------------------------------------

  output$maquinas_ui <- renderUI({
    req(input$n_maquinas)
    lapply(seq_len(input$n_maquinas), function(i) {
      tagList(
        textInput(paste0("nombre_", i), paste("Máquina", i), value = paste0("M", i)),
        numericInput(paste0("x_", i), "X:", value = i * 10),
        numericInput(paste0("y_", i), "Y:", value = i * 5),
        numericInput(
          paste0("Lp_", i),
          "Lᵢ = nivel de presión sonora a 1 m [dB(A)]:",
          value = 100
        )
      )
    })
  })

  output$trabajadores_ui <- renderUI({
    req(input$n_trabajadores)
    lapply(seq_len(input$n_trabajadores), function(i) {
      tagList(
        textInput(
          paste0("trab_nombre_", i),
          paste("Trabajador", i),
          value = paste0("T", i)
        ),
        numericInput(paste0("trab_x_", i), "X:", value = i * 5),
        numericInput(paste0("trab_y_", i), "Y:", value = i * 5),
        conditionalPanel(
          condition = "input.usar_epi == true",
          numericInput(
            paste0("trab_lceq_", i),
            "LCeq,T medido en el puesto [dB(C)]:",
            value = NA
          )
        )
      )
    })
  })

  # ---------------------------------------------------------------------------
  # Contorno
  # ---------------------------------------------------------------------------

  contorno_reactivo <- reactiveVal(NULL)

  observeEvent(input$archivo_contorno, {
    req(input$archivo_contorno)

    df <- tryCatch(
      read.csv(input$archivo_contorno$datapath, check.names = FALSE),
      error = function(e) NULL
    )

    if (is.null(df)) {
      showNotification("No se pudo leer el archivo CSV.", type = "error")
      return()
    }

    names(df) <- tolower(trimws(names(df)))

    if (!all(c("x", "y") %in% names(df))) {
      showNotification("El CSV debe tener columnas 'x' e 'y'.", type = "error")
      return()
    }

    df <- df[, c("x", "y")]
    df <- df[complete.cases(df), , drop = FALSE]

    if (nrow(df) < 3) {
      showNotification("El contorno necesita al menos tres vértices válidos.", type = "error")
      return()
    }

    contorno_reactivo(df)
  })

  output$tabla_contorno <- renderRHandsontable({
    datos <- contorno_reactivo()

    if (is.null(datos)) {
      datos <- data.frame(
        x = c(0, 0, 20, 40, 60),
        y = c(0, 40, 50, 50, 40)
      )
    }

    rhandsontable(datos, rowHeaders = NULL)
  })

  obtener_contorno <- reactive({
    # Si se ha seleccionado un contorno procedente del plano importado, tiene prioridad.
    cp <- contorno_desde_plano()
    if (!is.null(cp)) return(cp)

    if (!isTRUE(input$usar_contorno) || is.null(input$tabla_contorno)) return(NULL)

    coords <- hot_to_r(input$tabla_contorno)
    coords <- coords[complete.cases(coords), , drop = FALSE]
    coords <- coords[is.finite(coords$x) & is.finite(coords$y), , drop = FALSE]

    if (nrow(coords) < 3) return(NULL)

    # Cierre automático del polígono si el usuario no repite el primer vértice.
    primer_vertice <- c(coords$x[1], coords$y[1])
    ultimo_vertice <- c(coords$x[nrow(coords)], coords$y[nrow(coords)])
    if (!all(primer_vertice == ultimo_vertice)) {
      coords <- rbind(coords, coords[1, , drop = FALSE])
    }

    coords
  })

  # ---------------------------------------------------------------------------
  # Mediciones para estimación
  # ---------------------------------------------------------------------------

  output$tabla_mediciones <- renderRHandsontable({
    if (is.null(input$tabla_mediciones)) {
      df <- data.frame(x = c(5, 20), y = c(5, 15), Lp = c(95, 99))
      rhandsontable(df, rowHeaders = NULL)
    } else {
      rhandsontable(hot_to_r(input$tabla_mediciones), rowHeaders = NULL)
    }
  })

  # ---------------------------------------------------------------------------
  # Datos de máquinas, trabajadores y elementos gráficos
  # ---------------------------------------------------------------------------

  maquinas_df <- reactive({
    # Los controles nombre_i/x_i/y_i/Lp_i se generan mediante renderUI dentro
    # de una subpestaña. Shiny puede suspender ese output mientras la pestaña
    # está oculta. Por ello el motor no depende de que el usuario la haya
    # abierto: usa exactamente los mismos valores iniciales como respaldo.
    n <- as.integer(input_or(input$n_maquinas, 2))
    n <- max(1L, min(200L, n))

    data.frame(
      nombre = vapply(seq_len(n), function(i)
        input_or(input[[paste0("nombre_", i)]], paste0("M", i)), character(1)),
      x = vapply(seq_len(n), function(i)
        input_or(input[[paste0("x_", i)]], i * 10), numeric(1)),
      y = vapply(seq_len(n), function(i)
        input_or(input[[paste0("y_", i)]], i * 5), numeric(1)),
      Lp = vapply(seq_len(n), function(i)
        input_or(input[[paste0("Lp_", i)]], 100), numeric(1)),
      stringsAsFactors = FALSE
    )
  })

  output$ui_cerramientos <- renderUI({
    req(input$usar_cerramiento)
    maquinas <- maquinas_df()

    lapply(seq_len(nrow(maquinas)), function(i) {
      numericInput(
        paste0("cerramiento_", i),
        label = paste("Reducción para", maquinas$nombre[i], "[dB]:"),
        value = 0,
        min = 0,
        max = 60
      )
    })
  })

  Li_ajustado <- reactive({
    maquinas <- maquinas_df()
    Li <- maquinas$Lp

    if (isTRUE(input$usar_cerramiento)) {
      reducciones <- sapply(seq_len(length(Li)), function(i) {
        val <- input[[paste0("cerramiento_", i)]]
        if (is.null(val) || !is.finite(val)) 0 else val
      })
      Li <- Li - reducciones
    }

    Li
  })

  trabajadores_df <- reactive({
    if (!isTRUE(input$incluir_trabajadores)) return(NULL)
    n <- as.integer(input_or(input$n_trabajadores, 1))
    n <- max(1L, min(500L, n))

    data.frame(
      nombre = vapply(seq_len(n), function(i)
        input_or(input[[paste0("trab_nombre_", i)]], paste0("T", i)), character(1)),
      x = vapply(seq_len(n), function(i)
        input_or(input[[paste0("trab_x_", i)]], i * 5), numeric(1)),
      y = vapply(seq_len(n), function(i)
        input_or(input[[paste0("trab_y_", i)]], i * 5), numeric(1)),
      LCeqT = vapply(seq_len(n), function(i) {
        val <- input[[paste0("trab_lceq_", i)]]
        if (is.null(val) || length(val) == 0 || !is.finite(suppressWarnings(as.numeric(val[1])))) NA_real_ else as.numeric(val[1])
      }, numeric(1)),
      stringsAsFactors = FALSE
    )
  })

  puerta_df <- reactive({
    if (!isTRUE(input$incluir_puerta)) return(NULL)
    data.frame(x = input$puerta_x, y = input$puerta_y)
  })

  oficinas_df <- reactive({
    if (!isTRUE(input$incluir_oficinas)) return(NULL)
    data.frame(
      xmin = input$ofi_xmin,
      xmax = input$ofi_xmax,
      ymin = input$ofi_ymin,
      ymax = input$ofi_ymax
    )
  })

  # ---------------------------------------------------------------------------
  # Barreras acústicas derivadas del plano CAD
  # ---------------------------------------------------------------------------

  segmentos_barrera <- reactive({
    if (!isTRUE(input$usar_plano_importado) || !isTRUE(input$usar_paredes_acusticas)) return(data.frame())
    p <- plano_raw()
    if (is.null(p) || p$kind != "vector") return(data.frame())
    capas_b <- input$capas_barrera_acustica
    if (is.null(capas_b) || !length(capas_b)) return(data.frame())

    # Partimos de toda la geometría transformada y aplicamos el mismo encuadre
    # espacial que al edificio, pero las barreras no dependen de que la capa
    # esté visible en el gráfico.
    df <- plano_vector_transformado()
    if (is.null(df) || !nrow(df)) return(data.frame())

    modo <- if (is.null(input$encuadre_plano)) "completo_auto" else input$encuadre_plano
    core <- edificio_principal_raw()
    if (identical(modo, "completo_auto")) {
      d2 <- plano_vector_completo_util()
      if (!is.null(d2) && nrow(d2)) df <- d2
    } else if (identical(modo, "principal") && !is.null(core) && length(core$groups)) {
      d2 <- df[df$group %in% core$groups, , drop = FALSE]
      if (nrow(d2)) df <- d2
    } else if (identical(modo, "manual")) {
      vm <- ventana_manual_plano()
      if (!is.null(vm) && all(is.finite(vm))) {
        dentro <- df$x >= vm["xmin"] & df$x <= vm["xmax"] &
          df$y >= vm["ymin"] & df$y <= vm["ymax"]
        grupos <- unique(df$group[dentro])
        if (length(grupos)) df <- df[df$group %in% grupos, , drop = FALSE]
      }
    }

    df <- df[df$layer %in% capas_b, , drop = FALSE]
    if (!nrow(df)) return(data.frame())
    if ("geom_type" %in% names(df)) {
      df <- df[!toupper(df$geom_type) %in% c("POINT", "MULTIPOINT"), , drop = FALSE]
    }
    segmentos_desde_paths(df)
  })

  # ---------------------------------------------------------------------------
  # Aproximación geométrica de recintos para reverberación
  # ---------------------------------------------------------------------------

  # Distancia desde un punto hasta el primer segmento de barrera encontrado
  # en una dirección dada. La dirección se normaliza internamente.
  distancia_primer_muro <- function(px, py, vx, vy, seg, max_dist = Inf) {
    if (is.null(seg) || !nrow(seg) || !all(is.finite(c(px, py, vx, vy)))) return(NA_real_)
    nv <- sqrt(vx^2 + vy^2)
    if (!is.finite(nv) || nv < 1e-12) return(NA_real_)
    dx <- vx / nv; dy <- vy / nv

    wx <- seg$x2 - seg$x1
    wy <- seg$y2 - seg$y1
    den <- dx * wy - dy * wx
    ok <- is.finite(den) & abs(den) > 1e-10
    if (!any(ok)) return(NA_real_)

    qx <- seg$x1 - px
    qy <- seg$y1 - py
    t <- rep(NA_real_, nrow(seg))
    u <- rep(NA_real_, nrow(seg))
    t[ok] <- (qx[ok] * wy[ok] - qy[ok] * wx[ok]) / den[ok]
    u[ok] <- (qx[ok] * dy - qy[ok] * dx) / den[ok]

    eps <- 1e-5
    hit <- ok & t > eps & u >= -eps & u <= (1 + eps)
    vals <- t[hit]
    vals <- vals[is.finite(vals) & vals > 0]
    if (is.finite(max_dist)) vals <- vals[vals <= max_dist]
    if (!length(vals)) return(NA_real_)
    min(vals)
  }

  # Estima un rectángulo acústico local alrededor de una fuente a partir de
  # los primeros muros encontrados en ±X y ±Y. Si falta algún cierre se usa
  # el límite general de la nave como respaldo. Es una aproximación práctica,
  # no una reconstrucción topológica exacta del recinto.
  dimensiones_recinto_local <- function(sx, sy, seg) {
    L <- max(valor_num(input$largo, 1), 0.5)
    W <- max(valor_num(input$ancho, 1), 0.5)

    # Distancias hasta el borde global, suponiendo coordenadas normalizadas.
    fallback <- c(
      izquierda = max(sx, 0.25),
      derecha   = max(L - sx, 0.25),
      abajo     = max(sy, 0.25),
      arriba    = max(W - sy, 0.25)
    )

    if (is.null(seg) || !nrow(seg)) {
      return(list(largo = L, ancho = W, origen = "nave completa"))
    }

    d <- c(
      izquierda = distancia_primer_muro(sx, sy, -1,  0, seg, max_dist = L + W),
      derecha   = distancia_primer_muro(sx, sy,  1,  0, seg, max_dist = L + W),
      abajo     = distancia_primer_muro(sx, sy,  0, -1, seg, max_dist = L + W),
      arriba    = distancia_primer_muro(sx, sy,  0,  1, seg, max_dist = L + W)
    )
    for (nm in names(d)) {
      if (!is.finite(d[nm]) || d[nm] < 0.20) d[nm] <- fallback[nm]
    }

    largo_local <- min(L, max(0.5, d["izquierda"] + d["derecha"]))
    ancho_local <- min(W, max(0.5, d["abajo"] + d["arriba"]))
    list(largo = unname(largo_local), ancho = unname(ancho_local), origen = "muros próximos")
  }

  constante_sala_dim <- function(largo, ancho, altura, alpha) {
    if (!all(is.finite(c(largo, ancho, altura, alpha))) ||
        largo <= 0 || ancho <= 0 || altura <= 0 || alpha <= 0 || alpha >= 1) return(NA_real_)
    S <- 2 * (largo * ancho + largo * altura + ancho * altura)
    A <- S * alpha
    R <- A / (1 - alpha)
    if (!is.finite(R) || R <= 0) NA_real_ else R
  }

  constante_sala_fuente <- function(sx, sy, seg, modelo = "recinto") {
    alpha <- coef_absorcion()
    H <- valor_num(input$altura_techo, 5)
    if (!is.finite(alpha) || !is.finite(H) || H <= 0) return(NA_real_)

    if (identical(modelo, "recinto")) {
      dims <- dimensiones_recinto_local(sx, sy, seg)
      constante_sala_dim(dims$largo, dims$ancho, H, alpha)
    } else {
      constante_sala_dim(
        max(valor_num(input$largo, 1), 0.5),
        max(valor_num(input$ancho, 1), 0.5),
        H,
        alpha
      )
    }
  }

  # ---------------------------------------------------------------------------
  # Modelo acústico
  # ---------------------------------------------------------------------------

  superficie_total <- reactive({
    req(input$largo, input$ancho, input$altura_techo)
    2 * (
      input$largo * input$ancho +
        input$largo * input$altura_techo +
        input$ancho * input$altura_techo
    )
  })

  coef_absorcion <- reactive({
    alpha <- as.numeric(input$material_reverberacion)
    if (!is.finite(alpha) || alpha <= 0 || alpha >= 1) return(NA_real_)
    alpha
  })

  area_absorcion <- reactive({
    alpha <- coef_absorcion()
    if (!is.finite(alpha)) return(NA_real_)
    superficie_total() * alpha
  })

  constante_sala <- reactive({
    alpha <- coef_absorcion()
    A <- area_absorcion()
    if (!is.finite(alpha) || !is.finite(A)) return(NA_real_)
    A / (1 - alpha)
  })

  calcular_Lp <- function(px, py, maquinas, Li, incluir_reverberacion = isTRUE(input$usar_reverberacion)) {
    if (nrow(maquinas) == 0 || length(Li) != nrow(maquinas)) return(NA_real_)

    niveles_fuente <- numeric(nrow(maquinas))
    seg <- segmentos_barrera()
    usar_barreras <- isTRUE(input$usar_paredes_acusticas) && !is.null(seg) && nrow(seg) > 0
    modelo_paredes <- if (is.null(input$modelo_paredes)) "hibrido" else input$modelo_paredes
    perdida_primera <- valor_num(input$atenuacion_cruce_pared, 12)
    perdida_adicional <- valor_num(input$atenuacion_pared_adicional, 6)
    perdida_max <- valor_num(input$atenuacion_pared_max, 30)
    perdida_borde <- valor_num(input$perdida_borde, 6)
    modelo_reverberacion <- if (is.null(input$modelo_reverberacion)) "recinto" else input$modelo_reverberacion
    agrupar_m <- valor_num(input$agrupar_cruces_m, 0.30)

    for (i in seq_len(nrow(maquinas))) {
      sx <- maquinas$x[i]; sy <- maquinas$y[i]
      r <- sqrt((px - sx)^2 + (py - sy)^2)
      r <- max(r, 1)

      hits <- data.frame()
      n_cruces <- 0L
      if (usar_barreras) {
        hits <- intersecciones_rayo_barreras_detalle(
          sx, sy, px, py, seg, agrupar_m
        )
        n_cruces <- nrow(hits)
      }

      # Componente directa en campo libre.
      L_directa_libre <- Li[i] - 20 * log10(r)

      if (n_cruces > 0) {
        if (identical(modelo_paredes, "hibrido")) {
          perdida_trans <- perdida_pared_saturada(
            n_cruces,
            primera = perdida_primera,
            adicional = perdida_adicional,
            maxima = perdida_max
          )
          L_transmitida <- L_directa_libre - perdida_trans

          L_borde <- nivel_borde_aproximado(
            sx, sy, px, py, Li[i], seg, hits,
            perdida_borde = perdida_borde,
            penalizacion_otras = max(1, perdida_adicional / 2)
          )

          componentes_directas <- c(L_transmitida)
          if (is.finite(L_borde)) componentes_directas <- c(componentes_directas, L_borde)
          L_directa <- db_sum(componentes_directas)
        } else {
          L_directa <- L_directa_libre - perdida_primera * n_cruces
        }
      } else {
        L_directa <- L_directa_libre
      }

      if (isTRUE(incluir_reverberacion)) {
        # Reverberación simplificada por recinto:
        # - En modo "recinto" el campo difuso de la fuente solo se suma cuando
        #   fuente y receptor no están separados por una barrera seleccionada.
        # - Si existe un cerramiento, la energía que llega al otro lado ya está
        #   representada por transmisión/difracción en L_directa; no se hace
        #   atravesar artificialmente el campo reverberante restando X dB.
        # - En modo "global" se conserva una comparación de nave única sin
        #   compartimentación del campo difuso.
        sumar_difuso <- identical(modelo_reverberacion, "global") || n_cruces == 0

        if (sumar_difuso) {
          R <- constante_sala_fuente(
            sx, sy, seg,
            modelo = if (identical(modelo_reverberacion, "recinto")) "recinto" else "global"
          )
          if (!is.finite(R) || R <= 0) {
            niveles_fuente[i] <- L_directa
          } else {
            Q <- 1
            Lw <- Li[i] + 10 * log10(4 * pi / Q)
            L_difusa <- Lw + 10 * log10(4 / R)
            niveles_fuente[i] <- db_sum(c(L_directa, L_difusa))
          }
        } else {
          niveles_fuente[i] <- L_directa
        }
      } else {
        niveles_fuente[i] <- L_directa
      }
    }

    db_sum(niveles_fuente)
  }

  # ---------------------------------------------------------------------------
  # Estimación de L_i a partir de mediciones
  # ---------------------------------------------------------------------------

  observeEvent(input$estimar_Li, {
    req(input$usar_estimacion_Li)
    req(input$tabla_mediciones)

    puntos <- hot_to_r(input$tabla_mediciones)
    puntos <- puntos[complete.cases(puntos), , drop = FALSE]
    maquinas <- maquinas_df()

    if (nrow(puntos) < nrow(maquinas)) {
      showNotification("Para una estimación estable se recomiendan al menos tantos puntos de medición como fuentes.", type = "warning", duration = 6)
    }

    if (nrow(puntos) == 0 || any(!is.finite(puntos$x)) || any(!is.finite(puntos$y)) || any(!is.finite(puntos$Lp))) {
      showNotification("Las mediciones contienen datos no válidos.", type = "error")
      return()
    }

    estimar_Lp <- function(Li) {
      mapply(
        calcular_Lp,
        puntos$x,
        puntos$y,
        MoreArgs = list(
          maquinas = maquinas,
          Li = Li,
          incluir_reverberacion = isTRUE(input$usar_reverberacion)
        )
      )
    }

    error <- function(Li) {
      pred <- estimar_Lp(Li)
      sum((pred - puntos$Lp)^2)
    }

    resultado <- optim(
      par = rep(95, nrow(maquinas)),
      fn = error,
      lower = rep(40, nrow(maquinas)),
      upper = rep(140, nrow(maquinas)),
      method = "L-BFGS-B"
    )

    Li_estimado <- round(resultado$par, 2)
    names(Li_estimado) <- maquinas$nombre

    output$resultado_Li <- renderPrint({
      cat("Niveles estimados Lᵢ a 1 m [dB(A)]:\n")
      print(Li_estimado)
      cat("\nError cuadrático total:", round(resultado$value, 2), "\n")
      if (isTRUE(input$usar_reverberacion)) {
        cat("Estimación realizada con reverberación activada (modelo: ", if (is.null(input$modelo_reverberacion)) "recinto" else input$modelo_reverberacion, ").\n", sep = "")
      }
    })

    lapply(seq_len(nrow(maquinas)), function(i) {
      updateNumericInput(session, paste0("Lp_", i), value = Li_estimado[i])
    })
  })

  # ---------------------------------------------------------------------------
  # Malla de cálculo
  # ---------------------------------------------------------------------------

  puntos_df <- reactive({
    coords <- obtener_contorno()

    if (!is.null(coords)) {
      xmin <- min(coords$x, na.rm = TRUE)
      xmax <- max(coords$x, na.rm = TRUE)
      ymin <- min(coords$y, na.rm = TRUE)
      ymax <- max(coords$y, na.rm = TRUE)
    } else if (isTRUE(input$usar_plano_importado) &&
               !is.null(input$unidades_plano) && !identical(input$unidades_plano, "unknown") &&
               !is.null(plano_raw()) && identical(plano_raw()$kind, "vector")) {
      dm <- plano_vector_modelo()
      if (!is.null(dm) && nrow(dm)) {
        xmin <- min(dm$x, na.rm = TRUE)
        xmax <- max(dm$x, na.rm = TRUE)
        ymin <- min(dm$y, na.rm = TRUE)
        ymax <- max(dm$y, na.rm = TRUE)
      } else {
        xmin <- 0; xmax <- valor_num(input$largo, 1)
        ymin <- 0; ymax <- valor_num(input$ancho, 1)
      }
    } else {
      xmin <- 0
      xmax <- valor_num(input$largo, 1)
      ymin <- 0
      ymax <- valor_num(input$ancho, 1)
    }

    dx <- xmax - xmin
    dy <- ymax - ymin
    validate(need(is.finite(dx) && is.finite(dy) && dx > 0 && dy > 0,
                  "El dominio de cálculo no tiene dimensiones válidas."))

    # Resolución adaptativa: mantiene aproximadamente 120-180 nodos por eje y
    # garantiza al menos una malla 3x3 incluso para dominios pequeños.
    max_dim <- max(dx, dy)
    resolucion <- min(0.50, max(0.02, max_dim / 160))
    nx <- max(3L, ceiling(dx / resolucion) + 1L)
    ny <- max(3L, ceiling(dy / resolucion) + 1L)

    puntos <- expand.grid(
      x = seq(xmin, xmax, length.out = nx),
      y = seq(ymin, ymax, length.out = ny)
    )

    if (!is.null(coords)) {
      poligono <- st_sfc(st_polygon(list(as.matrix(coords[, c("x", "y")]))))
      puntos_sf <- st_as_sf(puntos, coords = c("x", "y"))
      dentro <- st_within(puntos_sf, poligono, sparse = FALSE)[, 1]
      puntos <- puntos[dentro, , drop = FALSE]
    }

    maquinas <- maquinas_df()
    Li <- Li_ajustado()

    puntos$Lp <- mapply(
      calcular_Lp,
      puntos$x,
      puntos$y,
      MoreArgs = list(maquinas = maquinas, Li = Li)
    )

    puntos
  })


  # Campo destinado exclusivamente a la representación gráfica. Se suaviza en
  # energía para evitar el pixelado y los saltos visuales en los límites de
  # sombra, pero el objeto puntos_df() permanece intacto para los cálculos.
  puntos_plot_df <- reactive({
    p <- puntos_df()
    validate(need(!is.null(p) && nrow(p) > 0, "No se ha podido generar la malla de cálculo."))
    validate(need(any(is.finite(p$Lp)), "No hay niveles acústicos calculables. Revise las fuentes sonoras."))
    if (!isTRUE(input$suavizado_visual)) {
      p$Lp_plot <- p$Lp
      return(p)
    }
    it <- valor_num(input$suavizado_iteraciones, 1)
    tryCatch(
      suavizar_campo_energia(p, iteraciones = it),
      error = function(e) {
        p$Lp_plot <- p$Lp
        p
      }
    )
  })

  # ---------------------------------------------------------------------------
  # Evaluación de trabajadores: el EPI no modifica el campo ambiental
  # ---------------------------------------------------------------------------

  trabajadores_niveles <- reactive({
    trabajadores <- trabajadores_df()
    if (is.null(trabajadores)) return(NULL)

    maquinas <- maquinas_df()
    Li <- Li_ajustado()

    trabajadores$LAeqT_ambiental <- mapply(
      calcular_Lp,
      trabajadores$x,
      trabajadores$y,
      MoreArgs = list(maquinas = maquinas, Li = Li)
    )

    trabajadores$LAeqT_protegido <- NA_real_

    if (isTRUE(input$usar_epi) && is.finite(input$snr_valor)) {
      derating <- if (is.finite(input$map_snr_derating)) input$map_snr_derating else 0
      if (derating <= input$snr_valor) {
        validos <- is.finite(trabajadores$LCeqT)
        trabajadores$LAeqT_protegido[validos] <-
          trabajadores$LCeqT[validos] - (input$snr_valor - derating)
      }
    }

    trabajadores
  })

  output$tabla_trabajadores <- renderTable({
    trabajadores <- trabajadores_niveles()
    if (is.null(trabajadores)) return(NULL)

    salida <- data.frame(
      Trabajador = trabajadores$nombre,
      X_m = round(trabajadores$x, 2),
      Y_m = round(trabajadores$y, 2),
      `LAeq,T ambiental [dB(A)]` = round(trabajadores$LAeqT_ambiental, 2),
      check.names = FALSE
    )

    if (isTRUE(input$usar_epi)) {
      salida$`LCeq,T medido [dB(C)]` <- round(trabajadores$LCeqT, 2)
      salida$`SNR [dB]` <- input$snr_valor
      salida$`Derating usuario [dB]` <- if (is.finite(input$map_snr_derating)) input$map_snr_derating else 0
      salida$`L'Aeq,T [dB(A)]` <- round(trabajadores$LAeqT_protegido, 2)
    }

    salida
  }, na = "No disponible")

  # ---------------------------------------------------------------------------
  # Escala gráfica
  # ---------------------------------------------------------------------------

  colores_viridis <- plasma(256)
  limite_inferior <- 70
  limite_superior <- 120
  corte <- 80
  valores <- seq(limite_inferior, limite_superior, length.out = 256)
  valores_rescale <- ifelse(
    valores < corte,
    rescale(valores, from = c(limite_inferior, corte), to = c(0, 0.05)),
    rescale(valores, from = c(corte, limite_superior), to = c(0.05, 1))
  )

  leyenda_colorbar <- scale_fill_gradientn(
    colours = colores_viridis,
    values = valores_rescale,
    limits = c(limite_inferior, limite_superior),
    name = "Lp [dB(A)]",
    breaks = seq(70, 120, by = 5),
    na.value = colores_viridis[1],
    oob = squish,
    guide = guide_colorbar(ticks = TRUE, barheight = 20, barwidth = 1.5)
  )


  agregar_plano_a_ggplot <- function(p, debajo = TRUE) {
    if (!isTRUE(input$usar_plano_importado)) return(p)
    pr <- plano_raw()
    if (is.null(pr)) return(p)

    if (pr$kind == "raster") {
      rr <- plano_raster_transformado()
      if (!is.null(rr)) {
        p <- p + annotation_raster(
          rr$raster,
          xmin = rr$xmin, xmax = rr$xmax,
          ymin = rr$ymin, ymax = rr$ymax,
          interpolate = TRUE
        )
      }
    }
    p
  }

  plano_overlay_df <- reactive({
    if (!isTRUE(input$usar_plano_importado)) return(NULL)
    if (is.null(input$unidades_plano) || identical(input$unidades_plano, "unknown")) return(NULL)
    pr <- plano_raw()
    if (is.null(pr) || pr$kind != "vector") return(NULL)
    df <- plano_overlay_completo()
    if (is.null(df) || !nrow(df)) return(NULL)
    if ("geom_type" %in% names(df)) {
      df <- df[!toupper(df$geom_type) %in% c("POINT", "MULTIPOINT"), , drop = FALSE]
    }
    if (!nrow(df)) return(NULL)

    # Protección de rendimiento para DWG muy detallados. Se conserva cada
    # grupo y solo se reduce la densidad de vértices cuando el conjunto es
    # enorme; no afecta a la geometría usada en el cálculo.
    max_vertices <- 25000L
    if (nrow(df) > max_vertices) {
      sp <- split(df, df$group)
      total <- sum(vapply(sp, nrow, integer(1)))
      factor_red <- max(1L, ceiling(total / max_vertices))
      sp <- lapply(sp, function(g) {
        if (nrow(g) <= 3L || factor_red <= 1L) return(g)
        idx <- unique(c(1L, seq.int(1L, nrow(g), by = factor_red), nrow(g)))
        g[idx, , drop = FALSE]
      })
      df <- do.call(rbind, sp)
      rownames(df) <- NULL
    }
    df
  })

  agregar_vector_plan_a_ggplot <- function(p) {
    if (!isTRUE(input$usar_plano_importado)) return(p)
    if (is.null(input$unidades_plano) || identical(input$unidades_plano, "unknown")) return(p)
    pr <- plano_raw()
    if (is.null(pr) || pr$kind != "vector") return(p)
    df <- plano_overlay_df()
    if (is.null(df) || !nrow(df)) return(p)

    p + geom_path(
      data = df,
      aes(x = x, y = y, group = group),
      inherit.aes = FALSE,
      color = "white",
      linewidth = 0.55,
      alpha = 0.96
    )
  }

  limites_mapas_completos <- reactive({
    # Extensión conjunta: plano completo útil + malla acústica. Así el overlay
    # nunca queda cortado aunque el dominio tenga forma de L o varias alas.
    xs <- ys <- numeric()
    ov <- plano_overlay_completo()
    if (!is.null(ov) && nrow(ov)) {
      xs <- c(xs, ov$x); ys <- c(ys, ov$y)
    }
    pp <- tryCatch(puntos_plot_df(), error = function(e) NULL)
    if (!is.null(pp) && nrow(pp)) {
      xs <- c(xs, pp$x); ys <- c(ys, pp$y)
    }
    xs <- xs[is.finite(xs)]; ys <- ys[is.finite(ys)]
    if (!length(xs) || !length(ys)) return(NULL)
    xr <- range(xs); yr <- range(ys)
    mx <- max(diff(xr) * 0.02, 0.05)
    my <- max(diff(yr) * 0.02, 0.05)
    c(xmin = xr[1] - mx, xmax = xr[2] + mx,
      ymin = yr[1] - my, ymax = yr[2] + my)
  })

  # ---------------------------------------------------------------------------
  # Mapa 2D
  # ---------------------------------------------------------------------------

  output$mapa_2d <- renderPlot({
    puntos <- puntos_plot_df()
    maquinas <- maquinas_df()
    validate(need(nrow(maquinas) > 0, "No hay fuentes sonoras definidas."))
    validate(need(nrow(puntos) > 0 && any(is.finite(puntos$Lp_plot)), "No hay campo acústico calculable."))
    trabajadores <- trabajadores_niveles()
    puerta <- puerta_df()
    oficinas <- oficinas_df()

    maquinas$Lp_val <- mapply(
      calcular_Lp,
      maquinas$x,
      maquinas$y,
      MoreArgs = list(maquinas = maquinas, Li = Li_ajustado())
    )
    maquinas$color_texto <- ifelse(maquinas$Lp_val >= 90, "black", "white")

    if (!is.null(trabajadores)) {
      trabajadores$color_texto <- ifelse(trabajadores$LAeqT_ambiental >= 90, "black", "white")
    }

    p <- ggplot()
    p <- agregar_plano_a_ggplot(p)
    p <- p +
      geom_raster(data = puntos, aes(x = x, y = y, fill = Lp_plot), alpha = if (isTRUE(input$usar_plano_importado) && !is.null(plano_raw()) && plano_raw()$kind == "raster") 0.72 else 1) +
      geom_point(data = maquinas, aes(x = x, y = y), color = "white", size = 3) +
      geom_text(
        data = maquinas,
        aes(x = x, y = y, label = nombre, color = color_texto),
        vjust = -1,
        fontface = "bold",
        size = 3.5,
        show.legend = FALSE
      )

    if (!is.null(trabajadores)) {
      p <- p +
        geom_text(data = trabajadores, aes(x = x, y = y), label = "👷", size = 6) +
        geom_text(
          data = trabajadores,
          aes(x = x, y = y, label = nombre, color = color_texto),
          vjust = 2,
          fontface = "bold",
          size = 3.5,
          show.legend = FALSE
        )
    }

    if (!is.null(puerta)) {
      p <- p +
        annotate(
          "rect",
          xmin = puerta$x - 0.5,
          xmax = puerta$x + 0.5,
          ymin = puerta$y - 0.5,
          ymax = puerta$y + 0.5,
          fill = "darkgreen",
          alpha = 0.6
        ) +
        annotate("text", x = puerta$x, y = puerta$y + 1, label = "Puerta", color = "darkgreen")
    }

    if (!is.null(oficinas)) {
      p <- p +
        annotate(
          "rect",
          xmin = oficinas$xmin,
          xmax = oficinas$xmax,
          ymin = oficinas$ymin,
          ymax = oficinas$ymax,
          fill = "grey40",
          alpha = 0.3
        ) +
        annotate(
          "text",
          x = (oficinas$xmin + oficinas$xmax) / 2,
          y = (oficinas$ymin + oficinas$ymax) / 2,
          label = "Oficinas",
          color = "black"
        )
    }

    p <- agregar_vector_plan_a_ggplot(p)

    coords <- obtener_contorno()
    if (!is.null(coords)) {
      p <- p + geom_path(
        data = coords, aes(x = x, y = y),
        color = if (isTRUE(input$usar_plano_importado)) "white" else "black",
        linewidth = if (isTRUE(input$usar_plano_importado)) 0.75 else 1
      )
    } else if (!isTRUE(input$usar_plano_importado)) {
      L <- max(0.1, input_or(input$largo, 100))
      A <- max(0.1, input_or(input$ancho, 50))
      borde_manual <- data.frame(x = c(0, L, L, 0, 0), y = c(0, 0, A, A, 0))
      p <- p + geom_path(data = borde_manual, aes(x = x, y = y),
                         inherit.aes = FALSE, color = "black", linewidth = 0.8)
    }

    lims <- limites_mapas_completos()
    p <- p + leyenda_colorbar
    if (!is.null(lims)) {
      p <- p + coord_equal(xlim = c(lims["xmin"], lims["xmax"]),
                           ylim = c(lims["ymin"], lims["ymax"]),
                           expand = FALSE)
    } else {
      p <- p + coord_equal()
    }
    p +
      theme_minimal() +
      scale_color_identity() +
      theme(panel.background = element_rect(fill = "white"))
  })

  output$tabla_distancias <- renderTable({
    maquinas <- maquinas_df()
    req(nrow(maquinas) > 0)
    objetivo <- input$nivel_objetivo_distancia
    req(is.finite(objetivo))

    # Se emplea el nivel a 1 m ya corregido por cerramiento, si procede.
    # La distancia sigue siendo una estimación de campo libre y no incorpora
    # reverberación, barreras ni la suma energética de otras fuentes.
    nivel_1m <- Li_ajustado()
    distancia <- distance_for_level_free_field(nivel_1m, objetivo)

    data.frame(
      Maquina = maquinas$nombre,
      `Nivel efectivo a 1 m [dB(A)]` = round(nivel_1m, 1),
      `Nivel objetivo [dB(A)]` = round(objetivo, 1),
      `Distancia teórica campo libre [m]` = round(distancia, 2),
      Nota = "Estimación de campo libre; no incorpora reflexiones, reverberación, barreras ni suma con otras fuentes. El mapa sí puede incorporar barreras CAD.",
      check.names = FALSE
    )
  }, striped = TRUE, bordered = TRUE)


  # ---------------------------------------------------------------------------
  # Mapa de isófonas
  # ---------------------------------------------------------------------------

  output$mapa_isofonas <- renderPlot({
    puntos <- puntos_plot_df()
    maquinas <- maquinas_df()
    validate(need(nrow(maquinas) > 0, "No hay fuentes sonoras definidas."))
    validate(need(nrow(puntos) > 0 && any(is.finite(puntos$Lp_plot)), "No hay campo acústico calculable."))
    trabajadores <- trabajadores_niveles()
    puerta <- puerta_df()
    oficinas <- oficinas_df()

    maquinas$Lp_val <- mapply(
      calcular_Lp,
      maquinas$x,
      maquinas$y,
      MoreArgs = list(maquinas = maquinas, Li = Li_ajustado())
    )
    maquinas$color_texto <- ifelse(maquinas$Lp_val >= 90, "black", "white")

    if (!is.null(trabajadores)) {
      trabajadores$color_texto <- ifelse(trabajadores$LAeqT_ambiental >= 90, "black", "white")
    }

    p <- ggplot()
    p <- agregar_plano_a_ggplot(p)
    p <- p +
      geom_raster(data = puntos, aes(x = x, y = y, fill = Lp_plot), alpha = if (isTRUE(input$usar_plano_importado) && !is.null(plano_raw()) && plano_raw()$kind == "raster") 0.72 else 1) +
      geom_contour(data = puntos, aes(x = x, y = y, z = Lp_plot), color = "grey10", linewidth = 0.35, breaks = c(75, 80, 85, 90, 95, 100, 105, 110)) +
      geom_point(data = maquinas, aes(x = x, y = y), color = "white", size = 2) +
      geom_text(
        data = maquinas,
        aes(x = x, y = y, label = nombre, color = color_texto),
        vjust = -1,
        fontface = "bold",
        size = 3.5,
        show.legend = FALSE
      )

    if (!is.null(trabajadores)) {
      p <- p +
        geom_text(data = trabajadores, aes(x = x, y = y), label = "👷", size = 6) +
        geom_text(
          data = trabajadores,
          aes(x = x, y = y, label = nombre, color = color_texto),
          vjust = 2,
          fontface = "bold",
          size = 3.5,
          show.legend = FALSE
        )
    }

    if (!is.null(puerta)) {
      p <- p +
        annotate(
          "rect",
          xmin = puerta$x - 0.5,
          xmax = puerta$x + 0.5,
          ymin = puerta$y - 0.5,
          ymax = puerta$y + 0.5,
          fill = "darkgreen",
          alpha = 0.6
        ) +
        annotate(
          "text",
          x = puerta$x,
          y = puerta$y + 1,
          label = "Puerta",
          color = "darkgreen",
          fontface = "bold"
        )
    }

    if (!is.null(oficinas)) {
      p <- p +
        annotate(
          "rect",
          xmin = oficinas$xmin,
          xmax = oficinas$xmax,
          ymin = oficinas$ymin,
          ymax = oficinas$ymax,
          fill = "grey40",
          alpha = 0.3
        ) +
        annotate(
          "text",
          x = (oficinas$xmin + oficinas$xmax) / 2,
          y = (oficinas$ymin + oficinas$ymax) / 2,
          label = "Oficinas",
          color = "black",
          fontface = "bold"
        )
    }

    p <- agregar_vector_plan_a_ggplot(p)

    coords <- obtener_contorno()
    if (!is.null(coords)) {
      p <- p + geom_path(
        data = coords, aes(x = x, y = y),
        color = if (isTRUE(input$usar_plano_importado)) "white" else "black",
        linewidth = if (isTRUE(input$usar_plano_importado)) 0.75 else 1
      )
    } else if (!isTRUE(input$usar_plano_importado)) {
      L <- max(0.1, input_or(input$largo, 100))
      A <- max(0.1, input_or(input$ancho, 50))
      borde_manual <- data.frame(x = c(0, L, L, 0, 0), y = c(0, 0, A, A, 0))
      p <- p + geom_path(data = borde_manual, aes(x = x, y = y),
                         inherit.aes = FALSE, color = "black", linewidth = 0.8)
    }

    lims <- limites_mapas_completos()
    p <- p + leyenda_colorbar
    if (!is.null(lims)) {
      p <- p + coord_equal(xlim = c(lims["xmin"], lims["xmax"]),
                           ylim = c(lims["ymin"], lims["ymax"]),
                           expand = FALSE)
    } else {
      p <- p + coord_equal()
    }
    p +
      theme_minimal() +
      scale_color_identity() +
      theme(panel.background = element_rect(fill = "white"))
  })

  # ---------------------------------------------------------------------------
  # Mapa 3D
  # ---------------------------------------------------------------------------

  output$mapa_3d <- renderPlotly({
    puntos <- puntos_plot_df()
    maquinas <- maquinas_df()
    validate(need(nrow(maquinas) > 0, "No hay fuentes sonoras definidas."))
    validate(need(any(is.finite(puntos$Lp_plot)), "No hay campo acústico calculable."))
    validate(need(nrow(puntos) >= 4, "No hay suficientes puntos para construir la superficie 3D."))
    validate(need(length(unique(puntos$x)) >= 2 && length(unique(puntos$y)) >= 2,
                  "La malla necesita al menos dos coordenadas X e Y distintas para el mapa 3D."))

    puntos$Lp_3d <- pmin(120, pmax(60, puntos$Lp_plot))
    puntos_3d <- dcast(puntos, y ~ x, value.var = "Lp_3d")
    Lp_matrix <- as.matrix(puntos_3d[, -1, drop = FALSE])
    x_vals <- as.numeric(colnames(puntos_3d)[-1])
    y_vals <- puntos_3d$y
    validate(need(nrow(Lp_matrix) >= 2 && ncol(Lp_matrix) >= 2,
                  "La matriz de niveles no tiene dimensión suficiente para una superficie 3D."))

    colorescale_plotly <- lapply(
      seq_len(256),
      function(i) list(valores_rescale[i], colores_viridis[i])
    )

    fig <- plot_ly(
      x = x_vals,
      y = y_vals,
      z = Lp_matrix,
      type = "surface",
      colorscale = colorescale_plotly,
      cmin = 70,
      cmax = 120,
      showscale = TRUE,
      contours = list(z = list(show = TRUE, usecolormap = TRUE, project = list(z = TRUE))),
      lighting = list(ambient = 0.65, diffuse = 0.75, specular = 0.15, roughness = 0.85)
    )

    # Superpone el plano CAD en blanco sobre la propia superficie acústica.
    # Las líneas siguen la altura del mapa (+0,8 dB solo para que no queden
    # ocultas por la superficie); no intervienen en ningún cálculo.
    if (isTRUE(input$usar_plano_importado) &&
        !is.null(input$unidades_plano) && !identical(input$unidades_plano, "unknown")) {
      pr <- plano_raw()
      if (!is.null(pr) && identical(pr$kind, "vector")) {
        plan3d <- plano_overlay_df()
        if (!is.null(plan3d) && nrow(plan3d)) {
          plan3d <- plan3d[!toupper(plan3d$geom_type) %in% c("POINT", "MULTIPOINT"), , drop = FALSE]
          if (nrow(plan3d)) {
            # Se limita la representación a la extensión de la superficie 3D.
            grupos_en_malla <- unique(plan3d$group[
              plan3d$x >= min(x_vals) & plan3d$x <= max(x_vals) &
              plan3d$y >= min(y_vals) & plan3d$y <= max(y_vals)
            ])
            plan3d <- plan3d[plan3d$group %in% grupos_en_malla, , drop = FALSE]

            if (nrow(plan3d)) {
              # Plotly/WebGL puede fallar con DWG extremadamente detallados.
              # El cálculo acústico conserva toda la geometría; solo la copia
              # visual 3D se aligera si supera este umbral.
              max_3d_vertices <- 8000L
              if (nrow(plan3d) > max_3d_vertices) {
                sp3 <- split(plan3d, plan3d$group)
                fac3 <- max(1L, ceiling(nrow(plan3d) / max_3d_vertices))
                sp3 <- lapply(sp3, function(g) {
                  if (nrow(g) <= 3L || fac3 <= 1L) return(g)
                  idx <- unique(c(1L, seq.int(1L, nrow(g), by = fac3), nrow(g)))
                  g[idx, , drop = FALSE]
                })
                plan3d <- do.call(rbind, sp3)
                rownames(plan3d) <- NULL
              }

              # Nivel de la superficie más próxima a cada vértice CAD.
              ix <- vapply(plan3d$x, function(v) which.min(abs(x_vals - v)), integer(1))
              iy <- vapply(plan3d$y, function(v) which.min(abs(y_vals - v)), integer(1))
              zz <- mapply(function(r, c) Lp_matrix[r, c], iy, ix)
              zz[!is.finite(zz)] <- 60
              plan3d$z <- pmin(120.8, pmax(60.8, zz + 0.8))

              # Una única traza con NA entre grupos es mucho más ligera que
              # crear cientos de trazas Plotly independientes.
              grupos3d <- split(plan3d, plan3d$group)
              linea3d <- do.call(rbind, lapply(grupos3d, function(g) {
                rbind(
                  g[, c("x", "y", "z"), drop = FALSE],
                  data.frame(x = NA_real_, y = NA_real_, z = NA_real_)
                )
              }))

              fig <- fig %>% add_trace(
                data = linea3d,
                x = ~x, y = ~y, z = ~z,
                type = "scatter3d", mode = "lines",
                line = list(color = "white", width = 3),
                hoverinfo = "skip",
                showlegend = FALSE,
                inherit = FALSE
              )
            }
          }
        }
      }
    }

    fig %>%
      layout(
        scene = list(
          xaxis = list(title = "X (m)"),
          yaxis = list(title = "Y (m)"),
          zaxis = list(title = "Lp [dB(A)]", range = c(60, 121)),
          aspectmode = "manual",
          aspectratio = list(x = 1.6, y = 1, z = 0.7)
        )
      )
  })

  # Los controles dinámicos viven en subpestañas. Deben existir aunque la
  # pestaña no se haya visitado todavía, porque los mapas pueden abrirse
  # directamente desde la barra principal.
  for (id in c("maquinas_ui", "trabajadores_ui", "ui_cerramientos",
               "plan_vector_ui", "plan_raster_ui")) {
    outputOptions(output, id, suspendWhenHidden = FALSE)
  }
}

shinyApp(ui = ui, server = server)
