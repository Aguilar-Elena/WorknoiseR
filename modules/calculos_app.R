library(shiny)
library(rhandsontable)
library(ggplot2)

options(shiny.maxRequestSize = 50 * 1024^2)

# -----------------------------------------------------------------------------
# Constantes y funciones auxiliares
# -----------------------------------------------------------------------------

bandas_octava <- c("31.5", "63", "125", "250", "500", "1000", "2000", "4000", "8000")
ponderacion_A <- c(-39.4, -26.2, -16.1, -8.6, -3.2, 0.0, 1.2, 1.0, -1.1)
ponderacion_C <- c(-3.0, -0.8, -0.2, 0.0, 0.0, 0.0, -0.2, -0.8, -3.0)

# Bandas habituales para el método por bandas de octava del protector (APV).
# Se mantienen separadas del espectro ambiental: la banda de 31,5 Hz sí se
# utiliza para calcular LAeq,T y LCeq,T, pero no se exige al protector si el
# fabricante no declara mf/sf para esa frecuencia.
bandas_apv <- c("63", "125", "250", "500", "1000", "2000", "4000", "8000")

db_sum <- function(x) db_add(x)

calc_laeq_d <- function(laeq_t, tiempo_h) laeq_d(laeq_t, tiempo_h)

calc_laeq_d_multi <- function(levels, durations_h) laeq_d_multi(levels, durations_h)

calc_weekly_exposure <- function(laeq_d) laeq_weekly(laeq_d)

apply_frequency_weighting <- function(levels, frequencies = bandas_octava,
                                      weighting = c("A", "C", "Z")) {
  frequency_weighting(levels, frequencies, weighting)
}

criterion_params <- function(standard = c("NIOSH", "OSHA", "CUSTOM"),
                             criterion_level = NULL, exchange_rate = NULL,
                             reference_time_h = 8, threshold = NULL) {
  standard <- match.arg(standard)
  if (standard == "NIOSH") {
    return(list(name = "NIOSH REL", criterion_level = 85, exchange_rate = 3,
                reference_time_h = 8, threshold = 80))
  }
  if (standard == "OSHA") {
    return(list(name = "OSHA PEL", criterion_level = 90, exchange_rate = 5,
                reference_time_h = 8, threshold = 90))
  }
  if (any(!is.finite(c(criterion_level, exchange_rate, reference_time_h))) ||
      exchange_rate <= 0 || reference_time_h <= 0) return(NULL)
  list(name = "Personalizado", criterion_level = criterion_level,
       exchange_rate = exchange_rate, reference_time_h = reference_time_h,
       threshold = if (is.null(threshold) || !is.finite(threshold)) -Inf else threshold)
}

max_exposure_time <- function(level, criterion_level = 85, exchange_rate = 3,
                              reference_time_h = 8) {
  if (any(!is.finite(c(level, criterion_level, exchange_rate, reference_time_h))) ||
      exchange_rate <= 0 || reference_time_h <= 0) return(NA_real_)
  reference_time_h * 2^((criterion_level - level) / exchange_rate)
}

noise_dose <- function(levels, durations_h, criterion_level = 85, exchange_rate = 3,
                       reference_time_h = 8, threshold = -Inf) {
  if (length(levels) == 0 || length(levels) != length(durations_h) ||
      any(!is.finite(levels)) || any(!is.finite(durations_h)) ||
      any(durations_h < 0) || length(threshold) != 1 || is.na(threshold)) return(NA_real_)

  use <- levels >= threshold & durations_h > 0
  if (!any(use)) return(0)

  allowed <- max_exposure_time(levels[use], criterion_level, exchange_rate, reference_time_h)
  100 * sum(durations_h[use] / allowed)
}

evaluate_rd286 <- function(laeq_d, lcpeak = NA_real_, protected_laeq_d = NA_real_,
                           protector_used = FALSE) {
  if (!is.finite(laeq_d)) return(NULL)

  exposure_band <- if (laeq_d >= 85) {
    "Valor superior de acción alcanzado/superado (>= 85 dB(A))"
  } else if (laeq_d >= 80) {
    "Valor inferior de acción alcanzado/superado (>= 80 dB(A))"
  } else {
    "Por debajo del valor inferior de acción (< 80 dB(A))"
  }

  peak_band <- if (!is.finite(lcpeak)) {
    "No evaluado"
  } else if (lcpeak >= 137) {
    "Valor superior de acción alcanzado/superado (>= 137 dB(C))"
  } else if (lcpeak >= 135) {
    "Valor inferior de acción alcanzado/superado (>= 135 dB(C))"
  } else {
    "Por debajo del valor inferior de acción (< 135 dB(C))"
  }

  effective_daily <- if (protector_used && is.finite(protected_laeq_d)) protected_laeq_d else laeq_d
  limit_daily <- if (effective_daily > 87) "Supera 87 dB(A)" else "No supera 87 dB(A)"

  limit_peak <- if (!is.finite(lcpeak)) {
    "No evaluado"
  } else if (!protector_used) {
    if (lcpeak > 140) "Supera 140 dB(C)" else "No supera 140 dB(C)"
  } else if (lcpeak <= 140) {
    "No supera 140 dB(C) incluso sin descontar el protector"
  } else {
    "No evaluable con los datos actuales: se requiere una estimación válida de la exposición real al pico con el protector"
  }

  data.frame(
    Magnitud = c("LAeq,d ambiental", "Lpico,C ambiental", "Valor límite LAeq,d", "Valor límite Lpico,C"),
    Resultado = c(
      sprintf("%.2f dB(A) — %s", laeq_d, exposure_band),
      if (is.finite(lcpeak)) sprintf("%.2f dB(C) — %s", lcpeak, peak_band) else peak_band,
      sprintf("Exposición real usada: %.2f dB(A) — %s", effective_daily, limit_daily),
      limit_peak
    ),
    stringsAsFactors = FALSE
  )
}

calc_hml <- function(laeq_t, lceq_t, H, M, L) hml_protection(laeq_t, lceq_t, H, M, L)

calc_snr <- function(lceq_t, snr, derating = 0) snr_protection(lceq_t, snr, derating)

df_octavas <- data.frame(
  Frecuencia_Hz = bandas_octava,
  Lp_dB = as.numeric(rep(NA, length(bandas_octava))),
  stringsAsFactors = FALSE
)

df_protector_apv <- data.frame(
  Frecuencia_Hz = bandas_apv,
  mf = as.numeric(rep(NA, length(bandas_apv))),
  sf = as.numeric(rep(NA, length(bandas_apv))),
  stringsAsFactors = FALSE
)

df_tramos <- data.frame(
  Tarea = c("Tarea 1", "Tarea 2", "Tarea 3"),
  LAeq_T_dBA = as.numeric(c(NA, NA, NA)),
  Duracion_h = as.numeric(c(NA, NA, NA)),
  stringsAsFactors = FALSE
)


# -----------------------------------------------------------------------------
# Importación masiva de campañas (Excel/CSV)
# -----------------------------------------------------------------------------

normalizar_nombre_col <- function(x) {
  x <- iconv(as.character(x), from = "", to = "ASCII//TRANSLIT")
  x <- tolower(x)
  gsub("[^a-z0-9]", "", x)
}

numero_flexible <- function(x) {
  # Normaliza columnas atómicas, data.frames de una columna y list-columns.
  if (is.data.frame(x)) {
    if (ncol(x) == 0) return(numeric())
    x <- x[[1]]
  }
  if (is.list(x)) {
    x <- vapply(x, function(z) {
      if (is.null(z) || length(z) == 0) return(NA_character_)
      as.character(z[[1]])
    }, character(1))
  }
  if (is.numeric(x)) return(as.numeric(x))
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "N/A", "-", "--")] <- NA_character_
  # Admite coma decimal cuando no hay punto decimal.
  x <- ifelse(grepl(",", x) & !grepl("\\.", x), gsub(",", ".", x), x)
  suppressWarnings(as.numeric(x))
}

auto_columna <- function(df, aliases) {
  if (is.null(df) || ncol(df) == 0) return("")
  nms <- names(df)
  norm <- normalizar_nombre_col(nms)
  aliases_norm <- normalizar_nombre_col(aliases)
  idx <- match(aliases_norm, norm, nomatch = 0)
  idx <- idx[idx > 0]
  if (length(idx)) nms[idx[1]] else ""
}

valor_columna <- function(df, col, default = NA) {
  if (is.list(col)) col <- unlist(col, recursive = TRUE, use.names = FALSE)
  if (length(col) > 1) col <- col[1]
  col <- if (length(col)) as.character(col) else ""
  if (is.null(col) || length(col) == 0 || is.na(col) || !nzchar(col) || !(col %in% names(df))) {
    return(rep(default, nrow(df)))
  }
  v <- df[[col]]
  if (is.data.frame(v) && ncol(v)) v <- v[[1]]
  if (is.list(v)) {
    v <- vapply(v, function(z) {
      if (is.null(z) || length(z) == 0) return(NA_character_)
      as.character(z[[1]])
    }, character(1))
  }
  v
}

leer_csv_flexible <- function(path) {
  intentos <- list(
    function() read.csv2(path, check.names = FALSE, stringsAsFactors = FALSE),
    function() read.csv(path, check.names = FALSE, stringsAsFactors = FALSE),
    function() read.delim(path, check.names = FALSE, stringsAsFactors = FALSE)
  )
  for (f in intentos) {
    x <- tryCatch(f(), error = function(e) NULL)
    if (!is.null(x) && ncol(x) > 1) return(x)
  }
  NULL
}

leer_archivo_campania <- function(path, filename) {
  ext <- tolower(tools::file_ext(filename))
  if (ext %in% c("xlsx", "xls")) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("Para importar Excel es necesario instalar el paquete 'readxl'.")
    }
    hojas <- readxl::excel_sheets(path)
    out <- lapply(hojas, function(h) {
      as.data.frame(readxl::read_excel(path, sheet = h, .name_repair = "unique"), stringsAsFactors = FALSE)
    })
    names(out) <- hojas
    return(out)
  }
  if (ext %in% c("csv", "txt", "tsv")) {
    x <- leer_csv_flexible(path)
    if (is.null(x)) stop("No se pudo interpretar el archivo delimitado.")
    return(list(EXPOSICIONES = x))
  }
  stop("Formato no admitido. Use .xlsx, .xls, .csv, .txt o .tsv.")
}

buscar_hoja <- function(hojas, aliases) {
  if (is.null(hojas) || !length(hojas)) return(NULL)
  nms <- names(hojas)
  nn <- normalizar_nombre_col(nms)
  aa <- normalizar_nombre_col(aliases)
  i <- match(aa, nn, nomatch = 0)
  i <- i[i > 0]
  if (length(i)) hojas[[i[1]]] else NULL
}

# Detecta columnas de bandas de octava. Se exige el espectro completo 31,5-8000 Hz
# para calcular automáticamente niveles globales A/C, evitando completar bandas ausentes.
detectar_columnas_bandas <- function(df) {
  if (is.null(df)) return(setNames(rep("", length(bandas_octava)), bandas_octava))
  out <- setNames(rep("", length(bandas_octava)), bandas_octava)
  for (f in bandas_octava) {
    token <- normalizar_nombre_col(f)
    aliases <- c(
      f, paste0("lp", f), paste0("hz", f), paste0("lp_", f),
      paste0("banda", f), paste0("freq", f), paste0("frecuencia", f)
    )
    # 31,5 puede aparecer como 31_5, 31.5 o 315 tras normalización.
    candidatos <- normalizar_nombre_col(aliases)
    nms_norm <- normalizar_nombre_col(names(df))
    idx <- match(candidatos, nms_norm, nomatch = 0)
    idx <- idx[idx > 0]
    if (length(idx)) out[f] <- names(df)[idx[1]]
  }
  out
}

calcular_global_desde_bandas <- function(df, weighting = c("A", "C")) {
  weighting <- match.arg(weighting)
  cols <- detectar_columnas_bandas(df)
  if (any(!nzchar(cols))) return(rep(NA_real_, nrow(df)))
  mat <- sapply(cols, function(cl) numero_flexible(df[[cl]]))
  if (is.null(dim(mat))) mat <- matrix(mat, ncol = length(cols))
  apply(mat, 1, function(v) {
    if (any(!is.finite(v))) return(NA_real_)
    weighted <- apply_frequency_weighting(v, bandas_octava, weighting)
    db_sum(weighted)
  })
}

clasificar_rd286_resumen <- function(laeq_d, lcpeak = NA_real_) {
  if (!is.finite(laeq_d)) return("No evaluable")
  nivel <- if (laeq_d > 87) {
    "LAeq,d > 87 dB(A)"
  } else if (laeq_d >= 85) {
    "Valor superior de acción"
  } else if (laeq_d >= 80) {
    "Valor inferior de acción"
  } else {
    "Por debajo del valor inferior de acción"
  }
  if (is.finite(lcpeak)) {
    pico <- if (lcpeak > 140) "Lpico,C > 140 dB(C)" else if (lcpeak >= 137) "Pico: valor superior de acción" else if (lcpeak >= 135) "Pico: valor inferior de acción" else NULL
    if (!is.null(pico)) nivel <- paste(nivel, pico, sep = " | ")
  }
  nivel
}

resumir_diario_campania <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  ids <- unique(df$trabajador_id[!is.na(df$trabajador_id) & nzchar(df$trabajador_id)])
  if (!length(ids)) return(NULL)

  dd <- df$dia
  dd[is.na(dd) | !nzchar(trimws(dd))] <- "Jornada importada"
  keys <- unique(paste(df$trabajador_id, dd, sep = "||"))
  keys <- keys[!grepl("^\\|\\|", keys)]

  filas <- lapply(keys, function(k) {
    parts <- strsplit(k, "\\|\\|", fixed = FALSE)[[1]]
    id <- parts[1]
    day <- paste(parts[-1], collapse = "||")
    d <- df[df$trabajador_id == id & dd == day, , drop = FALSE]
    valid <- is.finite(d$LAeq_T_dBA) & is.finite(d$Duracion_h) & d$Duracion_h > 0
    laeq_d_calc <- if (any(valid)) calc_laeq_d_multi(d$LAeq_T_dBA[valid], d$Duracion_h[valid]) else NA_real_
    laeq_d_imp_vals <- unique(d$LAeq_d_importado_dBA[is.finite(d$LAeq_d_importado_dBA)])
    laeq_d_imp <- if (length(laeq_d_imp_vals)) laeq_d_imp_vals[1] else NA_real_
    laeq_d <- if (is.finite(laeq_d_imp)) laeq_d_imp else laeq_d_calc
    origen_laeq_d <- if (is.finite(laeq_d_imp)) "importado" else if (is.finite(laeq_d_calc)) "calculado_tramos" else "no_calculable"
    diferencia_laeq_d <- if (is.finite(laeq_d_imp) && is.finite(laeq_d_calc)) laeq_d_calc - laeq_d_imp else NA_real_
    peak <- if (any(is.finite(d$LCpico_dBC))) max(d$LCpico_dBC[is.finite(d$LCpico_dBC)]) else NA_real_
    total_h <- if (any(is.finite(d$Duracion_h))) sum(d$Duracion_h[is.finite(d$Duracion_h)], na.rm = TRUE) else NA_real_
    dose_n <- if (any(valid)) noise_dose(d$LAeq_T_dBA[valid], d$Duracion_h[valid], 85, 3, 8, 80) else NA_real_
    dose_o <- if (any(valid)) noise_dose(d$LAeq_T_dBA[valid], d$Duracion_h[valid], 90, 5, 8, 90) else NA_real_

    prot_valid <- valid & is.finite(d$LAeq_T_protegido_dBA)
    laeq_d_prot <- if (any(valid) && sum(prot_valid) == sum(valid)) {
      calc_laeq_d_multi(d$LAeq_T_protegido_dBA[prot_valid], d$Duracion_h[prot_valid])
    } else NA_real_

    data.frame(
      trabajador_id = id,
      dia = day,
      registros = nrow(d),
      tiempo_total_h = total_h,
      LAeq_d_dBA = laeq_d,
      origen_LAeq_d = origen_laeq_d,
      LAeq_d_calculado_tramos_dBA = laeq_d_calc,
      LAeq_d_importado_dBA = laeq_d_imp,
      diferencia_calc_menos_importado_dB = diferencia_laeq_d,
      LCpico_max_dBC = peak,
      dosis_NIOSH_pct = dose_n,
      dosis_OSHA_pct = dose_o,
      LAeq_d_protegido_dBA = laeq_d_prot,
      clasificacion_RD286 = clasificar_rd286_resumen(laeq_d, peak),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, filas)
}

resumir_campania <- function(df, diario = NULL) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  ids <- unique(df$trabajador_id[!is.na(df$trabajador_id) & nzchar(df$trabajador_id)])
  if (!length(ids)) return(NULL)
  if (is.null(diario)) diario <- resumir_diario_campania(df)

  primero <- function(x) {
    x <- unique(x[!is.na(x) & nzchar(trimws(as.character(x)))])
    if (length(x)) as.character(x[1]) else ""
  }

  filas <- lapply(ids, function(id) {
    d <- df[df$trabajador_id == id, , drop = FALSE]
    dr <- if (!is.null(diario)) diario[diario$trabajador_id == id, , drop = FALSE] else NULL
    daily_levels <- if (!is.null(dr)) dr$LAeq_d_dBA[is.finite(dr$LAeq_d_dBA)] else numeric()
    peak <- if (any(is.finite(d$LCpico_dBC))) max(d$LCpico_dBC[is.finite(d$LCpico_dBC)]) else NA_real_
    total_h <- if (any(is.finite(d$Duracion_h))) sum(d$Duracion_h[is.finite(d$Duracion_h)], na.rm = TRUE) else NA_real_

    semanal <- if (length(daily_levels)) calc_weekly_exposure(daily_levels) else list(normative_5d = NA_real_, actual_days_mean = NA_real_, exposed_days = 0)
    max_daily <- if (length(daily_levels)) max(daily_levels) else NA_real_
    mean_daily <- if (length(daily_levels)) 10 * log10(mean(10^(daily_levels / 10))) else NA_real_
    prot_daily <- if (!is.null(dr)) dr$LAeq_d_protegido_dBA[is.finite(dr$LAeq_d_protegido_dBA)] else numeric()
    max_prot <- if (length(prot_daily)) max(prot_daily) else NA_real_
    dose_n_daily <- if (!is.null(dr)) dr$dosis_NIOSH_pct[is.finite(dr$dosis_NIOSH_pct)] else numeric()
    dose_o_daily <- if (!is.null(dr)) dr$dosis_OSHA_pct[is.finite(dr$dosis_OSHA_pct)] else numeric()
    max_dose_n <- if (length(dose_n_daily)) max(dose_n_daily) else NA_real_
    max_dose_o <- if (length(dose_o_daily)) max(dose_o_daily) else NA_real_

    valid <- is.finite(d$LAeq_T_dBA) & is.finite(d$Duracion_h) & d$Duracion_h > 0
    tma_n <- if (any(valid)) min(max_exposure_time(d$LAeq_T_dBA[valid], 85, 3, 8), na.rm = TRUE) else NA_real_

    data.frame(
      trabajador_id = id,
      trabajador = primero(d$trabajador),
      puesto = primero(d$puesto),
      zona = primero(d$zona),
      registros = nrow(d),
      dias_evaluados = length(daily_levels),
      tiempo_total_importado_h = total_h,
      LAeq_d_max_dBA = max_daily,
      media_energetica_dias_dBA = mean_daily,
      LCpico_max_dBC = peak,
      dosis_NIOSH_max_pct = max_dose_n,
      dosis_OSHA_max_pct = max_dose_o,
      TMA_min_NIOSH_h = tma_n,
      LAeq_d_protegido_max_dBA = max_prot,
      LAeq_s_normativo_5d = semanal$normative_5d,
      media_dias_expuestos_dBA = semanal$actual_days_mean,
      clasificacion_max_diaria_RD286 = clasificar_rd286_resumen(max_daily, peak),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, filas)
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}


idioma_worknoiser <- function(input = NULL) {
  lang <- if (!is.null(input) && !is.null(input$wn_language)) input$wn_language else worknoiser_language()
  if (!lang %in% c("es", "en")) lang <- "es"
  lang
}

traducir_exportacion <- function(df, lang = "es") {
  if (is.null(df) || !is.data.frame(df) || lang != "en") return(df)
  nombres <- c(
    trabajador_id = "worker_id", trabajador = "worker", puesto = "job", zona = "zone",
    tarea = "task", dia = "day", Duracion_h = "Duration_h", registros = "records",
    tiempo_total_h = "total_time_h", origen_LAeq_d = "LEX_8h_source",
    LAeq_d_calculado_tramos_dBA = "LEX_8h_calculated_periods_dBA",
    LAeq_d_importado_dBA = "LEX_8h_imported_dBA",
    diferencia_calc_menos_importado_dB = "LEX_8h_calculated_minus_imported_dB",
    dosis_NIOSH_pct = "NIOSH_dose_pct", dosis_OSHA_pct = "OSHA_dose_pct",
    LAeq_d_protegido_dBA = "protected_LEX_8h_dBA", clasificacion_RD286 = "EU_Directive_2003_10_EC_classification",
    dias_evaluados = "days_assessed", tiempo_total_importado_h = "total_imported_time_h",
    media_energetica_dias_dBA = "energetic_mean_days_dBA", dosis_NIOSH_max_pct = "max_NIOSH_dose_pct",
    dosis_OSHA_max_pct = "max_OSHA_dose_pct", LAeq_d_protegido_max_dBA = "max_protected_LEX_8h_dBA",
    media_dias_expuestos_dBA = "mean_exposed_days_dBA", clasificacion_max_diaria_RD286 = "max_daily_EU_Directive_2003_10_EC_classification",
    Indicador = "Indicator", Valor = "Value", Advertencia = "Warning"
  )
  old <- names(df)
  hit <- old %in% names(nombres)
  old[hit] <- unname(nombres[old[hit]])
  names(df) <- old
  value_map <- c(
    "importado"="imported", "calculado_tramos"="calculated_periods", "calculado_espectro"="calculated_spectrum",
    "no_calculable"="not_calculable", "no_disponible"="not_available",
    "Jornada importada"="Imported workday",
    "Valor superior de acción"="Upper action value", "Valor inferior de acción"="Lower action value",
    "Por debajo del valor inferior de acción"="Below lower action value",
    "Pico: valor superior de acción"="Peak: upper action value", "Pico: valor inferior de acción"="Peak: lower action value",
    "Registros de exposición"="Exposure records", "Trabajadores"="Workers",
    "LAeq,T importado"="Imported LAeq,T", "LAeq,T calculado desde espectro"="LAeq,T calculated from spectrum",
    "LAeq,T no calculable"="LAeq,T not calculable", "LCeq,T importado"="Imported LCeq,T",
    "LCeq,T calculado desde espectro"="LCeq,T calculated from spectrum", "LCeq,T no calculable"="LCeq,T not calculable",
    "Registros con duración válida"="Records with valid duration", "Registros con LAeq,d importado"="Records with imported LAeq,d",
    "Registros con LCpico"="Records with Lpeak,C", "Registros con SNR calculable"="Records with calculable SNR",
    "Trabajadores con LAeq,d calculable"="Workers with calculable LAeq,d",
    "No se han detectado advertencias automáticas relevantes."="No relevant automatic warnings were detected.",
    "Sin datos procesados."="No processed data."
  )
  for (j in seq_along(df)) {
    if (is.character(df[[j]])) {
      z <- df[[j]]
      idx <- match(z, names(value_map))
      ok <- !is.na(idx)
      z[ok] <- unname(value_map[idx[ok]])
      z <- gsub("Hay registros sin identificador de trabajador\\.", "Some records have no worker identifier.", z)
      z <- gsub("Hay registros sin duración válida: no podrán contribuir al LAeq,d/dosis\\.", "Some records have no valid duration and cannot contribute to LAeq,d/dose.", z)
      df[[j]] <- z
    }
  }
  df
}

crear_diagnostico_campania <- function(df, resumen = NULL) {
  if (is.null(df)) return(data.frame(Indicador = "Sin datos", Valor = ""))
  origen_laeq <- table(factor(df$origen_LAeq_T, levels = c("importado", "calculado_espectro", "no_calculable")))
  origen_lceq <- table(factor(df$origen_LCeq_T, levels = c("importado", "calculado_espectro", "no_calculable")))
  data.frame(
    Indicador = c(
      "Registros de exposición", "Trabajadores", "LAeq,T importado",
      "LAeq,T calculado desde espectro", "LAeq,T no calculable",
      "LCeq,T importado", "LCeq,T calculado desde espectro", "LCeq,T no calculable",
      "Registros con duración válida", "Registros con LAeq,d importado", "Registros con LCpico", "Registros con SNR calculable",
      "Trabajadores con LAeq,d calculable"
    ),
    Valor = c(
      nrow(df), length(unique(df$trabajador_id[nzchar(df$trabajador_id)])),
      origen_laeq["importado"], origen_laeq["calculado_espectro"], origen_laeq["no_calculable"],
      origen_lceq["importado"], origen_lceq["calculado_espectro"], origen_lceq["no_calculable"],
      sum(is.finite(df$Duracion_h) & df$Duracion_h > 0),
      sum(is.finite(df$LAeq_d_importado_dBA)),
      sum(is.finite(df$LCpico_dBC)),
      sum(is.finite(df$LAeq_T_protegido_dBA)),
      if (is.null(resumen)) 0 else sum(is.finite(resumen$LAeq_d_max_dBA))
    ),
    stringsAsFactors = FALSE
  )
}

crear_advertencias_campania <- function(df, trabajadores = NULL) {
  avisos <- character()
  if (is.null(df) || nrow(df) == 0) return(data.frame(Advertencia = "Sin datos procesados."))

  if (any(!nzchar(df$trabajador_id))) avisos <- c(avisos, "Hay registros sin identificador de trabajador.")
  if (any(!is.finite(df$Duracion_h))) avisos <- c(avisos, "Hay registros sin duración válida: no podrán contribuir al LAeq,d/dosis.")
  if (any(is.finite(df$Duracion_h) & df$Duracion_h <= 0)) avisos <- c(avisos, "Hay duraciones <= 0 h.")
  if (any(df$origen_LAeq_T == "no_calculable")) avisos <- c(avisos, "Hay registros sin LAeq,T y sin espectro completo para calcularlo.")
  if (any(is.finite(df$SNR_dB) & !is.finite(df$LCeq_T_dBC))) avisos <- c(avisos, "Hay registros con SNR pero sin LCeq,T ni espectro completo: SNR no calculable.")
  if (any(is.finite(df$LCeq_T_dBC) & is.finite(df$LAeq_T_dBA) & df$LCeq_T_dBC < df$LAeq_T_dBA - 3)) {
    avisos <- c(avisos, "Se han detectado registros con LCeq,T sensiblemente inferior a LAeq,T; conviene revisar ponderaciones/unidades.")
  }

  # Duraciones agregadas anómalas por trabajador y jornada.
  ids <- unique(df$trabajador_id[nzchar(df$trabajador_id)])
  for (id in ids) {
    d <- df[df$trabajador_id == id, , drop = FALSE]
    dias <- d$dia
    dias[is.na(dias) | !nzchar(trimws(dias))] <- "Jornada importada"
    for (dd in unique(dias)) {
      z <- d[dias == dd, , drop = FALSE]
      tt <- sum(z$Duracion_h[is.finite(z$Duracion_h)], na.rm = TRUE)
      if (tt > 24) avisos <- c(avisos, sprintf("%s (%s): la suma de duraciones supera 24 h (%.2f h).", id, dd, tt))
    }
  }

  if (!length(avisos)) avisos <- "No se han detectado advertencias automáticas relevantes."
  data.frame(Advertencia = unique(avisos), stringsAsFactors = FALSE)
}


procesar_exposiciones_importadas <- function(raw, mapping, hojas = NULL) {
  if (is.null(raw) || nrow(raw) == 0) stop("La hoja de exposiciones no contiene filas.")

  txt <- function(key, default = "") {
    v <- as.character(valor_columna(raw, mapping[[key]] %||% "", default))
    v[is.na(v)] <- ""
    v
  }
  num <- function(key) {
    numero_flexible(valor_columna(raw, mapping[[key]] %||% "", NA))
  }

  out <- data.frame(
    trabajador_id = trimws(txt("id")),
    trabajador = trimws(txt("nombre")),
    puesto = trimws(txt("puesto")),
    zona = trimws(txt("zona")),
    tarea = trimws(txt("tarea")),
    dia = trimws(txt("dia")),
    Duracion_h = num("duracion"),
    Lp_dB = num("lp"),
    LpA_dBA = num("lpa"),
    LAeq_T_dBA = num("laeq"),
    LAeq_d_importado_dBA = num("laeqd"),
    LCeq_T_dBC = num("lceq"),
    LCpico_dBC = num("lcpeak"),
    x = num("x"),
    y = num("y"),
    protector_id = trimws(txt("protector_id")),
    SNR_dB = num("snr"),
    derating_dB = num("derating"),
    stringsAsFactors = FALSE
  )

  out$derating_dB[!is.finite(out$derating_dB)] <- 0
  out$origen_LpA <- ifelse(is.finite(out$LpA_dBA), "importado", "no_calculable")
  out$origen_LAeq_T <- ifelse(is.finite(out$LAeq_T_dBA), "importado", "no_calculable")
  out$origen_LCeq_T <- ifelse(is.finite(out$LCeq_T_dBC), "importado", "no_calculable")
  out$origen_SNR <- ifelse(is.finite(out$SNR_dB), "importado", "no_disponible")

  # Si la hoja de exposiciones incluye un espectro completo, se calculan A/C.
  a_espectro <- calcular_global_desde_bandas(raw, "A")
  c_espectro <- calcular_global_desde_bandas(raw, "C")
  usar_lpa <- !is.finite(out$LpA_dBA) & is.finite(a_espectro)
  out$LpA_dBA[usar_lpa] <- a_espectro[usar_lpa]
  out$origen_LpA[usar_lpa] <- "calculado_espectro"
  usar_a <- !is.finite(out$LAeq_T_dBA) & is.finite(a_espectro)
  usar_c <- !is.finite(out$LCeq_T_dBC) & is.finite(c_espectro)
  out$LAeq_T_dBA[usar_a] <- a_espectro[usar_a]
  out$LCeq_T_dBC[usar_c] <- c_espectro[usar_c]
  out$origen_LAeq_T[usar_a] <- "calculado_espectro"
  out$origen_LCeq_T[usar_c] <- "calculado_espectro"

  # Hoja opcional ESPECTROS: enlaza por trabajador_id + tarea cuando sea posible.
  espectros <- buscar_hoja(hojas, c("ESPECTROS", "SPECTRA", "OCTAVAS", "BANDAS"))
  if (!is.null(espectros) && nrow(espectros) > 0) {
    id_e <- auto_columna(espectros, c("trabajador_id", "id_trabajador", "id", "codigo_trabajador", "trabajador"))
    tarea_e <- auto_columna(espectros, c("tarea", "actividad", "operacion", "operación"))
    if (nzchar(id_e)) {
      ids_e <- trimws(as.character(espectros[[id_e]]))
      tareas_e <- if (nzchar(tarea_e)) trimws(as.character(espectros[[tarea_e]])) else rep("", nrow(espectros))
      ae <- calcular_global_desde_bandas(espectros, "A")
      ce <- calcular_global_desde_bandas(espectros, "C")
      key_e <- if (nzchar(tarea_e)) paste(ids_e, tareas_e, sep = "||") else ids_e
      key_o <- if (nzchar(tarea_e)) paste(out$trabajador_id, out$tarea, sep = "||") else out$trabajador_id
      m <- match(key_o, key_e)
      aa <- ae[m]; cc <- ce[m]
      ua <- !is.finite(out$LAeq_T_dBA) & is.finite(aa)
      uc <- !is.finite(out$LCeq_T_dBC) & is.finite(cc)
      out$LAeq_T_dBA[ua] <- aa[ua]
      out$LCeq_T_dBC[uc] <- cc[uc]
      out$origen_LAeq_T[ua] <- "calculado_espectro"
      out$origen_LCeq_T[uc] <- "calculado_espectro"
    }
  }

  # Hoja opcional TRABAJADORES: completa metadatos/jornada cuando falten.
  trabajadores <- buscar_hoja(hojas, c("TRABAJADORES", "PERSONAL", "WORKERS", "EMPLEADOS"))
  if (!is.null(trabajadores) && nrow(trabajadores) > 0) {
    id_t <- auto_columna(trabajadores, c("trabajador_id", "id_trabajador", "id", "codigo_trabajador"))
    if (nzchar(id_t)) {
      m <- match(out$trabajador_id, trimws(as.character(trabajadores[[id_t]])))
      completar_txt <- function(dest, aliases) {
        cl <- auto_columna(trabajadores, aliases)
        if (!nzchar(cl)) return(dest)
        vals <- as.character(trabajadores[[cl]])[m]
        vacio <- is.na(dest) | !nzchar(trimws(dest))
        dest[vacio & !is.na(vals)] <- vals[vacio & !is.na(vals)]
        dest
      }
      out$trabajador <- completar_txt(out$trabajador, c("trabajador", "nombre", "nombre_trabajador"))
      out$puesto <- completar_txt(out$puesto, c("puesto", "puesto_trabajo", "job", "ocupacion", "ocupación"))
      out$zona <- completar_txt(out$zona, c("zona", "area", "área", "seccion", "sección"))
      jornada_col <- auto_columna(trabajadores, c("jornada_h", "horas_jornada", "jornada", "duracion_jornada"))
      out$jornada_declarada_h <- if (nzchar(jornada_col)) numero_flexible(trabajadores[[jornada_col]])[m] else NA_real_
    } else {
      out$jornada_declarada_h <- NA_real_
    }
  } else {
    out$jornada_declarada_h <- NA_real_
  }

  # Hoja opcional PROTECTORES: completa SNR por protector_id.
  protectores <- buscar_hoja(hojas, c("PROTECTORES", "EPI", "HEARINGPROTECTION", "PROTECTORS"))
  if (!is.null(protectores) && nrow(protectores) > 0 && any(nzchar(out$protector_id))) {
    id_p <- auto_columna(protectores, c("protector_id", "id_protector", "id", "codigo", "modelo"))
    snr_p <- auto_columna(protectores, c("snr", "snr_db", "valor_snr"))
    if (nzchar(id_p) && nzchar(snr_p)) {
      m <- match(out$protector_id, trimws(as.character(protectores[[id_p]])))
      snr_join <- numero_flexible(protectores[[snr_p]])[m]
      u <- !is.finite(out$SNR_dB) & is.finite(snr_join)
      out$SNR_dB[u] <- snr_join[u]
      out$origen_SNR[u] <- "hoja_protectores"
    }
  }

  # Cálculo SNR solo cuando existen LCeq,T y SNR. Nunca usa LAeq,T como sustituto.
  out$LAeq_T_protegido_dBA <- NA_real_
  ok_snr <- is.finite(out$LCeq_T_dBC) & is.finite(out$SNR_dB) &
    is.finite(out$derating_dB) & out$derating_dB >= 0 & out$derating_dB <= out$SNR_dB
  out$LAeq_T_protegido_dBA[ok_snr] <- mapply(
    calc_snr,
    out$LCeq_T_dBC[ok_snr], out$SNR_dB[ok_snr], out$derating_dB[ok_snr]
  )
  out$origen_LAeq_T_protegido <- ifelse(ok_snr, "calculado_SNR", "no_calculable")

  # Identificador mínimo: si no existe ID pero sí nombre, usa el nombre como clave sin inventar otro valor.
  sin_id <- !nzchar(out$trabajador_id) & nzchar(out$trabajador)
  out$trabajador_id[sin_id] <- out$trabajador[sin_id]

  # Blindaje final contra list-columns de importaciones flexibles.
  cols_num <- c("Duracion_h", "Lp_dB", "LpA_dBA", "LAeq_T_dBA", "LAeq_d_importado_dBA",
                "LCeq_T_dBC", "LCpico_dBC", "x", "y", "SNR_dB", "derating_dB",
                "jornada_declarada_h", "LAeq_T_protegido_dBA")
  for (nm in intersect(cols_num, names(out))) out[[nm]] <- numero_flexible(out[[nm]])
  cols_txt <- c("trabajador_id", "trabajador", "puesto", "zona", "tarea", "dia", "protector_id",
                "origen_LpA", "origen_LAeq_T", "origen_LCeq_T", "origen_SNR", "origen_LAeq_T_protegido")
  for (nm in intersect(cols_txt, names(out))) {
    v <- out[[nm]]
    if (is.list(v)) v <- vapply(v, function(z) if (is.null(z) || !length(z)) "" else as.character(z[[1]]), character(1))
    out[[nm]] <- as.character(v)
  }

  out
}

# -----------------------------------------------------------------------------
# Interfaz
# -----------------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Cálculo de niveles sonoros"),

  sidebarLayout(
    sidebarPanel(
      checkboxInput("usar_octavas", "¿Deseas introducir Lp por bandas de octava?", FALSE),

      conditionalPanel(
        condition = "input.usar_octavas == false",
        numericInput("num_fuentes", "Número de fuentes sonoras:", value = 1, min = 1),
        uiOutput("fuentes_lp")
      ),

      conditionalPanel(
        condition = "input.usar_octavas == true",
        h4("Introduce el Lp por bandas de octava (dB)"),
        rHandsontableOutput("tabla_octavas")
      ),

      numericInput(
        "tiempo",
        "Horas de exposición diaria (para LAeq,D):",
        value = NA,
        min = 0,
        max = 24
      ),

      numericInput(
        "pico",
        "Nivel de presión acústica de pico LCpico [dB(C)]:",
        value = NA
      ),

      checkboxInput(
        "usar_tramos",
        "¿Deseas calcular la jornada por tareas/tramos de exposición?",
        FALSE
      ),
      conditionalPanel(
        condition = "input.usar_tramos == true",
        helpText("Cada fila representa una tarea o tramo con su LAeq,T y duración. El LAeq,d conjunto se calcula por suma energética y normalización a 8 h."),
        rHandsontableOutput("tabla_tramos"),
        fileInput(
          "archivo_tramos",
          "Importar CSV de tramos (Tarea, LAeq_T_dBA, Duracion_h):",
          accept = ".csv"
        )
      ),

      checkboxInput("calcular_dosis", "¿Deseas calcular dosis de ruido y TMA?", FALSE),
      conditionalPanel(
        condition = "input.calcular_dosis == true",
        selectInput(
          "criterio_dosis",
          "Criterio de dosis:",
          choices = c(
            "NIOSH REL (85 dB(A), 3 dB, umbral 80 dB(A))" = "NIOSH",
            "OSHA PEL (90 dB(A), 5 dB, umbral 90 dB(A))" = "OSHA",
            "Personalizado" = "CUSTOM"
          ),
          selected = "NIOSH"
        ),
        conditionalPanel(
          condition = "input.criterio_dosis == 'CUSTOM'",
          numericInput("dosis_criterio", "Nivel criterio [dB(A)]:", 85),
          numericInput("dosis_intercambio", "Tasa de intercambio [dB]:", 3, min = 0.1),
          numericInput("dosis_tref", "Tiempo de referencia [h]:", 8, min = 0.1),
          numericInput("dosis_umbral", "Umbral de integración [dB(A)]:", 80)
        ),
        helpText("La dosis es una métrica configurable. Los presets NIOSH/OSHA no constituyen los valores de acción del RD 286/2006.")
      ),

      checkboxInput(
        "usar_proteccion",
        "¿Deseas calcular la atenuación con protectores auditivos?",
        FALSE
      ),

      conditionalPanel(
        condition = "input.usar_proteccion == true",
        radioButtons(
          "metodo_proteccion",
          "Método de cálculo:",
          choices = c("APV", "HML", "SNR")
        ),

        conditionalPanel(
          condition = "input.metodo_proteccion == 'APV'",
          h4("Introduce los valores del protector auditivo (mf y sf)"),
          helpText("El espectro ambiental incluye 31,5–8000 Hz. El método APV utiliza las bandas 63–8000 Hz declaradas habitualmente por el fabricante."),
          rHandsontableOutput("tabla_apv")
        ),

        conditionalPanel(
          condition = "input.metodo_proteccion == 'HML'",
          numericInput("H", "H (alta frecuencia) [dB]", value = NA),
          numericInput("M", "M (media frecuencia) [dB]", value = NA),
          numericInput("L", "L (baja frecuencia) [dB]", value = NA)
        ),

        conditionalPanel(
          condition = "input.metodo_proteccion == 'SNR'",
          numericInput("SNR", "Valor SNR [dB]", value = NA),
          numericInput(
            "snr_derating",
            "Corrección/derating adicional definido por el usuario [dB]:",
            value = 0,
            min = 0
          ),
          helpText("Por defecto = 0 dB. Esta corrección no forma parte del cálculo SNR básico; solo se aplica si el usuario decide introducirla.")
        ),

        conditionalPanel(
          condition = "input.metodo_proteccion == 'HML' || input.metodo_proteccion == 'SNR'",
          numericInput(
            "lceq_manual",
            "LCeq,T medido [dB(C)] (opcional si se usan bandas de octava):",
            value = NA
          ),
          helpText("LCeq,T y LCpico son magnitudes distintas. El nivel de pico no se utiliza como sustituto de LCeq,T.")
        )
      ),

      checkboxInput("calcular_s", "¿Deseas calcular exposición semanal?", FALSE),

      conditionalPanel(
        condition = "input.calcular_s == true",
        checkboxGroupInput(
          "dias_expuestos_semana",
          "Días con exposición:",
          choices = c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado", "Domingo"),
          selected = c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes")
        ),
        helpText("Puede seleccionar entre 1 y 7 días. Se muestran dos resultados: LAeq,s normativo (referencia fija de 5 jornadas) y media energética de los días realmente expuestos."),
        fileInput(
          "archivo_semana",
          "Importar CSV semanal (Dia, LAeq_d_dBA):",
          accept = ".csv"
        ),
        uiOutput("dias_semana_ui")
      ),



      tags$hr(),
      h4("Importación masiva de trabajadores/campañas"),
      fileInput(
        "archivo_campania",
        "Subir Excel/CSV de trabajadores y exposiciones:",
        accept = c(".xlsx", ".xls", ".csv", ".txt", ".tsv")
      ),
      downloadButton("descargar_plantilla_excel", "Descargar plantilla Excel"),
      helpText("La plantilla es recomendada, no obligatoria. Si usa un Excel propio podrá mapear las columnas manualmente."),
      uiOutput("selector_hoja_campania"),
      uiOutput("mapeo_columnas_campania"),
      actionButton("procesar_campania", "Procesar campaña importada"),

      actionButton("calcular", "Calcular")
    ),

    mainPanel(
      tabsetPanel(

        tabPanel(
          "Importación masiva",
          downloadButton("descargar_resultados_campania", "Descargar resultados procesados (Excel)"),
          br(), br(),
          h4("Diagnóstico de datos importados"),
          tableOutput("diagnostico_campania"),
          h4("Advertencias y datos no calculables"),
          tableOutput("advertencias_campania"),
          h4("Resultados por trabajador"),
          tableOutput("resultados_campania"),
          h4("Resultados diarios por trabajador"),
          tableOutput("resultados_diarios_campania"),
          h4("Vista previa de exposiciones normalizadas"),
          tableOutput("preview_campania")
        ),
        tabPanel("Resultados", verbatimTextOutput("resultados")),
        tabPanel("Conformidad RD 286/2006", tableOutput("tabla_conformidad")),
        tabPanel(
          "Perfil de exposición",
          helpText("El perfil muestra los LAeq,T de cada tramo y el LAeq,d acumulado. Las líneas de 80/85/87 dB(A) se aplican al LAeq,d acumulado, no al nivel aislado de cada tramo."),
          plotOutput("perfil_exposicion")
        ),
        tabPanel(
          "Bandas de octava",
          conditionalPanel(
            condition = "input.usar_octavas == true",
            h4("Gráfico de bandas de octava"),
            plotOutput("grafico_octavas")
          )
        )
      )
    )
  )
)

# -----------------------------------------------------------------------------
# Servidor
# -----------------------------------------------------------------------------

server <- function(input, output, session) {

  tabla_reactiva <- reactiveVal(df_octavas)
  tabla_apv_reactiva <- reactiveVal(df_protector_apv)
  tabla_tramos_reactiva <- reactiveVal(df_tramos)
  semana_importada <- reactiveVal(NULL)
  ultima_conformidad <- reactiveVal(NULL)

  campania_hojas <- reactiveVal(NULL)

  observeEvent(input$archivo_campania, {
    req(input$archivo_campania)
    hojas <- tryCatch(
      leer_archivo_campania(input$archivo_campania$datapath, input$archivo_campania$name),
      error = function(e) {
        showNotification(conditionMessage(e), type = "error", duration = NULL)
        NULL
      }
    )
    if (is.null(hojas)) return()
    campania_hojas(hojas)
    showNotification(sprintf("Archivo cargado: %d hoja(s)/tabla(s).", length(hojas)), type = "message")
  })

  output$selector_hoja_campania <- renderUI({
    hojas <- campania_hojas()
    if (is.null(hojas)) return(NULL)
    nms <- names(hojas)
    norm <- normalizar_nombre_col(nms)
    cand <- which(norm %in% normalizar_nombre_col(c("EXPOSICIONES", "EXPOSICION", "MEDICIONES", "DATOS")))
    selected <- if (length(cand)) nms[cand[1]] else nms[1]
    selectInput("hoja_campania", "Hoja/tabla principal de exposiciones:", choices = nms, selected = selected)
  })

  campania_raw <- reactive({
    hojas <- campania_hojas()
    req(!is.null(hojas), input$hoja_campania)
    hojas[[input$hoja_campania]]
  })

  output$mapeo_columnas_campania <- renderUI({
    raw <- campania_raw()
    req(!is.null(raw))
    cols <- names(raw)
    opciones <- c("— No asignar —" = "", setNames(cols, cols))

    selector <- function(id, label, aliases) {
      selectInput(id, label, choices = opciones, selected = auto_columna(raw, aliases))
    }

    wellPanel(
      tags$b("Correspondencia de columnas"),
      helpText("La aplicación intenta reconocerlas automáticamente. Revise las asignaciones antes de procesar."),
      selector("map_id", "ID trabajador", c("trabajador_id", "id_trabajador", "id", "codigo_trabajador", "codigo")),
      selector("map_nombre", "Nombre/identificador descriptivo", c("trabajador", "nombre", "nombre_trabajador", "empleado")),
      selector("map_puesto", "Puesto", c("puesto", "puesto_trabajo", "ocupacion", "ocupación", "job")),
      selector("map_zona", "Zona/área", c("zona", "area", "área", "seccion", "sección")),
      selector("map_tarea", "Tarea/actividad", c("tarea", "actividad", "operacion", "operación")),
      selector("map_dia", "Día/fecha de exposición", c("dia", "día", "fecha", "jornada", "day")),
      selector("map_duracion", "Tiempo de exposición [h]", c("duracion_h", "duración_h", "tiempo_h", "tiempo_exposicion", "tiempo_exposición", "horas", "duracion")),
      selector("map_lp", "Lp [dB]", c("lp", "lp_db", "nivel_lp", "nivel_ruido")),
      selector("map_lpa", "LpA [dB(A)]", c("lpa", "lpa_dba", "nivel_a", "nivel_ponderado_a")),
      selector("map_laeq", "LAeq,T [dB(A)]", c("laeq_t", "laeqt", "laeq_t_dba", "laeq", "nivel_equivalente_a")),
      selector("map_laeqd", "LAeq,d [dB(A)] ya disponible (opcional)", c("laeq_d", "laeqd", "laeq_d_dba", "lex8h", "nivel_exposicion_diario")),
      selector("map_lceq", "LCeq,T [dB(C)]", c("lceq_t", "lceqt", "lceq_t_dbc", "lceq", "nivel_equivalente_c")),
      selector("map_lcpeak", "LCpico [dB(C)]", c("lcpico", "lcpeak", "pico_c", "pico", "lcpeak_dbc")),
      selector("map_x", "Coordenada X", c("x", "coord_x", "coordenada_x")),
      selector("map_y", "Coordenada Y", c("y", "coord_y", "coordenada_y")),
      selector("map_protector_id", "ID/modelo protector", c("protector_id", "id_protector", "protector", "epi", "modelo_protector")),
      selector("map_snr", "SNR [dB]", c("snr", "snr_db", "valor_snr")),
      selector("map_derating", "Derating SNR opcional [dB]", c("derating", "derating_db", "correccion_snr", "corrección_snr"))
    )
  })

  output$descargar_plantilla_excel <- downloadHandler(
    filename = function() if (identical(input$wn_language, "en")) "noise_campaign_template.xlsx" else "plantilla_campania_ruido.xlsx",
    content = function(file) {
      hojas <- list(
        TRABAJADORES = data.frame(
          trabajador_id = "T001", trabajador = "Operario 1", puesto = "Puesto 1",
          zona = "Zona A", jornada_h = 8, protector_id = "P01", stringsAsFactors = FALSE
        ),
        EXPOSICIONES = data.frame(
          trabajador_id = c("T001", "T001"), tarea = c("Tarea 1", "Tarea 2"),
          dia = c("Lunes", "Lunes"), zona = c("Zona A", "Zona B"),
          tiempo_h = c(2, 6), Lp_dB = c(NA, NA), LpA_dBA = c(NA, NA),
          LAeq_T_dBA = c(91, 84), LAeq_d_dBA = c(NA, NA), LCeq_T_dBC = c(95, 87), LCpico_dBC = c(132, 125),
          x = c(NA, NA), y = c(NA, NA), protector_id = c("P01", "P01"),
          SNR_dB = c(NA, NA), derating_dB = c(0, 0), stringsAsFactors = FALSE
        ),
        ESPECTROS = data.frame(
          trabajador_id = "T001", tarea = "Tarea 1",
          `31.5` = 70, `63` = 76, `125` = 82, `250` = 88, `500` = 91,
          `1000` = 94, `2000` = 93, `4000` = 89, `8000` = 84,
          check.names = FALSE, stringsAsFactors = FALSE
        ),
        PROTECTORES = data.frame(
          protector_id = "P01", modelo = "Protector ejemplo", SNR = 28, H = 33, M = 27, L = 20,
          stringsAsFactors = FALSE
        ),
        APV = data.frame(
          protector_id = rep("P01", length(bandas_apv)), frecuencia_Hz = bandas_apv,
          mf = NA_real_, sf = NA_real_, stringsAsFactors = FALSE
        ),
        MEDICIONES = data.frame(
          medicion_id = "M001", trabajador_id = "T001", fecha = "", zona = "Zona A",
          LAeq_T_dBA = NA_real_, LCeq_T_dBC = NA_real_, LCpico_dBC = NA_real_,
          stringsAsFactors = FALSE
        )
      )

      if (identical(idioma_worknoiser(input), "en")) {
        names(hojas) <- c("WORKERS", "EXPOSURES", "SPECTRA", "PROTECTORS", "APV", "MEASUREMENTS")
        hojas <- lapply(hojas, traducir_exportacion, lang = "en")
      }

      if (requireNamespace("openxlsx", quietly = TRUE)) {
        openxlsx::write.xlsx(hojas, file = file, overwrite = TRUE)
      } else if (requireNamespace("writexl", quietly = TRUE)) {
        writexl::write_xlsx(hojas, path = file)
      } else {
        stop("Para generar la plantilla Excel instale 'openxlsx' o 'writexl'.")
      }
    }
  )

  campania_procesada <- eventReactive(input$procesar_campania, {
    raw <- campania_raw()
    req(!is.null(raw))

    mapping <- list(
      id = input$map_id %||% "", nombre = input$map_nombre %||% "",
      puesto = input$map_puesto %||% "", zona = input$map_zona %||% "",
      tarea = input$map_tarea %||% "", dia = input$map_dia %||% "",
      duracion = input$map_duracion %||% "", lp = input$map_lp %||% "",
      lpa = input$map_lpa %||% "", laeq = input$map_laeq %||% "",
      laeqd = input$map_laeqd %||% "", lceq = input$map_lceq %||% "", lcpeak = input$map_lcpeak %||% "",
      x = input$map_x %||% "", y = input$map_y %||% "",
      protector_id = input$map_protector_id %||% "", snr = input$map_snr %||% "",
      derating = input$map_derating %||% ""
    )

    df <- tryCatch(
      procesar_exposiciones_importadas(raw, mapping, campania_hojas()),
      error = function(e) {
        showNotification(conditionMessage(e), type = "error", duration = NULL)
        NULL
      }
    )
    req(!is.null(df))

    derivados <- tryCatch({
      diario <- resumir_diario_campania(df)
      resumen <- resumir_campania(df, diario)
      diagnostico <- crear_diagnostico_campania(df, resumen)
      advertencias <- crear_advertencias_campania(df)
      list(diario = diario, resumen = resumen, diagnostico = diagnostico, advertencias = advertencias)
    }, error = function(e) {
      showNotification(paste("Error al resumir la campaña:", conditionMessage(e)), type = "error", duration = NULL)
      NULL
    })
    req(!is.null(derivados))
    diario <- derivados$diario
    resumen <- derivados$resumen
    diagnostico <- derivados$diagnostico
    advertencias <- derivados$advertencias

    if (!is.null(diario) && any(is.finite(diario$diferencia_calc_menos_importado_dB))) {
      idx_diff <- which(is.finite(diario$diferencia_calc_menos_importado_dB) & abs(diario$diferencia_calc_menos_importado_dB) > 0.5)
      if (length(idx_diff)) {
        extra_diff <- vapply(idx_diff, function(i) {
          sprintf("%s (%s): LAeq,d calculado difiere %.2f dB del LAeq,d importado; revisar datos/criterio de cálculo.",
                  diario$trabajador_id[i], diario$dia[i], diario$diferencia_calc_menos_importado_dB[i])
        }, character(1))
        advertencias <- rbind(advertencias, data.frame(Advertencia = extra_diff, stringsAsFactors = FALSE))
      }
    }

    # Contraste entre duración diaria y jornada declarada, cuando exista.
    if (any(is.finite(df$jornada_declarada_h)) && !is.null(diario)) {
      extra <- character()
      for (i in seq_len(nrow(diario))) {
        id <- diario$trabajador_id[i]
        d <- df[df$trabajador_id == id, , drop = FALSE]
        jvals <- d$jornada_declarada_h[is.finite(d$jornada_declarada_h)]
        if (!length(jvals)) next
        j <- jvals[1]
        tt <- diario$tiempo_total_h[i]
        if (is.finite(j) && is.finite(tt) && abs(tt - j) > 0.25) {
          extra <- c(extra, sprintf("%s (%s): duración importada %.2f h frente a jornada declarada %.2f h.", id, diario$dia[i], tt, j))
        }
      }
      if (length(extra)) advertencias <- rbind(advertencias, data.frame(Advertencia = extra, stringsAsFactors = FALSE))
    }

    list(df = df, diario = diario, resumen = resumen, diagnostico = diagnostico, advertencias = unique(advertencias))
  })

  output$diagnostico_campania <- renderTable({
    x <- campania_procesada()
    if (is.null(x)) return(data.frame(Indicador = "Sin campaña procesada", Valor = ""))
    x$diagnostico
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$advertencias_campania <- renderTable({
    x <- campania_procesada()
    if (is.null(x)) return(data.frame(Advertencia = "Sin campaña procesada"))
    x$advertencias
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$resultados_campania <- renderTable({
    x <- campania_procesada()
    if (is.null(x) || is.null(x$resumen)) return(data.frame(Mensaje = "No hay trabajadores con ID suficiente para resumir."))
    y <- x$resumen
    numcols <- vapply(y, is.numeric, logical(1))
    y[numcols] <- lapply(y[numcols], function(z) round(z, 2))
    y
  }, striped = TRUE, bordered = TRUE, spacing = "xs")

  output$resultados_diarios_campania <- renderTable({
    x <- campania_procesada()
    if (is.null(x) || is.null(x$diario)) return(data.frame(Mensaje = "No hay jornadas calculables."))
    y <- x$diario
    numcols <- vapply(y, is.numeric, logical(1))
    y[numcols] <- lapply(y[numcols], function(z) round(z, 2))
    y
  }, striped = TRUE, bordered = TRUE, spacing = "xs")

  output$preview_campania <- renderTable({
    x <- campania_procesada()
    if (is.null(x)) return(data.frame(Mensaje = "Sin campaña procesada"))
    y <- head(x$df, 100)
    numcols <- vapply(y, is.numeric, logical(1))
    y[numcols] <- lapply(y[numcols], function(z) round(z, 2))
    y
  }, striped = TRUE, bordered = TRUE, spacing = "xs")

  output$descargar_resultados_campania <- downloadHandler(
    filename = function() paste0(if (identical(input$wn_language, "en")) "noise_campaign_results_" else "resultados_campania_ruido_", Sys.Date(), ".xlsx"),
    content = function(file) {
      x <- campania_procesada()
      req(!is.null(x))
      lang <- idioma_worknoiser(input)
      hojas_salida <- list(
        EXPOSICIONES_NORMALIZADAS = traducir_exportacion(x$df, lang),
        RESULTADOS_DIARIOS = traducir_exportacion(x$diario %||% data.frame(), lang),
        RESUMEN_TRABAJADORES = traducir_exportacion(x$resumen %||% data.frame(), lang),
        DIAGNOSTICO = traducir_exportacion(x$diagnostico, lang),
        ADVERTENCIAS = traducir_exportacion(x$advertencias, lang)
      )
      if (identical(lang, "en")) names(hojas_salida) <- c("NORMALIZED_EXPOSURES", "DAILY_RESULTS", "WORKER_SUMMARY", "DIAGNOSTICS", "WARNINGS")
      if (requireNamespace("openxlsx", quietly = TRUE)) {
        openxlsx::write.xlsx(hojas_salida, file = file, overwrite = TRUE)
      } else if (requireNamespace("writexl", quietly = TRUE)) {
        writexl::write_xlsx(hojas_salida, path = file)
      } else {
        stop("Para exportar a Excel instale 'openxlsx' o 'writexl'.")
      }
    }
  )

  output$dias_semana_ui <- renderUI({
    req(input$dias_expuestos_semana)
    dias_todos <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado", "Domingo")
    idx <- match(input$dias_expuestos_semana, dias_todos)
    imp <- semana_importada()
    lapply(idx, function(i) {
      val <- NA_real_
      if (!is.null(imp) && dias_todos[i] %in% names(imp) && is.finite(imp[[dias_todos[i]]])) {
        val <- imp[[dias_todos[i]]]
      } else if (!is.null(input[[paste0("laeqd_", i)]]) && is.finite(input[[paste0("laeqd_", i)]])) {
        val <- input[[paste0("laeqd_", i)]]
      }
      numericInput(
        paste0("laeqd_", i),
        paste("LAeq,D -", dias_todos[i], "[dB(A)]:"),
        value = val
      )
    })
  })

  observeEvent(input$archivo_semana, {
    req(input$archivo_semana)
    df_imp <- tryCatch(leer_csv_flexible(input$archivo_semana$datapath), error = function(e) NULL)
    if (is.null(df_imp) || !nrow(df_imp)) {
      showNotification("No se pudo leer el CSV semanal.", type = "error")
      return()
    }
    names(df_imp) <- trimws(names(df_imp))
    col_dia <- auto_columna(df_imp, c("Dia", "Día", "dia", "day"))
    col_nivel <- auto_columna(df_imp, c("LAeq_d_dBA", "LAeq_d", "LAeqd", "nivel_diario"))
    if (!nzchar(col_dia) || !nzchar(col_nivel)) {
      showNotification("El CSV semanal debe contener Dia y LAeq_d_dBA.", type = "error")
      return()
    }
    dias_todos <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado", "Domingo")
    idx <- match(normalizar_nombre_col(df_imp[[col_dia]]), normalizar_nombre_col(dias_todos))
    niveles <- numero_flexible(df_imp[[col_nivel]])
    ok <- !is.na(idx) & is.finite(niveles)
    if (!any(ok)) {
      showNotification("No se encontraron días/niveles semanales válidos.", type = "error")
      return()
    }
    vals <- niveles[ok]; names(vals) <- dias_todos[idx[ok]]
    vals <- vals[!duplicated(names(vals), fromLast = TRUE)]
    semana_importada(vals)
    updateCheckboxGroupInput(session, "dias_expuestos_semana", selected = names(vals))
    showNotification("Datos semanales importados. Pulse Calcular.", type = "message", duration = 4)
  })

  output$tabla_tramos <- renderRHandsontable({
    req(input$usar_tramos)
    rhandsontable(tabla_tramos_reactiva(), rowHeaders = NULL)
  })

  tramos_importados_recientemente <- reactiveVal(FALSE)

  observeEvent(input$tabla_tramos, {
    if (!is.null(input$tabla_tramos)) {
      if (isTRUE(tramos_importados_recientemente())) {
        tramos_importados_recientemente(FALSE)
      } else {
        tabla_tramos_reactiva(hot_to_r(input$tabla_tramos))
      }
    }
  }, ignoreInit = TRUE)

  observeEvent(input$archivo_tramos, {
    req(input$archivo_tramos)
    df_imp <- tryCatch(
      read.csv(input$archivo_tramos$datapath, check.names = FALSE),
      error = function(e) NULL
    )
    if (is.null(df_imp)) {
      showNotification("No se pudo leer el CSV de tramos.", type = "error")
      return()
    }
    names(df_imp) <- trimws(names(df_imp))
    requeridas <- c("Tarea", "LAeq_T_dBA", "Duracion_h")
    if (!all(requeridas %in% names(df_imp))) {
      showNotification("El CSV debe contener las columnas: Tarea, LAeq_T_dBA, Duracion_h.", type = "error")
      return()
    }
    df_imp <- df_imp[, requeridas, drop = FALSE]
    df_imp$LAeq_T_dBA <- numero_flexible(df_imp$LAeq_T_dBA)
    df_imp$Duracion_h <- numero_flexible(df_imp$Duracion_h)
    tramos_importados_recientemente(TRUE)
    tabla_tramos_reactiva(df_imp)
    showNotification("Tramos importados. Pulse Calcular para obtener el LAeq,d de la jornada.", type = "message", duration = 4)
  })

  output$fuentes_lp <- renderUI({
    req(input$num_fuentes)
    if (isTRUE(input$usar_octavas)) return(NULL)

    lapply(seq_len(input$num_fuentes), function(i) {
      numericInput(
        paste0("lp_", i),
        paste("Lp de la fuente", i, "[dB(A)]:"),
        value = NA
      )
    })
  })

  output$tabla_octavas <- renderRHandsontable({
    req(input$usar_octavas)
    rhandsontable(tabla_reactiva(), rowHeaders = NULL)
  })

  output$tabla_apv <- renderRHandsontable({
    req(input$usar_proteccion)
    req(input$metodo_proteccion == "APV")
    rhandsontable(tabla_apv_reactiva(), rowHeaders = NULL)
  })

  datos_octavas <- reactive({
    req(input$usar_octavas)
    req(input$tabla_octavas)

    df <- hot_to_r(input$tabla_octavas)
    if (any(is.na(df$Lp_dB)) || any(!is.finite(df$Lp_dB))) return(NULL)

    df$Frecuencia_Hz <- as.character(df$Frecuencia_Hz)
    df$LpA <- apply_frequency_weighting(df$Lp_dB, df$Frecuencia_Hz, "A")
    df$LpC <- apply_frequency_weighting(df$Lp_dB, df$Frecuencia_Hz, "C")
    df
  })

  output$grafico_octavas <- renderPlot({
    df <- datos_octavas()
    req(!is.null(df))

    df$Frecuencia_Hz <- factor(df$Frecuencia_Hz, levels = bandas_octava)

    df_long <- rbind(
      data.frame(Frecuencia_Hz = df$Frecuencia_Hz, Tipo = "Lp", Valor = df$Lp_dB),
      data.frame(Frecuencia_Hz = df$Frecuencia_Hz, Tipo = "LpA", Valor = df$LpA)
    )

    laeq_t <- db_sum(df$LpA)
    df_long <- rbind(
      df_long,
      data.frame(Frecuencia_Hz = "Total", Tipo = "LAeq,T", Valor = laeq_t)
    )

    df_long$Frecuencia_Hz <- factor(
      df_long$Frecuencia_Hz,
      levels = c(bandas_octava, "Total")
    )

    ggplot(df_long, aes(x = Frecuencia_Hz, y = Valor, fill = Tipo)) +
      geom_col(position = position_dodge(width = 0.7), width = 0.6) +
      geom_text(
        aes(label = round(Valor, 1)),
        position = position_dodge(width = 0.7),
        vjust = -0.5,
        size = 4
      ) +
      labs(
        x = "Frecuencia (Hz)",
        y = "Nivel sonoro (dB)",
        title = "Lp y LpA por bandas de octava + LAeq,T"
      ) +
      theme_minimal() +
      ylim(0, max(df_long$Valor, na.rm = TRUE) + 10)
  })

  output$perfil_exposicion <- renderPlot({
    req(input$usar_tramos)
    df_t <- tabla_tramos_reactiva()
    req(!is.null(df_t))
    levels <- suppressWarnings(as.numeric(df_t$LAeq_T_dBA))
    durations <- suppressWarnings(as.numeric(df_t$Duracion_h))
    valid <- is.finite(levels) & is.finite(durations) & durations > 0
    req(any(valid))

    levels <- levels[valid]
    durations <- durations[valid]
    tareas <- as.character(df_t$Tarea[valid])
    starts <- c(0, head(cumsum(durations), -1))
    ends <- cumsum(durations)
    cumulative_daily <- 10 * log10(cumsum(durations * 10^(levels / 10)) / 8)

    seg <- data.frame(Tarea = tareas, Inicio = starts, Fin = ends, LAeq_T = levels)
    cum <- data.frame(Tiempo = ends, LAeq_d_acumulado = cumulative_daily)

    ggplot() +
      geom_segment(data = seg, aes(x = Inicio, xend = Fin, y = LAeq_T, yend = LAeq_T), linewidth = 1.2) +
      geom_point(data = cum, aes(x = Tiempo, y = LAeq_d_acumulado), size = 2.5) +
      geom_line(data = cum, aes(x = Tiempo, y = LAeq_d_acumulado), linetype = "dashed") +
      geom_hline(yintercept = c(80, 85, 87), linetype = c("dotted", "dashed", "dotdash")) +
      labs(
        title = "Perfil de exposición por tareas/tramos",
        subtitle = "Segmentos: LAeq,T | Línea discontinua con puntos: LAeq,d acumulado normalizado a 8 h",
        x = "Tiempo acumulado (h)", y = "Nivel [dB(A)]"
      ) +
      theme_minimal()
  })

  output$tabla_conformidad <- renderTable({
    x <- ultima_conformidad()
    if (is.null(x)) {
      if (identical(idioma_worknoiser(input), "en")) {
        return(data.frame(Message = "Click 'Calculate' to assess compliance."))
      }
      return(data.frame(Mensaje = "Pulse 'Calcular' para evaluar la conformidad."))
    }
    x
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  observeEvent(input$calcular, {
    output$resultados <- renderPrint({

      laeq_t <- NA_real_
      lceq_t_calculado <- NA_real_
      laeq_d_simple <- NA_real_
      laeq_d_jornada <- NA_real_
      laeq_d_evaluacion <- NA_real_
      laeq_t_protegido <- NA_real_
      laeq_d_protegido <- NA_real_
      df <- NULL

      # -----------------------------------------------------------------------
      # Cálculo del nivel ambiental
      # -----------------------------------------------------------------------

      if (isTRUE(input$usar_octavas)) {
        df <- datos_octavas()

        if (is.null(df)) {
          cat("Faltan datos válidos en la tabla de bandas de octava.\n")
          return()
        }

        laeq_t <- db_sum(df$LpA)
        lceq_t_calculado <- db_sum(df$LpC)

        cat("Tabla por bandas de octava:\n\n")
        print(data.frame(
          Frecuencia_Hz = df$Frecuencia_Hz,
          Lp_dB = round(df$Lp_dB, 1),
          LpA_dB = round(df$LpA, 1),
          LpC_dB = round(df$LpC, 1)
        ))

        cat("\nLAeq,T calculado a partir de las bandas:", round(laeq_t, 2), "dB(A)\n")
        cat("LCeq,T calculado a partir de las bandas:", round(lceq_t_calculado, 2), "dB(C)\n")

      } else {
        req(input$num_fuentes)
        lps <- sapply(seq_len(input$num_fuentes), function(i) input[[paste0("lp_", i)]])

        if (length(lps) == 0 || any(is.na(lps)) || any(!is.finite(lps))) {
          if (isTRUE(input$usar_tramos) || isTRUE(input$calcular_s)) {
            cat("Nivel ambiental principal: no calculado (no es necesario para el cálculo por tramos/semanal).\n")
          } else {
            cat("No se han introducido niveles válidos para todas las fuentes.\n")
            return()
          }
        } else {
          laeq_t <- db_sum(lps)
          cat("LAeq,T (suma energética de fuentes):", round(laeq_t, 2), "dB(A)\n")
        }
      }

      # -----------------------------------------------------------------------
      # LAeq,d simple y jornada por tareas/tramos
      # -----------------------------------------------------------------------

      laeq_d_simple <- calc_laeq_d(laeq_t, input$tiempo)
      if (is.finite(laeq_d_simple)) {
        cat("LAeq,d (nivel actual normalizado a 8 h):", round(laeq_d_simple, 2), "dB(A)\n")
      } else {
        cat("LAeq,d simple: no calculado (se requiere 0 < tiempo <= 24 h).\n")
      }

      tramos_levels <- NULL
      tramos_durations <- NULL
      if (isTRUE(input$usar_tramos)) {
        df_t <- tabla_tramos_reactiva()
        tramos_levels <- suppressWarnings(as.numeric(df_t$LAeq_T_dBA))
        tramos_durations <- suppressWarnings(as.numeric(df_t$Duracion_h))
        valid <- is.finite(tramos_levels) & is.finite(tramos_durations) & tramos_durations > 0

        if (any(valid)) {
          tramos_levels <- tramos_levels[valid]
          tramos_durations <- tramos_durations[valid]
          laeq_d_jornada <- calc_laeq_d_multi(tramos_levels, tramos_durations)
          cat("\nJornada por tareas/tramos:\n")
          print(data.frame(
            Tarea = as.character(df_t$Tarea[valid]),
            LAeq_T_dBA = round(tramos_levels, 2),
            Duracion_h = round(tramos_durations, 3)
          ))
          cat("Duración total incluida:", round(sum(tramos_durations), 3), "h\n")
          cat("LAeq,d conjunto de la jornada:", round(laeq_d_jornada, 2), "dB(A)\n")
        } else {
          cat("\nJornada por tareas/tramos: no hay filas válidas.\n")
        }
      }

      laeq_d_evaluacion <- if (is.finite(laeq_d_jornada)) laeq_d_jornada else laeq_d_simple

      # -----------------------------------------------------------------------
      # LCpico: independiente de LCeq,T
      # -----------------------------------------------------------------------

      if (is.finite(input$pico)) {
        cat("LCpico indicado:", round(input$pico, 2), "dB(C)\n")
      } else {
        cat("LCpico: no indicado. No se estima a partir de LCeq,T ni de las bandas.\n")
      }

      # -----------------------------------------------------------------------
      # Exposición semanal: resultado normativo + media de días reales
      # -----------------------------------------------------------------------

      if (isTRUE(input$calcular_s)) {
        dias_todos <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado", "Domingo")
        idx_dias <- match(input$dias_expuestos_semana, dias_todos)
        laeqd_vals <- sapply(idx_dias, function(i) input[[paste0("laeqd_", i)]])
        semanal <- calc_weekly_exposure(laeqd_vals)

        cat("\nExposición semanal:\n")
        if (semanal$exposed_days > 0) {
          cat("Días con LAeq,d válido:", semanal$exposed_days, "\n")
          cat("LAeq,s normativo (referencia fija 5 jornadas):", round(semanal$normative_5d, 2), "dB(A)\n")
          cat("Media energética de los días realmente expuestos:", round(semanal$actual_days_mean, 2), "dB(A)\n")
          cat("Nota: el segundo valor es descriptivo; no sustituye al LAeq,s definido por el RD 286/2006.\n")
        } else {
          cat("No se han introducido valores diarios LAeq,d válidos.\n")
        }
      }

      # -----------------------------------------------------------------------
      # Protectores auditivos
      # -----------------------------------------------------------------------

      if (isTRUE(input$usar_proteccion)) {

        metodo <- input$metodo_proteccion

        if (identical(metodo, "APV")) {
          if (!isTRUE(input$usar_octavas) || is.null(df)) {
            cat("\nMétodo APV: se requieren niveles ambientales por bandas de octava.\n")
          } else if (is.null(input$tabla_apv)) {
            cat("\nMétodo APV: falta la tabla de datos del protector.\n")
          } else {
            df_apv <- hot_to_r(input$tabla_apv)

            if (any(is.na(df_apv$mf)) || any(is.na(df_apv$sf)) ||
                any(!is.finite(df_apv$mf)) || any(!is.finite(df_apv$sf))) {
              cat("\nMétodo APV: faltan valores mf o sf.\n")
            } else {
              df_apv$Frecuencia_Hz <- as.character(df_apv$Frecuencia_Hz)
              df_apv$APV <- df_apv$mf - df_apv$sf

              df_ruido_apv <- df[df$Frecuencia_Hz %in% df_apv$Frecuencia_Hz, , drop = FALSE]
              idx <- match(df_ruido_apv$Frecuencia_Hz, df_apv$Frecuencia_Hz)

              if (any(is.na(idx))) {
                cat("\nMétodo APV: no se han podido emparejar todas las frecuencias del protector.\n")
              } else {
                df_ruido_apv$mf <- df_apv$mf[idx]
                df_ruido_apv$sf <- df_apv$sf[idx]
                df_ruido_apv$APV <- df_apv$APV[idx]
                df_ruido_apv$LpA_protegido <- df_ruido_apv$LpA - df_ruido_apv$APV
                laeq_t_protegido <- db_sum(df_ruido_apv$LpA_protegido)

                cat("\nMétodo APV (APV = mf - sf; eficacia asumida 84 %):\n")
                print(data.frame(
                  Frecuencia_Hz = df_ruido_apv$Frecuencia_Hz,
                  LpA_dB = round(df_ruido_apv$LpA, 1),
                  mf_dB = round(df_ruido_apv$mf, 1),
                  sf_dB = round(df_ruido_apv$sf, 1),
                  APV_dB = round(df_ruido_apv$APV, 1),
                  LpA_protegido_dB = round(df_ruido_apv$LpA_protegido, 1)
                ))
                cat("L'Aeq,T (protección APV, bandas 63-8000 Hz):", round(laeq_t_protegido, 2), "dB(A)\n")
                cat("Nota: la banda ambiental de 31,5 Hz se incluye en LAeq,T/LCeq,T, pero no en APV si el protector no aporta mf/sf a 31,5 Hz.\n")
              }
            }
          }
        }

        if (identical(metodo, "HML")) {
          lceq_t <- if (is.finite(input$lceq_manual)) input$lceq_manual else lceq_t_calculado

          if (!is.finite(lceq_t)) {
            cat("\nMétodo HML: se requiere LCeq,T medido o calculable a partir de bandas de octava.\n")
          } else {
            hml <- calc_hml(laeq_t, lceq_t, input$H, input$M, input$L)

            if (is.null(hml)) {
              cat("\nMétodo HML: faltan H, M o L.\n")
            } else {
              laeq_t_protegido <- hml$laeq_t_protegido
              cat("\nMétodo HML:\n")
              cat("LAeq,T =", round(laeq_t, 2), "dB(A)\n")
              cat("LCeq,T =", round(lceq_t, 2), "dB(C)",
                  if (is.finite(input$lceq_manual)) "[medido/manual]" else "[calculado por bandas]", "\n")
              cat("LCeq,T - LAeq,T =", round(hml$delta_ca, 2), "dB\n")
              cat("PNR =", round(hml$pnr, 2), "dB\n")
              cat("L'Aeq,T (protección HML):", round(laeq_t_protegido, 2), "dB(A)\n")
            }
          }
        }

        if (identical(metodo, "SNR")) {
          lceq_t <- if (is.finite(input$lceq_manual)) input$lceq_manual else lceq_t_calculado
          derating <- if (is.finite(input$snr_derating)) input$snr_derating else 0

          if (!is.finite(input$SNR)) {
            cat("\nMétodo SNR: falta el valor SNR del protector.\n")
          } else if (!is.finite(lceq_t)) {
            cat("\nMétodo SNR: se requiere LCeq,T medido o calculable a partir de bandas de octava.\n")
          } else if (derating > input$SNR) {
            cat("\nMétodo SNR: el derating no puede ser mayor que el SNR.\n")
          } else {
            laeq_t_protegido <- calc_snr(lceq_t, input$SNR, derating)
            cat("\nMétodo SNR:\n")
            cat("LCeq,T =", round(lceq_t, 2), "dB(C)",
                if (is.finite(input$lceq_manual)) "[medido/manual]" else "[calculado por bandas]", "\n")
            cat("SNR =", round(input$SNR, 2), "dB\n")
            cat("Derating adicional definido por el usuario =", round(derating, 2), "dB\n")
            cat("L'Aeq,T = LCeq,T - (SNR - derating):", round(laeq_t_protegido, 2), "dB(A)\n")
          }
        }

        if (is.finite(laeq_t_protegido) && is.finite(input$tiempo) && input$tiempo > 0) {
          laeq_d_protegido <- calc_laeq_d(laeq_t_protegido, input$tiempo)
          cat("L'Aeq,d estimado con protector para el tiempo indicado:", round(laeq_d_protegido, 2), "dB(A)\n")
          if (is.finite(laeq_d_jornada)) {
            cat("Nota: este L'Aeq,d protegido corresponde al nivel/tiempo principal, no a la jornada multitramos completa.\n")
          }
        }
      }

      # -----------------------------------------------------------------------
      # Dosis y TMA
      # -----------------------------------------------------------------------

      if (isTRUE(input$calcular_dosis)) {
        params <- if (input$criterio_dosis == "CUSTOM") {
          criterion_params(
            "CUSTOM",
            criterion_level = input$dosis_criterio,
            exchange_rate = input$dosis_intercambio,
            reference_time_h = input$dosis_tref,
            threshold = input$dosis_umbral
          )
        } else {
          criterion_params(input$criterio_dosis)
        }

        if (is.null(params)) {
          cat("\nDosis/TMA: parámetros de criterio no válidos.\n")
        } else {
          if (!is.null(tramos_levels) && length(tramos_levels) > 0) {
            dose_levels <- tramos_levels
            dose_durations <- tramos_durations
          } else if (is.finite(laeq_t) && is.finite(input$tiempo) && input$tiempo > 0) {
            dose_levels <- laeq_t
            dose_durations <- input$tiempo
          } else {
            dose_levels <- NULL
            dose_durations <- NULL
          }

          cat("\nDosis de ruido / TMA —", params$name, "\n")
          cat("Nivel criterio:", params$criterion_level, "dB(A) | Intercambio:", params$exchange_rate,
              "dB | Referencia:", params$reference_time_h, "h | Umbral:", params$threshold, "dB(A)\n")

          if (is.null(dose_levels)) {
            cat("No hay niveles y duraciones válidos para calcular la dosis.\n")
          } else {
            dose <- noise_dose(
              dose_levels, dose_durations,
              params$criterion_level, params$exchange_rate,
              params$reference_time_h, params$threshold
            )
            cat("Dosis acumulada:", round(dose, 2), "%\n")

            tma <- max_exposure_time(
              dose_levels,
              params$criterion_level,
              params$exchange_rate,
              params$reference_time_h
            )
            print(data.frame(
              Nivel_dBA = round(dose_levels, 2),
              Duracion_real_h = round(dose_durations, 3),
              TMA_h_criterio = round(tma, 3),
              Incluido_en_dosis = dose_levels >= params$threshold
            ))
            cat("Nota: TMA significa tiempo máximo según el criterio seleccionado; no se etiqueta como 'tiempo seguro' universal.\n")
          }
        }
      }

      # -----------------------------------------------------------------------
      # Evaluación normativa española
      # -----------------------------------------------------------------------

      if (is.finite(laeq_d_evaluacion)) {
        protected_for_limit <- if (is.finite(laeq_d_jornada)) NA_real_ else laeq_d_protegido
        conf <- evaluate_noise_compliance(
          laeq_d = laeq_d_evaluacion,
          lcpeak = input$pico,
          protected_laeq_d = protected_for_limit,
          protector_used = isTRUE(input$usar_proteccion)
        )
        ultima_conformidad(conf)
        cat("\nEvaluación RD 286/2006:\n")
        print(conf, row.names = FALSE)
        if (is.finite(laeq_d_jornada) && isTRUE(input$usar_proteccion)) {
          cat("Nota: con jornada multitramos no se aplica automáticamente la atenuación de un único cálculo de protector a todos los tramos.\n")
        }
      } else {
        ultima_conformidad(NULL)
        cat("\nEvaluación RD 286/2006: no disponible sin LAeq,d válido.\n")
      }
    })
  })
}

shinyApp(ui = ui, server = server)
