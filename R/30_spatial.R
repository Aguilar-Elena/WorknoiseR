# Spatial acoustic model independent from Shiny

#' Free-field level at distance from a level specified at 1 metre
#' @export
level_at_distance_free_field <- function(level_1m, distance_m) {
  level_1m <- suppressWarnings(as.numeric(level_1m))
  distance_m <- suppressWarnings(as.numeric(distance_m))
  if (length(level_1m) != length(distance_m) && length(level_1m) != 1L && length(distance_m) != 1L) return(NA_real_)
  if (any(!is.finite(level_1m)) || any(!is.finite(distance_m)) || any(distance_m <= 0)) return(NA_real_)
  level_1m - 20 * log10(distance_m)
}

#' Distance at which a free-field source reaches a target level
#' @export
distance_for_level_free_field <- function(level_1m, target_level) {
  level_1m <- suppressWarnings(as.numeric(level_1m)); target_level <- suppressWarnings(as.numeric(target_level))
  if (any(!is.finite(level_1m)) || length(target_level) != 1L || !is.finite(target_level)) return(rep(NA_real_, length(level_1m)))
  pmax(1, 10^((level_1m - target_level) / 20))
}

#' Convert polyline paths to acoustic barrier segments
#' @export
barrier_segments_from_paths <- function(df) {
  if (is.null(df) || !is.data.frame(df) || !nrow(df) || !all(c("x", "y", "group") %in% names(df))) return(data.frame())
  df <- df[is.finite(df$x) & is.finite(df$y), , drop = FALSE]
  if (!nrow(df)) return(data.frame())
  if (!"layer" %in% names(df)) df$layer <- "barrier"
  groups <- split(df, df$group)
  out <- lapply(groups, function(g) {
    if (nrow(g) < 2L) return(NULL)
    data.frame(x1 = g$x[-nrow(g)], y1 = g$y[-nrow(g)],
               x2 = g$x[-1L], y2 = g$y[-1L],
               layer = g$layer[-1L], group = g$group[-1L], stringsAsFactors = FALSE)
  })
  out <- Filter(Negate(is.null), out)
  if (!length(out)) return(data.frame())
  seg <- do.call(rbind, out)
  len <- sqrt((seg$x2 - seg$x1)^2 + (seg$y2 - seg$y1)^2)
  seg <- seg[is.finite(len) & len > 1e-4, , drop = FALSE]
  rownames(seg) <- NULL
  seg
}

.worknoiser_barrier_hits <- function(sx, sy, rx, ry, seg, grouping_m = 0.30) {
  if (is.null(seg) || !is.data.frame(seg) || !nrow(seg) ||
      !all(c("x1", "y1", "x2", "y2") %in% names(seg)) ||
      !all(is.finite(c(sx, sy, rx, ry)))) return(data.frame())
  dx <- rx - sx; dy <- ry - sy; L <- sqrt(dx^2 + dy^2)
  if (!is.finite(L) || L < 1e-8) return(data.frame())
  wx <- seg$x2 - seg$x1; wy <- seg$y2 - seg$y1
  den <- dx * wy - dy * wx
  ok <- is.finite(den) & abs(den) > 1e-10
  if (!any(ok)) return(data.frame())
  qx <- seg$x1 - sx; qy <- seg$y1 - sy
  tt <- uu <- rep(NA_real_, nrow(seg))
  tt[ok] <- (qx[ok] * wy[ok] - qy[ok] * wx[ok]) / den[ok]
  uu[ok] <- (qx[ok] * dy - qy[ok] * dx) / den[ok]
  eps <- 1e-6
  hit <- ok & tt > eps & tt < (1 - eps) & uu > eps & uu < (1 - eps)
  idx <- which(hit)
  if (!length(idx)) return(data.frame())
  d <- tt[idx] * L; ord <- order(d); idx <- idx[ord]; d <- d[ord]
  grouping_m <- if (length(grouping_m) == 1L && is.finite(grouping_m) && grouping_m >= 0) grouping_m else 0.30
  if (length(d) > 1L && grouping_m > 0) {
    groups <- cumsum(c(TRUE, diff(d) > grouping_m))
    keep <- vapply(split(seq_along(d), groups), function(ii) ii[ceiling(length(ii) / 2)], integer(1))
    idx <- idx[keep]; d <- d[keep]
  }
  data.frame(idx = idx, d = d, x_int = sx + (d / L) * dx,
             y_int = sy + (d / L) * dy, stringsAsFactors = FALSE)
}

.worknoiser_wall_loss <- function(n, first = 12, additional = 6, maximum = 30) {
  if (!is.finite(n) || n <= 0) return(0)
  first <- max(0, first); additional <- max(0, additional); maximum <- max(first, maximum)
  min(maximum, first + additional * max(0, n - 1))
}

.worknoiser_edge_level <- function(sx, sy, rx, ry, Li, seg, hits,
                                   edge_loss = 6, extra_penalty = 3) {
  if (is.null(hits) || !nrow(hits) || is.null(seg) || !nrow(seg)) return(NA_real_)
  r0 <- sqrt((rx - sx)^2 + (ry - sy)^2)
  if (!is.finite(r0) || r0 <= 0) return(NA_real_)
  ids <- unique(hits$idx[seq_len(min(3L, nrow(hits)))])
  candidates <- numeric()
  for (j in ids) {
    ends <- rbind(c(seg$x1[j], seg$y1[j]), c(seg$x2[j], seg$y2[j]))
    for (k in seq_len(nrow(ends))) {
      ex <- ends[k, 1]; ey <- ends[k, 2]
      path <- sqrt((ex - sx)^2 + (ey - sy)^2) + sqrt((rx - ex)^2 + (ry - ey)^2)
      if (!is.finite(path) || path < 1) next
      excess <- max(0, path - r0)
      excess_loss <- 10 * log10(1 + excess / 0.25)
      extra <- extra_penalty * max(0, nrow(hits) - 1)
      candidates <- c(candidates, Li - 20 * log10(path) - max(0, edge_loss) - excess_loss - extra)
    }
  }
  if (!length(candidates)) return(NA_real_)
  max(candidates, na.rm = TRUE)
}

.worknoiser_room_constant <- function(reverberation, n_sources) {
  if (is.null(reverberation) || identical(reverberation, FALSE)) return(rep(NA_real_, n_sources))
  if (isTRUE(reverberation)) reverberation <- list(enabled = TRUE)
  if (!is.list(reverberation) || identical(reverberation$enabled, FALSE)) return(rep(NA_real_, n_sources))
  if (!is.null(reverberation$room_constant)) {
    R <- suppressWarnings(as.numeric(reverberation$room_constant))
    if (length(R) == 1L) R <- rep(R, n_sources)
    if (length(R) == n_sources) return(R)
  }
  dims <- suppressWarnings(as.numeric(c(reverberation$length, reverberation$width, reverberation$height)))
  alpha <- suppressWarnings(as.numeric(reverberation$alpha))
  if (length(dims) == 3L && all(is.finite(dims)) && all(dims > 0) &&
      length(alpha) == 1L && is.finite(alpha) && alpha > 0 && alpha < 1) {
    S <- 2 * (dims[1] * dims[2] + dims[1] * dims[3] + dims[2] * dims[3])
    A <- S * alpha
    return(rep(A / (1 - alpha), n_sources))
  }
  rep(NA_real_, n_sources)
}

.worknoiser_source_levels <- function(sources) {
  candidates <- c("Li", "level_1m", "Lp_1m", "Lp", "level")
  hit <- candidates[candidates %in% names(sources)]
  if (!length(hit)) stop("sources debe contener una columna Li, level_1m, Lp_1m, Lp o level.", call. = FALSE)
  as.numeric(sources[[hit[1]]])
}

#' Sound level at a receiver from multiple sources
#'
#' This is the Shiny-independent public spatial engine. It mirrors the current
#' WorknoiseR direct-field and hybrid barrier logic, and supports a simplified
#' global/room reverberant component through an explicit configuration list.
#'
#' @param x,y Receiver coordinates in metres.
#' @param sources Data frame with x, y and source level at 1 m (Li/level_1m/etc.).
#' @param barriers Optional data frame of x1,y1,x2,y2 segments.
#' @param barrier Optional list: model, first_loss, additional_loss, max_loss,
#'   edge_loss, grouping_m.
#' @param reverberation Optional list with enabled, model (global/room),
#'   room_constant or length/width/height/alpha, and Q.
#' @return Combined level in dB.
#' @export
receiver_level <- function(x, y, sources, barriers = NULL, barrier = NULL,
                           reverberation = NULL) {
  x <- suppressWarnings(as.numeric(x)); y <- suppressWarnings(as.numeric(y))
  if (length(x) != 1L || length(y) != 1L || !all(is.finite(c(x, y)))) return(NA_real_)
  if (is.null(sources) || !is.data.frame(sources) || !nrow(sources) || !all(c("x", "y") %in% names(sources))) return(NA_real_)
  Li <- .worknoiser_source_levels(sources)
  if (length(Li) != nrow(sources) || any(!is.finite(Li)) || any(!is.finite(sources$x)) || any(!is.finite(sources$y))) return(NA_real_)

  if (is.null(barrier)) barrier <- list()
  bmodel <- if (!is.null(barrier$model)) as.character(barrier$model)[1] else "hybrid"
  first_loss <- if (!is.null(barrier$first_loss)) as.numeric(barrier$first_loss) else 12
  additional_loss <- if (!is.null(barrier$additional_loss)) as.numeric(barrier$additional_loss) else 6
  max_loss <- if (!is.null(barrier$max_loss)) as.numeric(barrier$max_loss) else 30
  edge_loss <- if (!is.null(barrier$edge_loss)) as.numeric(barrier$edge_loss) else 6
  grouping_m <- if (!is.null(barrier$grouping_m)) as.numeric(barrier$grouping_m) else 0.30
  use_barriers <- !is.null(barriers) && is.data.frame(barriers) && nrow(barriers) > 0

  rev_enabled <- is.list(reverberation) && !identical(reverberation$enabled, FALSE)
  rev_model <- if (rev_enabled && !is.null(reverberation$model)) as.character(reverberation$model)[1] else "global"
  Q <- if (rev_enabled && !is.null(reverberation$Q)) suppressWarnings(as.numeric(reverberation$Q)[1]) else 1
  if (!is.finite(Q) || Q <= 0) Q <- 1
  R <- .worknoiser_room_constant(reverberation, nrow(sources))

  levels <- numeric(nrow(sources))
  for (i in seq_len(nrow(sources))) {
    sx <- as.numeric(sources$x[i]); sy <- as.numeric(sources$y[i])
    r <- max(sqrt((x - sx)^2 + (y - sy)^2), 1)
    hits <- if (use_barriers) .worknoiser_barrier_hits(sx, sy, x, y, barriers, grouping_m) else data.frame()
    n_cross <- nrow(hits)
    direct_free <- Li[i] - 20 * log10(r)
    if (n_cross > 0L) {
      if (identical(tolower(bmodel), "hybrid") || identical(tolower(bmodel), "hibrido")) {
        transmitted <- direct_free - .worknoiser_wall_loss(n_cross, first_loss, additional_loss, max_loss)
        edge <- .worknoiser_edge_level(sx, sy, x, y, Li[i], barriers, hits,
                                       edge_loss, max(1, additional_loss / 2))
        direct <- db_add(c(transmitted, if (is.finite(edge)) edge else numeric()))
      } else {
        direct <- direct_free - first_loss * n_cross
      }
    } else direct <- direct_free

    if (rev_enabled) {
      add_diffuse <- identical(tolower(rev_model), "global") || n_cross == 0L
      if (add_diffuse && is.finite(R[i]) && R[i] > 0) {
        Lw <- Li[i] + 10 * log10(4 * pi / Q)
        diffuse <- Lw + 10 * log10(4 / R[i])
        levels[i] <- db_add(c(direct, diffuse))
      } else levels[i] <- direct
    } else levels[i] <- direct
  }
  db_add(levels)
}

.worknoiser_points_in_polygon <- function(x, y, polygon) {
  if (is.null(polygon) || !is.data.frame(polygon) || !all(c("x", "y") %in% names(polygon)) || nrow(polygon) < 3L) return(rep(TRUE, length(x)))
  px <- as.numeric(polygon$x); py <- as.numeric(polygon$y)
  ok <- is.finite(px) & is.finite(py); px <- px[ok]; py <- py[ok]
  if (length(px) < 3L) return(rep(TRUE, length(x)))
  if (px[1] != px[length(px)] || py[1] != py[length(py)]) { px <- c(px, px[1]); py <- c(py, py[1]) }
  inside <- rep(FALSE, length(x)); j <- length(px) - 1L
  for (i in seq_len(length(px) - 1L)) {
    xi <- px[i]; yi <- py[i]; xj <- px[j]; yj <- py[j]
    crosses <- ((yi > y) != (yj > y)) & (x < (xj - xi) * (y - yi) / ((yj - yi) + .Machine$double.eps) + xi)
    inside <- xor(inside, crosses)
    j <- i
  }
  inside
}

#' Calculate a rectangular or polygon-clipped noise grid
#' @export
noise_grid <- function(sources, domain = NULL, polygon = NULL, resolution = NULL,
                       barriers = NULL, barrier = NULL, reverberation = NULL) {
  if (is.null(sources) || !is.data.frame(sources) || !nrow(sources)) stop(worknoiser_translate("source_required"), call. = FALSE)
  if (!is.null(polygon)) {
    if (!is.data.frame(polygon) || !all(c("x", "y") %in% names(polygon))) stop("polygon debe contener x e y.", call. = FALSE)
    xmin <- min(polygon$x, na.rm = TRUE); xmax <- max(polygon$x, na.rm = TRUE)
    ymin <- min(polygon$y, na.rm = TRUE); ymax <- max(polygon$y, na.rm = TRUE)
  } else if (!is.null(domain)) {
    d <- suppressWarnings(as.numeric(domain))
    if (length(d) != 4L || any(!is.finite(d))) stop("domain debe ser c(xmin, xmax, ymin, ymax).", call. = FALSE)
    xmin <- d[1]; xmax <- d[2]; ymin <- d[3]; ymax <- d[4]
  } else {
    margin <- 5
    xmin <- min(sources$x, na.rm = TRUE) - margin; xmax <- max(sources$x, na.rm = TRUE) + margin
    ymin <- min(sources$y, na.rm = TRUE) - margin; ymax <- max(sources$y, na.rm = TRUE) + margin
  }
  dx <- xmax - xmin; dy <- ymax - ymin
  if (!all(is.finite(c(dx, dy))) || dx <= 0 || dy <= 0) stop("El dominio no tiene dimensiones v\u00e1lidas.", call. = FALSE)
  if (is.null(resolution)) resolution <- min(0.50, max(0.02, max(dx, dy) / 160))
  resolution <- suppressWarnings(as.numeric(resolution)[1])
  if (!is.finite(resolution) || resolution <= 0) stop("resolution debe ser > 0.", call. = FALSE)
  nx <- max(3L, ceiling(dx / resolution) + 1L); ny <- max(3L, ceiling(dy / resolution) + 1L)
  pts <- expand.grid(x = seq(xmin, xmax, length.out = nx), y = seq(ymin, ymax, length.out = ny))
  if (!is.null(polygon)) pts <- pts[.worknoiser_points_in_polygon(pts$x, pts$y, polygon), , drop = FALSE]
  pts$Lp <- mapply(function(xx, yy) receiver_level(xx, yy, sources, barriers, barrier, reverberation), pts$x, pts$y)
  rownames(pts) <- NULL
  pts
}

#' Extract isophone contour lines from a WorknoiseR grid
#' @export
noise_contours <- function(grid, breaks = c(75, 80, 85, 90, 95, 100, 105, 110), level_col = "Lp") {
  if (is.null(grid) || !is.data.frame(grid) || !all(c("x", "y", level_col) %in% names(grid))) stop(worknoiser_translate("grid_required"), call. = FALSE)
  xs <- sort(unique(grid$x)); ys <- sort(unique(grid$y))
  if (length(xs) < 2L || length(ys) < 2L) return(data.frame())
  z <- matrix(NA_real_, nrow = length(xs), ncol = length(ys))
  ix <- match(grid$x, xs); iy <- match(grid$y, ys)
  z[cbind(ix, iy)] <- as.numeric(grid[[level_col]])
  cls <- grDevices::contourLines(x = xs, y = ys, z = z, levels = breaks)
  if (!length(cls)) return(data.frame(level = numeric(), group = integer(), x = numeric(), y = numeric()))
  do.call(rbind, lapply(seq_along(cls), function(i) {
    data.frame(level = cls[[i]]$level, group = i, x = cls[[i]]$x, y = cls[[i]]$y)
  }))
}

#' Estimate source levels from receiver measurements
#' @export
estimate_source_levels <- function(measurements, sources, barriers = NULL, barrier = NULL,
                                   reverberation = NULL, start = 95, lower = 40,
                                   upper = 140) {
  if (is.null(measurements) || !is.data.frame(measurements) || !all(c("x", "y", "Lp") %in% names(measurements))) stop("measurements debe contener x, y y Lp.", call. = FALSE)
  measurements <- measurements[complete.cases(measurements[, c("x", "y", "Lp")]), , drop = FALSE]
  if (!nrow(measurements) || any(!is.finite(as.matrix(measurements[, c("x", "y", "Lp")])))) stop("No hay mediciones v\u00e1lidas.", call. = FALSE)
  if (is.null(sources) || !is.data.frame(sources) || !nrow(sources) || !all(c("x", "y") %in% names(sources))) stop("sources debe contener x e y.", call. = FALSE)
  n <- nrow(sources)
  expand_param <- function(x, n, nm) {
    x <- suppressWarnings(as.numeric(x)); if (length(x) == 1L) x <- rep(x, n)
    if (length(x) != n || any(!is.finite(x))) stop(nm, " no es v\u00e1lido.", call. = FALSE)
    x
  }
  start <- expand_param(start, n, "start"); lower <- expand_param(lower, n, "lower"); upper <- expand_param(upper, n, "upper")
  if (any(lower >= upper)) stop("lower debe ser menor que upper.", call. = FALSE)
  source_template <- sources
  source_template$Li <- start
  objective <- function(Li) {
    s <- source_template; s$Li <- Li
    pred <- mapply(function(xx, yy) receiver_level(xx, yy, s, barriers, barrier, reverberation), measurements$x, measurements$y)
    if (any(!is.finite(pred))) return(.Machine$double.xmax / 100)
    sum((pred - measurements$Lp)^2)
  }
  opt <- stats::optim(par = start, fn = objective, lower = lower, upper = upper, method = "L-BFGS-B")
  fitted_sources <- source_template; fitted_sources$Li <- as.numeric(opt$par)
  predicted <- mapply(function(xx, yy) receiver_level(xx, yy, fitted_sources, barriers, barrier, reverberation), measurements$x, measurements$y)
  nm <- if ("nombre" %in% names(sources)) as.character(sources$nombre) else if ("name" %in% names(sources)) as.character(sources$name) else paste0("source_", seq_len(n))
  levels <- setNames(as.numeric(opt$par), nm)
  list(levels = levels, objective = opt$value, convergence = opt$convergence,
       message = opt$message, predicted = predicted,
       residuals = measurements$Lp - predicted, fitted_sources = fitted_sources,
       optim = opt)
}
