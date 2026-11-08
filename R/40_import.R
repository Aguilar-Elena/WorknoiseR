# Flexible numeric and CSV import helpers

#' Convert heterogeneous spreadsheet/CSV columns to numeric values
#' @export
numeric_flexible <- function(x) {
  if (is.data.frame(x)) {
    if (ncol(x) == 0L) return(numeric())
    x <- x[[1]]
  }
  if (is.list(x)) {
    x <- vapply(x, function(z) {
      if (is.null(z) || length(z) == 0L) return(NA_character_)
      as.character(z[[1]])
    }, character(1))
  }
  if (is.numeric(x)) return(as.numeric(x))
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "N/A", "-", "--")] <- NA_character_
  x <- ifelse(grepl(",", x) & !grepl("\\.", x), gsub(",", ".", x), x)
  suppressWarnings(as.numeric(x))
}

.worknoiser_read_csv_once <- function(path, sep, dec, encoding = "UTF-8") {
  tryCatch(utils::read.csv(path, sep = sep, dec = dec, check.names = FALSE,
                           stringsAsFactors = FALSE, fileEncoding = encoding),
           error = function(e) NULL)
}

#' Read CSV files using common Spanish/international separator conventions
#'
#' Tries semicolon/comma/tab delimiters without converting columns to lists.
#' @export
read_noise_csv <- function(path) {
  if (length(path) != 1L || !file.exists(path)) stop(worknoiser_translate("file_missing"), call. = FALSE)
  candidates <- list(
    .worknoiser_read_csv_once(path, ";", ","),
    .worknoiser_read_csv_once(path, ",", "."),
    .worknoiser_read_csv_once(path, "\t", ".")
  )
  score <- vapply(candidates, function(x) if (is.null(x)) -Inf else ncol(x), numeric(1))
  if (!any(is.finite(score))) stop(worknoiser_translate("csv_read_error"), call. = FALSE)
  candidates[[which.max(score)]]
}
