# WorknoiseR 0.5.0 dependencies / dependencias
# ES: Ejecute este script si la app informa de paquetes ausentes.
# EN: Run this script if the app reports missing packages.

required <- c(
  "shiny", "rhandsontable", "ggplot2", "plotly", "reshape2",
  "viridisLite", "scales", "sf"
)
optional <- c("readxl", "openxlsx", "writexl", "png", "jpeg", "tiff", "pdftools")

missing_required <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_required)) install.packages(missing_required)

message("Required dependencies checked / Dependencias obligatorias comprobadas.")
message("Optional packages / Paquetes opcionales: ", paste(optional, collapse = ", "))
