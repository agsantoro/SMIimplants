# PLOS ONE: https://journals.plos.org/plosone/s/figures (2026-10-15).
# Dimensions are inches at final publication size, not a resized screenshot.
exportFigure = function(filename, draw, width = 7.5, height = 5.5, dpi = 600) {
  stopifnot(width >= 2.63, width <= 7.5, height <= 8.75,
            dpi >= 300, dpi <= 600)
  out = Sys.getenv("SMI_FIGURE_DIR", unset = "outputs/figures")
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  path = file.path(out, filename)
  grDevices::tiff(path, width = width, height = height, units = "in",
                 res = dpi, compression = "lzw", type = "cairo",
                 bg = "white", family = "Arial", pointsize = 10)
  tryCatch(draw(), finally = grDevices::dev.off())
  if (file.info(path)$size >= 10e6) stop("Figure exceeds PLOS 10 MB limit: ", path)
  message("Exported ", path)
  invisible(path)
}
