# 00_utils.R

# --- 1. RDS Saver ---
this_saveRDS <- function(x) {
  obj_name <- deparse(substitute(x))
  file_name <- paste0(obj_name, ".rds")
  file_path <- here::here("results", file_name)
  saveRDS(x, file = file_path)
  message(">>> Saved to: ", file_path)
}

# --- 2. Figure Saver ---
this_ggsave <- function(x, width = 16, height = 14) {
  obj_name <- deparse(substitute(x))
  file_name <- paste0(obj_name, ".pdf")
  file_path <- here::here("figures", file_name)
  ggsave(
    filename = file_path,
    plot     = x,
    width    = width,
    height   = height,
    units    = "cm",
    dpi      = 300,
    device   = grDevices::cairo_pdf
  )
  message(">>> Saved to: ", file_path)
}
