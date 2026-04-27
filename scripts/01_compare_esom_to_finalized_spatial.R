args <- commandArgs(trailingOnly = TRUE)

arg_val <- function(flag, default = "") {
  i <- which(args == flag)
  if (!length(i) || i[length(i)] == length(args)) return(default)
  args[i[length(i)] + 1]
}

file_is_nonempty <- function(path) {
  if (!nzchar(path) || !file.exists(path)) return(FALSE)
  info <- file.info(path)
  isTRUE(!is.na(info$size) && info$size > 0)
}

resolve_input_file <- function(user_path = "", candidates = character(), label = "input file") {
  paths <- unique(c(user_path, candidates))
  for (p in paths) {
    if (file_is_nonempty(p)) return(normalizePath(p, mustWork = TRUE))
  }
  attempted <- paths[nzchar(paths)]
  stop("Unable to find a usable ", label, ". Tried: ", paste(attempted, collapse = ", "), call. = FALSE)
}

resolve_output_dir <- function(path, default_path) {
  outdir <- if (nzchar(path)) path else default_path
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(outdir)) stop("Unable to create output directory: ", outdir, call. = FALSE)
  normalizePath(outdir, mustWork = FALSE)
}

norm_chr <- function(x) {
  x <- gsub("\u00A0", " ", as.character(x), fixed = TRUE)
  x <- trimws(x)
  x[x == ""] <- NA_character_
  x
}

load_naming_key <- function(naming_key_file) {
  if (!file.exists(naming_key_file)) {
    return(data.frame(from = character(0), to = character(0), stringsAsFactors = FALSE))
  }
  x <- read.csv(naming_key_file, stringsAsFactors = FALSE, check.names = FALSE)
  x$from <- trimws(as.character(x$from))
  x$to <- trimws(as.character(x$to))
  x
}

norm_lter <- function(x, naming_key = NULL) {
  x <- norm_chr(x)
  if (!is.null(naming_key) && nrow(naming_key) > 0) {
    idx <- match(x, naming_key$from)
    hit <- !is.na(idx)
    x[hit] <- naming_key$to[idx[hit]]
  }
  x
}

harmonize_stream_name <- function(x) {
  x <- norm_chr(x)
  lower_x <- tolower(x)
  x[lower_x %in% c("east fork", "eastfork")] <- "east fork"
  x[lower_x %in% c("west fork", "westfork")] <- "west fork"
  recodes <- c(
    "Amazon River at Obidos" = "Obidos",
    "MGWEIR" = "MG_WEIR",
    "ORlow" = "OR_low",
    "OR_WEIR" = "OR_low"
  )
  idx <- match(x, names(recodes))
  hit <- !is.na(idx)
  x[hit] <- unname(recodes[idx[hit]])
  x
}

first_present_name <- function(df, choices) {
  hit <- choices[choices %in% names(df)]
  if (!length(hit)) return("")
  hit[1]
}

box_root <- Sys.getenv(
  "SILICA_DATA_ROOT",
  "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/spatial_data_extractions"
)

spatial_file <- resolve_input_file(
  arg_val("--spatial"),
  c(file.path(box_root, "harmonization_current", "inclusive_master", "all-data_si-extract_inclusive_master_old_20250325_new_20260323_20260410.csv")),
  label = "finalized spatial dataset"
)

esom_file <- resolve_input_file(
  arg_val("--esom"),
  c(file.path(box_root, "ESOM_Sites.csv")),
  label = "ESOM site list"
)

naming_key_file <- resolve_input_file(
  arg_val("--naming-key"),
  c(file.path(box_root, "lter_aliases.csv")),
  label = "naming key file"
)

outdir <- resolve_output_dir(
  arg_val("--outdir"),
  file.path(box_root, "esom_current", "spatial_compare")
)

naming_key <- load_naming_key(naming_key_file)
spatial <- read.csv(spatial_file, stringsAsFactors = FALSE, check.names = FALSE)
esom <- read.csv(esom_file, stringsAsFactors = FALSE, check.names = FALSE)

esom_lter_col <- first_present_name(esom, c("LTER", "lter"))
esom_stream_col <- first_present_name(esom, c("Stream_Name", "stream_name", "Stream", "stream", "Site", "site", "Site_Name", "site_name"))
if (!nzchar(esom_lter_col) || !nzchar(esom_stream_col)) {
  stop("ESOM site list must contain LTER and Stream_Name-like columns.", call. = FALSE)
}

spatial$LTER_matched <- norm_lter(spatial$LTER, naming_key)
spatial$Stream_matched <- harmonize_stream_name(spatial$Stream_Name)
spatial$site_key <- paste(spatial$LTER_matched, spatial$Stream_matched, sep = "__")
spatial_unique <- unique(spatial[c("LTER", "Stream_Name", "LTER_matched", "Stream_matched", "site_key")])
spatial_unique <- spatial_unique[!is.na(spatial_unique$site_key), , drop = FALSE]

esom$LTER_raw <- esom[[esom_lter_col]]
esom$Stream_raw <- esom[[esom_stream_col]]
esom$LTER_matched <- norm_lter(esom$LTER_raw, naming_key)
esom$Stream_matched <- harmonize_stream_name(esom$Stream_raw)
esom$site_key <- paste(esom$LTER_matched, esom$Stream_matched, sep = "__")
esom_unique <- unique(esom[c("LTER_raw", "Stream_raw", "LTER_matched", "Stream_matched", "site_key")])
esom_unique <- esom_unique[!is.na(esom_unique$site_key), , drop = FALSE]

esom_missing <- esom_unique[!(esom_unique$site_key %in% spatial_unique$site_key), , drop = FALSE]
spatial_missing <- spatial_unique[!(spatial_unique$site_key %in% esom_unique$site_key), , drop = FALSE]
summary_df <- data.frame(
  metric = c("esom_sites_total", "spatial_sites_total", "esom_sites_not_in_spatial", "spatial_sites_not_in_esom"),
  value = c(nrow(esom_unique), nrow(spatial_unique), nrow(esom_missing), nrow(spatial_missing)),
  stringsAsFactors = FALSE
)

write.csv(esom_unique, file.path(outdir, "esom_sites_name_matched.csv"), row.names = FALSE)
write.csv(esom_missing, file.path(outdir, "esom_sites_not_in_spatial.csv"), row.names = FALSE)
write.csv(spatial_missing, file.path(outdir, "spatial_sites_not_in_esom.csv"), row.names = FALSE)
write.csv(summary_df, file.path(outdir, "esom_spatial_compare_summary.csv"), row.names = FALSE)

cat("WROTE:", outdir, "\n")
