librarian::shelf(dplyr, tidyr)

arg_or_default <- function(args, i, default) {
  if (length(args) >= i && nzchar(args[[i]])) args[[i]] else default
}

arg_or_first_existing <- function(args, i, candidates) {
  if (length(args) >= i && nzchar(args[[i]])) {
    return(args[[i]])
  }
  hit <- candidates[file.exists(candidates)]
  if (length(hit)) {
    return(hit[[1]])
  }
  candidates[[1]]
}

read_csv_clean <- function(path) {
  x <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  blank_names <- is.na(names(x)) | names(x) == ""
  names(x)[blank_names] <- paste0("source_col_", seq_len(sum(blank_names)))
  names(x) <- make.unique(names(x))
  x
}

normalize_site_key <- function(x) {
  x <- trimws(as.character(x))
  x <- tolower(x)
  x[x %in% c("", "na")] <- NA_character_
  x
}

normalize_stream_key <- function(x) {
  x <- normalize_site_key(x)
  x[x %in% c("mg_weir", "mgweir")] <- "mgweir"
  x[x %in% c("or_low", "orlow")] <- "orlow"
  x[x %in% c("sopchoppy river", "sopchoppy river ")] <- "sopchoppy river"
  x[x %in% c("east fork")] <- "east fork"
  x[x %in% c("west fork")] <- "west fork"
  x
}

clean_class_name <- function(x) {
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  x
}

first_existing_col <- function(x, candidates) {
  found <- intersect(candidates, names(x))
  if (length(found) == 0) return(rep(NA_character_, nrow(x)))
  as.character(x[[found[[1]]]])
}

box_root <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn"
esom_dir <- file.path(box_root, "esom", "spatial-data")

args <- commandArgs(trailingOnly = TRUE)
esom_file <- arg_or_first_existing(
  args,
  1,
  c(
    file.path(esom_dir, "esom_final_combined_spatial_data_20260523.csv"),
    file.path(esom_dir, "ESOM_final_combined_spatial_data_20260523.csv")
  )
)
gee_file <- arg_or_default(
  args,
  2,
  file.path(
    box_root,
    "spatial-data-extractions",
    "gee-glc-lulc-outputs",
    "merged-master-checkpoints",
    "DSi_LULC_filled_interpolated_Simple_06252026_V2.csv"
  )
)
out_file <- arg_or_default(args, 3, file.path(esom_dir, "esom_final_combined_spatial_data_20260629.csv"))
missing_file <- arg_or_default(
  args,
  4,
  file.path(esom_dir, "esom_gee_glc_missing_sites_20260629.csv")
)
summary_file <- arg_or_default(
  args,
  5,
  file.path(esom_dir, "esom_final_combined_spatial_data_summary_20260629.csv")
)

esom <- read_csv_clean(esom_file)
gee <- read_csv_clean(gee_file)

required_gee_cols <- c("Stream_Name", "Year", "Simple_Class", "LandClass_sum")
if (!all(required_gee_cols %in% names(gee))) {
  stop("GEE/GLC file must contain Stream_Name, Year, Simple_Class, and LandClass_sum.")
}

old_land_cols <- grep(
  "^(major_land$|land_|gee_glc_|gee_lulc_|glc_|gee_glc_match$|gee_glc_match_method$)",
  names(esom),
  value = TRUE
)
esom_base <- esom[, setdiff(names(esom), old_land_cols), drop = FALSE]

gee_long <- gee %>%
  transmute(
    gee_stream_raw = as.character(Stream_Name),
    .gee_stream_key = normalize_stream_key(Stream_Name),
    Year = as.integer(Year),
    Simple_Class = clean_class_name(Simple_Class),
    LandClass_sum = as.numeric(LandClass_sum)
  ) %>%
  filter(!is.na(.gee_stream_key), nzchar(.gee_stream_key), !is.na(Year), nzchar(Simple_Class))

gee_stream_variants <- gee_long %>%
  distinct(.gee_stream_key, gee_stream_raw)

duplicate_gee_keys <- gee_stream_variants %>%
  count(.gee_stream_key, name = "n_stream_name_variants") %>%
  filter(n_stream_name_variants > 1)

unique_gee_keys <- setdiff(
  unique(gee_stream_variants$.gee_stream_key),
  duplicate_gee_keys$.gee_stream_key
)

gee_exact <- gee_long %>%
  mutate(.gee_match_id = paste0("exact:", gee_stream_raw))

gee_keyed <- gee_long %>%
  filter(.gee_stream_key %in% unique_gee_keys) %>%
  mutate(.gee_match_id = paste0("key:", .gee_stream_key))

gee_wide <- bind_rows(gee_exact, gee_keyed) %>%
  group_by(.gee_match_id, Year, Simple_Class) %>%
  summarize(
    LandClass_sum = if (all(is.na(LandClass_sum))) NA_real_ else mean(LandClass_sum, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(.gee_col = paste("gee_glc", Year, Simple_Class, sep = "_")) %>%
  select(.gee_match_id, .gee_col, LandClass_sum) %>%
  pivot_wider(names_from = .gee_col, values_from = LandClass_sum)

esom_stream <- first_existing_col(esom_base, c("ESOM_Stream_Name", "Stream_Name"))
esom_keyed <- esom_base %>%
  mutate(
    .gee_stream_raw = esom_stream,
    .gee_stream_key = normalize_stream_key(.gee_stream_raw),
    gee_glc_match_method = case_when(
      .gee_stream_raw %in% gee_long$gee_stream_raw ~ "exact stream name",
      .gee_stream_key %in% unique_gee_keys ~ "normalized stream name",
      TRUE ~ "no GEE/GLC match"
    ),
    .gee_match_id = case_when(
      gee_glc_match_method == "exact stream name" ~ paste0("exact:", .gee_stream_raw),
      gee_glc_match_method == "normalized stream name" ~ paste0("key:", .gee_stream_key),
      TRUE ~ NA_character_
    )
  )

out <- left_join(esom_keyed, gee_wide, by = ".gee_match_id", na_matches = "never") %>%
  mutate(gee_glc_match = gee_glc_match_method != "no GEE/GLC match") %>%
  select(-.gee_stream_raw, -.gee_stream_key, -.gee_match_id)

missing_src <- out[!out$gee_glc_match, , drop = FALSE]
missing <- data.frame(
  LTER = first_existing_col(missing_src, c("ESOM_LTER", "LTER")),
  Stream_Name = first_existing_col(missing_src, c("ESOM_Stream_Name", "Stream_Name")),
  Shapefile_Name = first_existing_col(missing_src, c("Shapefile_Name", "shp_nm")),
  reason_missing = "missing GEE/GLC stream-name match",
  stringsAsFactors = FALSE
) %>%
  arrange(LTER, Stream_Name)

dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(missing_file), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(summary_file), recursive = TRUE, showWarnings = FALSE)

write.csv(out, out_file, row.names = FALSE, na = "")
write.csv(missing, missing_file, row.names = FALSE, na = "")

summary_row <- data.frame(
  esom_rows = nrow(out),
  old_land_columns_removed = length(old_land_cols),
  gee_glc_columns_added = ncol(gee_wide) - 1,
  gee_glc_matched_sites = sum(out$gee_glc_match),
  gee_glc_missing_sites = sum(!out$gee_glc_match),
  duplicate_normalized_gee_stream_keys = nrow(duplicate_gee_keys),
  stringsAsFactors = FALSE
)

if (file.exists(summary_file)) {
  summary_existing <- read_csv_clean(summary_file)
  summary_existing <- summary_existing[, setdiff(names(summary_existing), names(summary_row)), drop = FALSE]
  summary_out <- bind_cols(summary_existing, summary_row)
} else {
  summary_out <- summary_row
}
write.csv(summary_out, summary_file, row.names = FALSE, na = "")

cat("WROTE:", out_file, "\n", sep = "")
cat("WROTE:", missing_file, "\n", sep = "")
cat("WROTE:", summary_file, "\n", sep = "")
cat("esom_rows=", nrow(out), "\n", sep = "")
cat("old_land_columns_removed=", length(old_land_cols), "\n", sep = "")
cat("gee_glc_columns_added=", ncol(gee_wide) - 1, "\n", sep = "")
cat("gee_glc_missing_sites=", sum(!out$gee_glc_match), "\n", sep = "")
cat("duplicate_normalized_gee_stream_keys=", nrow(duplicate_gee_keys), "\n", sep = "")
