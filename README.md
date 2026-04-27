# ESOM

This repo is for ESOM site-list checks and comparisons.

It is separate from `spatial-data`.

## Scope
- maintain ESOM site lists
- compare ESOM sites to finalized spatial outputs
- keep ESOM-specific QA separate from spatial harmonization

## Expected Inputs
- ESOM site list CSVs
- finalized spatial dataset from `spatial-data`
- naming key file when needed

## Initial Structure
- `01_site_lists/`: ESOM site-list inputs and notes
- `02_comparisons/`: saved comparison outputs or review files
- `scripts/`: reusable comparison scripts

## First Script
```bash
Rscript scripts/01_compare_esom_to_finalized_spatial.R \
  --spatial "/path/to/all-data_si-extract_finalized_spatial_YYYYMMDD.csv" \
  --esom "/path/to/ESOM_Sites.csv" \
  --naming-key "/path/to/lter_aliases.csv" \
  --outdir "/path/to/esom_compare"
```

Outputs:
- `esom_sites_name_matched.csv`
- `esom_sites_not_in_spatial.csv`
- `spatial_sites_not_in_esom.csv`
- `esom_spatial_compare_summary.csv`
