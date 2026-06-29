# esom

This repo is for esom site-list checks and comparisons.

It is separate from `spatial-qaqc`.

## Scope
- maintain esom site lists
- compare esom sites to finalized spatial outputs
- keep esom-specific QA separate from spatial harmonization

## Expected Inputs
- esom site list CSVs
- finalized spatial dataset from `spatial-qaqc`
- naming key file when needed

## Initial Structure
- `01_site_lists/`: esom site-list inputs and notes
- `02_comparisons/`: saved comparison outputs or review files
- `scripts/`: reusable comparison scripts

## First Script
```bash
Rscript scripts/01_compare_esom_to_finalized_spatial.R \
  --spatial "/path/to/all-data_si-extract_finalized_spatial_YYYYMMDD.csv" \
  --esom "/path/to/esom_sites.csv" \
  --naming-key "/path/to/lter_aliases.csv" \
  --outdir "/path/to/esom_compare"
```

Outputs:
- `esom_sites_name_matched.csv`
- `esom_sites_not_in_spatial.csv`
- `spatial_sites_not_in_esom.csv`
- `esom_spatial_compare_summary.csv`
