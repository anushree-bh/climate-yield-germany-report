# Climate Conditions and Winter Wheat Yields in Germany (2000–2024)

A district-level panel analysis of how growing-season temperature and precipitation are associated with winter wheat yields across German districts.

**Read the report online:** https://anushree-bh.github.io/climate-yield-germany-report/

## Summary

- 8,148 district-year observations across 366 districts and 25 harvest years
- Growing season defined as October of the previous year to July of the harvest year
- Fixed-effects regression (district and year fixed effects), with standard errors clustered by district
- Both temperature and precipitation show non-linear (inverted-U) associations with yields, with estimated turning points at about 8.33 °C and about 502 mm

The results are preliminary and describe observational associations, not causal effects.

## Repository contents

| Path | Description |
|---|---|
| `crop_data.R` | Main analysis script: data preparation, climate extraction, regressions, figures |
| `climate_2000_2024.csv` / `.rds` | District-level growing-season climate data, 2000–2024 |
| `districts_397.gpkg` | District boundaries used for spatial aggregation |
| `data/ww_yield.csv` | Winter wheat yield panel |
| `regression_results.docx`, `table2_sensitivity.docx` | Regression tables |
| `*.png`, `output/` | Figures |
| `docs/` | Report website (GitHub Pages) and the full report as a PDF |

## Data sources

Some large raw input files are not included in this repository:

- **Crop yield data** (`Final_data_2024.csv`): Offermann & Duden (2025), *Crop yields and area in Germany at a harmonized district-level from 1979: Update to 2024*, OpenAgrar. https://doi.org/10.3220/253-2025-244
- **Climate data** (`*.nc`): DWD HYRAS-DE v6-1 monthly temperature and precipitation grids, downloaded automatically by `crop_data.R` from https://opendata.dwd.de/climate_environment/CDC/grids_germany/monthly/hyras_de/
- **Administrative boundaries** (`vg250_ebenen_0101/`): VG250 dataset from the German Federal Agency for Cartography and Geodesy (BKG)

## Running the analysis

Requires R with these packages:

```r
install.packages(c("tidyverse", "dplyr", "tidyr", "ggplot2", "sf", "terra",
                   "exactextractr", "ncdf4", "fixest", "modelsummary"))
```

Place `Final_data_2024.csv` and the `vg250_ebenen_0101/` folder in the project root, then run `crop_data.R`.
