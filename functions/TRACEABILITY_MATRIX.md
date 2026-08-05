# Function traceability matrix

Maps which R (and Python) files in `functions/` are sourced/imported by each analysis
and pipeline script. An `x` indicates the file is loaded via `source()` (or, for
`glm.py`, Python `import`) in that script. The `dir` column indicates where the
function is used: `analysis` = `analysis_and_plots/` only, `pipeline` = `pipeline/`
only, `both` = both directories.

Rebuilt from scratch by grepping every current script's `source()`/`import` calls
(the previous version of this file had drifted out of sync with several file renames
and no longer matched the repo).

Last updated: 2026-08-01

| R file | dir | fig01 | fig02_sfig04 | fig03 | fig04_sfig05 | fig05 | fig06 | fig07 | fig08 | sfig01 | sfig07 | sfig08 | sfig09 | sfig10 | 01_preprocess | 02_aggregate | 03_glm_inputs | 04_run_glm |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| params.R | both | x | x | x | x | x | x | x | x | x | x | x | x | x | x | x | x | |
| theme.R | both | x | x | x | x | x | x | x | x | x | x | x | x | x | x | x | x | |
| general.R | both | x | x | x | x | x | x | x | x | x | x | x | x | x | x | x | x | |
| r_general.R | both | x | x | x | x | x | x | x | x | x | x | x | x | x | x | x | x | |
| stats.R | both | x | x | x | x | x | x | x | x | x | x | x | x | x | x | | | |
| analysis_peth.R | both | x | x | x | x | x | | x | x | x | x | x | x | x | x | x | | |
| r_plots.R | analysis | x | x | x | x | x | x | x | x | x | x | x | x | x | | | | |
| pca.R | analysis | x | x | x | x | x | | x | | x | x | x | x | x | | | | |
| plotting_placements.R | analysis | | x | x | x | x | x | x | | x | x | x | x | x | | | | |
| compile_data.R | both | | | | | | | | | | | | | x | x | | | |
| r_head_fixed_processing.R | both | | | | | | | | | | | | | x | x | | | |
| get_null.R | pipeline | | | | | | | | | | | | | | | | x | |
| glm_generic.R | pipeline | | | | | | | | | | | | | | | | x | |
| pred_matrix.R | pipeline | | | | | | | | | | | | | | | | x | |
| signal_matrix.R | pipeline | | | | | | | | | | | | | | | | x | |
| glm.py | pipeline | | | | | | | | | | | | | | | | | x |

## Notes

- `compile_data.R` and `r_head_fixed_processing.R` are used only by `sfig10.Rmd`
  (electric tail shock — needs raw arduino/TDT preprocessing, unlike the other
  supplementary figures which read pre-computed CSVs) and by
  `01_preprocess_sessions.Rmd`.
- `get_null.R`, `glm_generic.R`, `pred_matrix.R`, `signal_matrix.R` are used
  exclusively by `03_generate_glm_inputs.Rmd` (GLM decoding pipeline).
- `glm.py` is not `source()`d from an Rmd — `04_run_glm.py` adds `functions/` to
  `sys.path` and imports it directly (`from glm import ...`). It is called via
  `subprocess` from `03_generate_glm_inputs.Rmd`, which is why the two pipeline
  scripts are listed as separate columns here despite being one execution step.
- `fig06.Rmd` does not source `pca.R` or `analysis_peth.R`; `fig08.Rmd` does not
  source `pca.R` (but does source `analysis_peth.R`) — neither figure needs the
  PCA helpers.
- `fig01.Rmd` and `fig08.Rmd` do not source `plotting_placements.R` — neither has
  fiber-placement panels.
- Every script's own `source()` list is the ground truth here; no transitive
  dependencies between files in `functions/` (e.g. one function file calling into
  another) are tracked — this matrix only reflects direct sourcing from
  `analysis_and_plots/`, `pipeline/`, and `04_run_glm.py`.
