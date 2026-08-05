# multi-spout brief access fiber photometry — analysis repository

Reproduction repository for:

> **Gordon et al. (2026)** — *Lateral hypothalamic control of a spatially organized striatal dopamine landscape during consummatory behavior*

This repository contains all analysis code and pre-computed data files needed
to reproduce the figures and statistics in the paper. Raw fiber photometry data
and full session outputs are archived separately on Zenodo:
https://doi.org/10.5281/zenodo.21809309

---

## repository contents

| destination | what is included |
|-------------|-----------------|
| **Zenodo** ([10.5281/zenodo.21809309](https://doi.org/10.5281/zenodo.21809309)) | all code, pre-computed CSVs, full session data, GLM outputs |
| **GitHub** | all code and pre-computed result CSVs; no raw session data or PDF figure outputs |

---

## requirements

### R
Tested with **R 4.3.3**. Required packages are listed in
`coding_environments/r_package_versions_2026-06-18.txt`.
Key packages: `tidyverse`, `afex`, `emmeans`, `janitor`, `readxl`, `ggh4x`,
`arrow`, `zoo`, `lubridate`, `patchwork`, `ggbeeswarm`, `ggpubr`, `pals`,
`boot`, `corrplot`, `Hmisc`, `plotly`, `openxlsx`, `lme4`.

### Python
Tested with **Python 3.11** (Anaconda 2024.02). The conda environment
specification is in `coding_environments/environment_base.yml`.

To create the environment:
```bash
conda env create -f coding_environments/environment_base.yml
```

The path to the Python executable must be set in
`logs_and_keys/key_local_directories.R` (see [pipeline scripts](#pipeline-scripts-pipeline)
below).

---

## reproducibility

Scripts operate at two levels depending on the available data:

**Level 1 — figure scripts from pre-computed CSVs** (no raw data required)

All figure scripts in `analysis_and_plots/` include a `toggle_process` flag.
With `toggle_process = 0` (the default), scripts read pre-computed CSVs from
their `pre/` subdirectory and from `data/`. This mode reproduces all figures
without needing raw session data.

Pre-computed CSVs are included in both the Zenodo archive and the GitHub
repository.

**Level 2 — full pipeline from raw session data** (requires Zenodo archive)

With `toggle_process = 1`, figure scripts reprocess data from `data/sessions/`
and `data/sessions_combined_subject/`. These directories contain the full
preprocessed session outputs and are available in the Zenodo archive only.

Running `pipeline/01_preprocess_sessions.Rmd` through `pipeline/04_run_glm.py`
regenerates all session-level outputs from raw fiber photometry and behavioral
data. This additionally requires:
- `logs_and_keys/key_local_directories.R` — a local file (not included)
  defining paths to the raw data server and the Python executable.
  See the template below.

**`key_local_directories.R` template:**
```r
path_python      <- "/path/to/conda/envs/env_name/bin/python"
dir_tdt          <- "/path/to/tdt/data/"
dir_npm          <- "/path/to/npm/data/"
dir_arduino_raw  <- "/path/to/arduino/raw/"
```

---

## repository structure

```
multi_spout_brief_access_fp_revision_repo/
├── pipeline/                  # ordered preprocessing pipeline scripts
│   ├── 01_preprocess_sessions.Rmd
│   ├── 02_aggregate.Rmd
│   ├── 03_generate_glm_inputs.Rmd
│   └── 04_run_glm.py
├── analysis_and_plots/        # figure scripts (one per figure or figure group)
│   ├── fig01.Rmd
│   ├── fig02_sfig04.Rmd
│   ├── fig03.Rmd
│   ├── fig04_sfig05.Rmd
│   ├── fig05.Rmd
│   ├── fig06.Rmd
│   ├── fig07.Rmd
│   ├── fig08.Rmd
│   ├── sfig01.Rmd
│   ├── sfig07.Rmd
│   ├── sfig08.Rmd
│   ├── sfig09.Rmd
│   └── sfig10.Rmd
├── stats_compilation/         # compiled statistics workbook
│   ├── stats_compiler.R       # compiles all panel stats into a single xlsx
│   └── stats_compiled.xlsx    # output: one sheet per panel, index sheet first
├── functions/                 # R and Python utility functions
│   ├── TRACEABILITY_MATRIX.md # maps which functions are sourced by which scripts
│   └── ...
├── logs_and_keys/             # session logs, hardware keys, inclusion flags
├── data/                      # analysis data
│   ├── arduino/               # behavioral data from Arduino serial output
│   ├── sessions/              # per-session preprocessed FP and behavioral data
│   ├── combined_setid/        # aggregated cross-session outputs (pipeline/02)
│   ├── sessions_combined_subject/  # subject-level combined outputs (pipeline/03)
│   ├── glm_null_distributions/     # GLM permutation null distributions (pipeline/03–04)
│   ├── fig01h/                # pre-computed: LHA baseline-subtracted representative traces
│   ├── fig02_sfig04/          # pre-computed: opto stimulation and CNO inhibition FP data
│   │   ├── stimulation/
│   │   ├── inhibition/
│   │   └── interaction/
│   ├── fig03/                 # pre-computed: cross-regional striatal DA release data
│   ├── fig04_sfig05/          # pre-computed: opto inhibition FP and licking data
│   ├── fig08/                 # pre-computed: VTA ChR2 opto behavior summaries
│   ├── sfig01/                # pre-computed: LHA dual-color validation traces
│   └── sfig10/                # pre-computed: extended time-scale LHA FP data
└── coding_environments/       # reproducibility: package version snapshots and conda env
    ├── environment_base.yml   # conda environment specification (Python)
    ├── r_package_versions_*.txt  # R package version snapshot
    └── report_r_package_versions.R  # script used to generate the snapshot
```

---

## pipeline scripts (`pipeline/`)

Scripts are numbered in execution order. Each sources functions from `./functions/`
and reads logs from `./logs_and_keys/`. Local paths are defined in
`logs_and_keys/key_local_directories.R` (not included — see template above).

### `01_preprocess_sessions.Rmd`

Preprocesses raw behavioral and fiber photometry data into per-session
analysis-ready files. Run once per session; `overwrite = 0` skips already
processed sessions.

**steps (in order):**

| step | function | description |
|------|----------|-------------|
| 1 | `import_arduino()` | copy raw arduino `.csv` files from network server to local `data/arduino/raw/` |
| 2a | `extract_serial_output()` | parse arduino serial output into event / param / param_dynamic tables → `data/arduino/extracted/` |
| 2b | `process_multi_spout()` | compute trial-level lick summaries and binned counts → `data/arduino/processed/` |
| 3a | `locally_save_multispout_data_behavior()` | copy and trim behavioral files into per-session folders in `data/sessions/` |
| 3b | `locally_save_multispout_data_fp_peth()` | copy raw peri-event FP feather files into per-session folders |
| 3c | `locally_save_multispout_data_fp_baseline()` | copy and trim pre-task baseline FP streams into per-session folders |
| 4a | `multispout_preprocess_fp_peth()` | assign brain-region IDs, compute z-scores, save `*_streams_peth_preprocessed.feather` |
| 4b | `multispout_preprocess_fp_baseline()` | z-score baseline streams and compute lha_ratio; required for quality control |
| 5 | `summarise_sessions()` | compute per-session peth and trace summary statistics |
| 6 | `get_signal_quality_control()` | flag sessions / regions with poor signal quality |
| 7a | *(inline)* | compute STR inter-regional correlation matrices → `data/sessions_combined_subject/` |
| 7b | *(inline)* | compute LHA/STR inter-regional correlation matrices → `data/sessions_combined_subject/` |

### `02_aggregate.Rmd`

Aggregates preprocessed session data into combined output files for analysis
and figure scripts. Loops over set_ids to limit memory usage.

**outputs (written to `data/combined_setid/`):**

| file pattern | description |
|---|---|
| `{set_id}_full_peth_summary_trace_overall.csv` | population-level mean peth trace, all trials |
| `{set_id}_full_peth_summary_trace_subject.csv` | subject-level mean peth trace |
| `{set_id}_full_peth_summary_trace_triallick.csv` | peth trace split by trial_lick flag |
| `{set_id}_full_peth_summary_means_subject.csv` | subject-level binned means |
| `{set_id}_full_peth_summary_means_trial.csv` | trial-level binned means |
| `{set_id}_full_peth_summary_means_binnedtrial.csv` | means grouped by blocks of 10 trials |
| `{set_id}_filtlick_*` | same outputs restricted to trials with licking |
| `licking_spout_summary.csv` | per-subject mean lick count and lick proportion by solution |
| `licking_trial_summary.csv` | per-trial lick data across all sessions |
| `licking_binnedlick_trace_overall.csv` | population-level binned lick rate trace |
| `licking_binnedlick_trace_subject.csv` | subject-level binned lick rate trace |

### `03_generate_glm_inputs.Rmd`

Builds signal and predictor matrices per session for GLM decoding analysis.
Reads preprocessed feather files from `data/sessions/` and writes per-subject
combined outputs to `data/sessions_combined_subject/` and GLM inputs there.
Also writes `data/key_blockname_region_filtered.csv` — a session × region
inclusion index used by `pipeline/04_run_glm.py`.
Also triggers `pipeline/04_run_glm.py` via subprocess.

### `04_run_glm.py`

Python script called from `03_generate_glm_inputs.Rmd` via subprocess. Runs
cross-validated OLS regression on the signal and predictor matrices to generate
GLM coefficient estimates and permutation null distributions.

---

## functions (`functions/`)

See `functions/TRACEABILITY_MATRIX.md` for a full map of which function files are
sourced by each pipeline and figure script.

### parameters and theming

| file | contents |
|------|----------|
| `params.R` | shared lookup tables (`key_regions`, `key_solutions`, `key_set_id`), color palettes, signal IDs, formatting helpers (`format_solutions`, `format_pc`) |
| `theme.R` | ggplot2 themes (`theme_ag01`, `theme_ag_raster`), plot utilities (`save_pdf`, `remove_x_all`, `geom_hpline`), statistical output helpers (`quick_sig`, `save_aov`, `save_ttest`) |
| `factor_solution.csv` | ordered factor levels for solution type (read by `sfig09.Rmd`) |

### general utilities

| file | contents |
|------|----------|
| `general.R` | session I/O (`read_session`), region label mapping (`get_region_labels`, `edit_regions`, `format_regions`), solution value conversion (`get_solution_value`), set_id derivation (`get_set_id`), placement and QC filters (`filter_placements`, `filter_qc_metrics`) |
| `r_general.R` | file/directory utilities (`dir_diff`, `format_dir`), data combining (`combined_import`, `combine_files`, `coerce_character`), event extraction (`get_event_bouts`, `get_peri_event`, `get_trial_lick_summary`), peak detection (`get_peaks_threshold`) |
| `r_plots.R` | shared plotting helpers used across figure scripts |

### behavioral preprocessing

| file | contents |
|------|----------|
| `r_head_fixed_processing.R` | arduino extraction (`extract_serial_output`, `extract_event`, `extract_param`, `extract_param_dynamic`), multi-spout processing (`process_multi_spout`, `generate_trial_ids_multispout`, `generate_trial_summary_multispout`, `create_solution_value`) |
| `compile_data.R` | high-level pipeline wrappers (`import_arduino`, `locally_save_multispout_data_behavior`, `locally_save_multispout_data_fp_peth`, `locally_save_multispout_data_fp_baseline`, `multispout_preprocess_fp_peth`, `multispout_preprocess_fp_baseline`, `summarise_sessions`, `get_signal_quality_control`) |

### statistical analysis

| file | contents |
|------|----------|
| `stats.R` | bootstrap CIs (`bootstrap_mean`), correlation matrices (`get_cormatrix`, `tidy_cormatrix`, `plt_corrmatrix`), ANOVA wrappers (`aov_rm_one_between`) |
| `analysis_peth.R` | session combining (`combine_session_fp`, `combine_session_files`, `combine_session_binnedlick`, `combine_session_fp_bl`), signal summarisation (`get_mean_signals`, `get_peth_binned_summary`), pairwise bar plots (`plt_bar_pairwise`) |

### multivariate / decoding

| file | contents |
|------|----------|
| `glm_generic.R` | OLS pipeline wrappers (`perform_ols_v02`, `glm_per_fold`) |
| `signal_matrix.R` | batch signal matrix generation (`generate_signal_matrix_batch`) |
| `pred_matrix.R` | prediction matrix utilities |
| `get_null.R` | null distribution generation (`generate_nulls`, `pad_signal_matrix`) |
| `pca.R` | PCA summary helpers (`tidy_pca_summary`) |

### other

| file | contents |
|------|----------|
| `plotting_placements.R` | fiber placement visualizations (`plt_str`, `plt_str_3d`, `calculate_pairwise_distances`) |
| `glm.py` | Python cross-validated OLS regression (called from R via subprocess by `04_run_glm.py`) |

---

## analysis and plots (`analysis_and_plots/`)

Each figure script reads from the data sources listed below. Scripts with
commented-out preprocessing blocks document how pre-computed CSVs were originally
generated from raw data; those blocks require network access and are not intended
to be re-run from this repository.

| script | data source(s) | notes |
|--------|----------------|-------|
| `fig01.Rmd` | `data/sessions/`, `data/combined_setid/`, `data/fig01h/` | fig01h contains pre-computed LHA representative traces |
| `fig02_sfig04.Rmd` | `data/fig02_sfig04/` (stimulation/, inhibition/, interaction/) | pre-computed from opto stimulation characterization and CNO+opto experiments |
| `fig03.Rmd` | `data/fig03/` | pre-computed from cross-regional stimulation characterization |
| `fig04_sfig05.Rmd` | `data/fig04_sfig05/` | pre-computed from opto inhibition experiment |
| `fig05.Rmd` | `data/combined_setid/` | generated by pipeline/02; writes per-panel CSVs to `analysis_and_plots/fig05/` (used by sfig07) |
| `fig06.Rmd` | `data/sessions_combined_subject/` | generated by pipeline/03 |
| `fig07.Rmd` | `data/sessions_combined_subject/` | generated by pipeline/03; writes CSVs to `analysis_and_plots/fig07/` (used by sfig08) |
| `fig08.Rmd` | `data/fig08/` | pre-computed from VTA ChR2 opto behavior experiment |
| `sfig01.Rmd` | `data/sfig01/`, `data/sessions/` | sfig01/ contains pre-computed LHA dual-color validation traces; also reads from sessions/ |
| `sfig07.Rmd` | `analysis_and_plots/fig05/`, `data/sessions/` | depends on fig05.Rmd having been run first |
| `sfig08.Rmd` | `analysis_and_plots/fig07/`, `data/sessions_combined_subject/` | depends on fig07.Rmd having been run first |
| `sfig09.Rmd` | `data/combined_setid/`, `data/sessions/` | generated by pipeline/01 and pipeline/02 |
| `sfig10.Rmd` | `data/sfig10/` | pre-computed extended time-scale LHA FP data |

---

## stats compilation (`stats_compilation/`)

`stats_compiler.R` reads every stats CSV written by the figure scripts and compiles
them into a single Excel workbook, `stats_compiled.xlsx`.

**To regenerate the workbook**, run from the project root:

```r
source('./stats_compilation/stats_compiler.R')
```

Requires the `tidyverse` and `openxlsx` packages.

### workbook structure

The workbook contains 86 sheets. The first sheet (`index`) is an index with one row
per data sheet:

| column | description |
|--------|-------------|
| `figure_id` | figure number (e.g. `fig01`, `sfig07`) |
| `panel_id` | panel letter(s) within the figure |
| `statistical_test` | test type (ANOVA, HSD, Wilcoxon, t-test, Pearson correlation, etc.) |
| `stratified_by` | grouping variables if the test was run separately per group |
| `sheet_name` | name of the corresponding data sheet |

Data sheets are ordered to match figure and panel order:
`fig01 → fig02 → ... → fig08 → sfig01 → sfig04 → sfig05 → sfig07 → ... → sfig10`.
sfig04 and sfig05 panels are embedded in the `fig02_sfig04` and `fig04_sfig05`
scripts but appear in supplemental figure order in the workbook.

### region decoding

For any sheet where a column (`region`, `contrast1_within`, `contrast2_within`)
contains internal region codes (e.g. `nac`, `lha_gaba`), the compiler:

1. Filters rows to the regions listed in `key_regions` (defined in `functions/params.R`).
2. Inserts a `*_label` column immediately after the code column with the
   publication-ready label (e.g. `nac` → `NAcCR`, `lha_gaba` → `LHA:GABA`).

Columns that already contain display labels (e.g. `NAcCR`, `DMS`) are unchanged.

---

## logs and keys (`logs_and_keys/`)

| file | contents |
|------|----------|
| `log_data_analysis.xlsx` | session inclusion/exclusion flags; sheets: `subjects`, `fp`, `experiments` |
| `key_events_arduino.csv` | arduino event ID → character label mapping |
| `log_behavior_headfixed_multispout_sessions.csv` | session-level metadata (subject, date, procedure, cohort) |
| `log_behavior_headfixed_multispout_soutids.csv` | spout → solution mapping per session |
| `log_fiberphotometry_tdt.csv` | TDT recording metadata (fiber IDs, brain regions) |
| `log_fiberphotometry_npm.csv` | NPM recording metadata |
| `log_fiber_placements.csv` | histological fiber placement coordinates and exclusion flags |

These logs are trimmed to the subjects and sessions included in the paper.

---

## data (`data/`)

Data fall into three categories:

**Generated by the pipeline** — produced by running `pipeline/01–04` against raw data.
Requires `logs_and_keys/key_local_directories.R` (not included; see template above).
These directories are populated in the Zenodo archive.

| directory | generated by | description |
|-----------|-------------|-------------|
| `arduino/raw/` | pipeline/01 | local copies of raw arduino CSV files |
| `arduino/extracted/` | pipeline/01 | per-session event / param tables |
| `arduino/processed/` | pipeline/01 | trial-level behavioral summaries |
| `sessions/<blockname>/` | pipeline/01 | per-session preprocessed FP feathers, behavioral CSVs, quality metrics |
| `combined_setid/` | pipeline/02 | aggregated cross-session FP and licking summaries |
| `sessions_combined_subject/` | pipeline/03 | subject-level combined signal/predictor matrices |
| `glm_null_distributions/` | pipeline/03–04 | GLM permutation null distributions |
| `key_blockname_region_filtered.csv` | pipeline/03 | session × region inclusion index for GLM |

**Pre-computed** — summarized CSVs produced from external analysis scripts (not in this
repository). The commented-out preprocessing blocks in each figure script document
the provenance. These cannot be regenerated from this repository alone, but are
included in both the Zenodo archive and GitHub repository.

| directory | experiment | figures |
|-----------|------------|---------|
| `fig01h/` | LHA representative traces | fig01 panel H |
| `fig02_sfig04/` | NPM opto stim char. + CNO inhibition | fig02, sfig04 |
| `fig03/` | NPM cross-regional striatal stim char. | fig03 |
| `fig04_sfig05/` | NPM opto inhibition | fig04, sfig05 |
| `fig08/` | VTA ChR2 opto behavior (brief + free access) | fig08 |
| `sfig01/` | LHA dual-color validation traces | sfig01 |
| `sfig10/` | Extended time-scale LHA FP data | sfig10 |

### per-session output (`data/sessions/<blockname>/`)

| file | description |
|------|-------------|
| `*_streams_peth_preprocessed.feather` | preprocessed peri-event FP signals (z-scored, region-labeled) |
| `*_streams_baseline_preprocessed.feather` | preprocessed baseline FP signals (z-scored, lha_ratio included) |
| `*_quality_metrics.csv` | per-region signal quality flags (`exclude_poor_signal`) |
| `*_data_trial_summary.csv` | per-trial lick counts, latency, ILI |
| `*_data_trial.csv` | per-lick events relative to trial onset |
| `*_data_trial_binned.csv` | lick counts in 100 ms bins relative to trial onset |
| `*_data_spout_summary.csv` | per-spout session averages |
| `*_data_session_binned.csv` | lick counts in bins of trials across the session |
| `*_fp_events.csv` | raw lick event timestamps aligned to FP time base |

---

## cohorts included in the paper

### brief-access main cohorts (full session data in `data/sessions/`)

| cohort | imaging system | targets |
|--------|----------------|---------|
| `aaw` | TDT | LHA (GABA + Glut simultaneous) |
| `abb` | TDT | NAcShL, NAcShM | water restriction, brief access |
| `abo` | NPM | NAcCR, NAcCC, NAcShL, DMS, DLS, TS |
| `acd` | NPM | NAcCR, NAcCC, NAcShL, DMS, DLS, TS |
| `acp` | NPM | NAcCR, NAcCC, NAcShL, DMS, DLS, TS |
| `acr` | NPM | LHA (GABA + Glut simultaneous) NAcCC, DMS, DLS, TS |

note: brief access data included in figures 1, 5, 6, 7 and supplemental figures 4, 7, 8, 9

### cohorts for specific figures (pre-computed data only)

| cohort | imaging system | experiment | figures |
|--------|----------------|------------|---------|
| `acq` | TDT (dual-color) | LHA GABA + Glut dual-color validation | sfig01 |
| `abw` | NPM | Striatal optogenetic stimulation — record across 4 regions while stimulating each in turn (DLS, DMS, NAcCR, TS) | fig03 |
| `aco` | NA | VTA ChR2 optogenetic stimulation, free-access and brief-access licking behavior | fig08 |

### brain regions

| ID | label |
|----|-------|
| `lha_gaba` | LHA:GABA |
| `lha_glut` | LHA:Glut |
| `lha_ratio` | LHA:Ratio |
| `nac` / `nac_corr` | NAcCore central (NAcCR / NAcCC) |
| `nac_shelllat` | NAcShL |
| `nacsh_med` | NAcShM |
| `dms` | DMS |
| `dls` | DLS |
| `ts` | TS |

---

## coding environments (`coding_environments/`)

| file | description |
|------|-------------|
| `environment_base.yml` | conda environment specification for Python dependencies (used by `04_run_glm.py` and `glm.py`) |
| `r_package_versions_*.txt` | snapshot of R package versions at the time of final analysis |
| `report_r_package_versions.R` | script used to generate the package version snapshot |
