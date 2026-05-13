# Journal Repository Restructure Plan

This document records the approved clean, publication-oriented structure for the DEBIAS_MX repository. The restructure was implemented on the `codex-journal-restructure` branch after review with the project owner.

## Goals

- Make the repository understandable to journal reviewers and readers from the first page.
- Support reproduction of the main computational results from municipal-level input data.
- Keep raw, processed, code, results, manuscript assets, and documentation clearly separated.
- Keep historical exploratory material out of the journal-facing repository.
- Avoid presenting the repository as a formal R package, while preserving the two local R package/code modules that implement the analysis.
- Keep documentation in English.

## Proposed Top-Level Structure

```text
DEBIAS_MX/
  README.md
  LICENSE
  CITATION.cff
  data-availability.md
  RESTRUCTURE_PLAN.md
  data/
    raw/
      census/
      active_population/
      spatial/
    processed/
      measure_bias/
      explain_bias/
    labels/
  code/
    01_measure_bias/
    02_explain_bias/
    03_figures_tables/
    style/
  src/
    MeasureBiasV.0.0/
    ExplainBiasV.0.0/
  results/
    measure_bias/
    explain_bias/
      holdout/
      blocked_cv/
  manuscript/
    figures/
    supplementary-figures/
  docs/
    variable_dictionary/
    methods/
    project_notes/
  archive/
    README.md
```

## Current Folder Mapping

| Current path | Proposed destination | Action | Notes |
| --- | --- | --- | --- |
| `01.Measure_Bias/Inputs/01.Census2020_Mun_Población.csv` | `data/raw/census/census2020_municipal_population.csv` | Move/rename | Raw municipal population input. |
| `01.Measure_Bias/Inputs/Active_population_fb_tts.csv` | `data/raw/active_population/facebook_tts.csv` | Move/rename | Aggregated municipal active population. |
| `01.Measure_Bias/Inputs/Active_population_fb_stt.csv` | `data/raw/active_population/facebook_stt.csv` | Move/rename | Aggregated municipal active population. |
| `01.Measure_Bias/Inputs/Active_population_phone_00_05.csv` | `data/raw/active_population/phone_00_05.csv` | Move/rename | Aggregated municipal active population. |
| `01.Measure_Bias/Inputs/Active_population_phone_00_08.csv` | `data/raw/active_population/phone_00_08.csv` | Move/rename | Aggregated municipal active population. |
| `01.Measure_Bias/Inputs/Active_population_phone_01_01.csv` | `data/raw/active_population/phone_01_01.csv` | Move/rename | Aggregated municipal active population. |
| `01.Measure_Bias/Inputs/SHP/` | `data/raw/spatial/municipalities/` | Move/rename | Municipal shapefile used by measure-bias stage. |
| `SHP/` | `data/raw/spatial/gadm/` | Move/rename | External/admin spatial boundary files. |
| `Final_Results_BlockedCV/Inputs/` | `data/processed/explain_bias/blocked_cv/` | Move/rename | Model-ready inputs for blocked CV. |
| `Final_Results_Hold-Out/Inputs/` | `data/processed/explain_bias/holdout/` | Move/rename | Model-ready inputs for holdout validation. |
| `02.Explain_Bias/Inputs/` | `data/processed/explain_bias/legacy_inputs/` | Move/rename | Earlier explain-bias inputs; keep if still referenced. |
| `01.Measure_Bias/01_Measure_Bias_Full.qmd` | `code/01_measure_bias/measure_bias_full.qmd` | Move/rename/update paths | Main measure-bias workflow. |
| `Final_Results_BlockedCV/*.qmd` | `code/02_explain_bias/` | Move/rename/update paths | Main blocked-CV explain-bias workflows. |
| `Final_Results_Hold-Out/*.qmd` | `code/02_explain_bias/` | Move/rename/update paths | Main holdout explain-bias workflow. |
| `01.Measure_Bias/style/` | `code/style/` | Move/update paths | Shared plotting theme. |
| `01.Measure_Bias/MeasureBiasV.0.0/` | `src/MeasureBiasV.0.0/` | Move | Reusable R source module. |
| `02.Explain_Bias/ExplainBiasV.0.0/` | `src/ExplainBiasV.0.0/` | Move | Reusable R source module. |
| `01.Measure_Bias/Measure_Bias_*` | `results/measure_bias/` | Move/rename | Outputs from bias measurement runs. |
| `Measure Bias/` | `results/measure_bias/diagnostics/` | Move/rename | KNN diagnostics/analysis outputs. |
| `Final_Results_BlockedCV/Final_Results_BlockedCV_*` | `results/explain_bias/blocked_cv/` | Move/rename | Final blocked-CV model outputs, SHAP outputs, maps, predictions. |
| `Final_Results_Hold-Out/Final_Results_HoldOut_*` | `results/explain_bias/holdout/` | Move/rename | Final holdout model outputs, SHAP outputs, maps, predictions. |
| `Figures/` | `results/figures_legacy/` or `manuscript/figures/legacy/` | Review | Appears to contain selected PDF figures; decide whether paper-facing. |
| `manuscript/figures/` | `manuscript/figures/` | Keep/reorganize lightly | Contains figure code, data, plots, and visualisations. |
| `manuscript/supplementary-figures/` | `manuscript/supplementary-figures/` | Keep | Supplementary paper assets. |
| `Variables.docx` | `docs/variable_dictionary/variables.docx` | Move/rename | Variable documentation. |
| `01_table-variable-dictionary.pdf` | `docs/variable_dictionary/table_variable_dictionary.pdf` | Move/rename | Reference variable dictionary. |
| `Spatial_Cross_Validation.pdf` | `docs/methods/spatial_cross_validation.pdf` | Move/rename | Methods reference. |
| `hyperparameters.docx` | `docs/methods/hyperparameters.docx` | Move/rename | Hyperparameter documentation. |
| `Results Tables.docx` | `manuscript/tables/results_tables.docx` | Move/rename | Manually assembled paper table artifact. |
| `Results Tables.pptx` | `manuscript/tables/results_tables.pptx` | Move/rename | Manually assembled paper table artifact. |
| `Old Results/` | Exclude from journal-facing repo | Remove from branch or move outside repo | User preference: keep out. |
| `.DS_Store` files | Exclude | Delete from branch and add to `.gitignore` | macOS metadata, not useful for publication. |

## Duplicated Large Files

The repository currently contains many repeated copies of the same or similar shapefiles inside result folders. Several individual shapefiles are about 55 MB, and the repository is about 5 GB total.

For a GitHub-only publication repository, the recommended approach is:

- Keep one canonical copy of each spatial dataset in `data/raw/spatial/`.
- Remove duplicated shapefile copies from individual `results/**/Inputs/` folders.
- Update scripts and documentation so they read spatial data from the canonical location.
- Keep final CSV, PDF, PNG, and model output artifacts needed to inspect or reproduce reported outputs.

This keeps the repository complete while making it less repetitive.

## Proposed README Contents

The new `README.md` should include:

1. Project title and short summary.
2. Repository structure.
3. Data availability statement.
4. Software requirements.
5. Reproducibility workflow:
   - Step 1: measure active-population bias from raw municipal inputs.
   - Step 2: prepare explain-bias inputs.
   - Step 3: run holdout and blocked-CV explain-bias models.
   - Step 4: assemble figures and tables.
6. Explanation of what is manually assembled.
7. Notes on municipal aggregation and privacy.
8. Citation instructions.
9. Contact/maintainer information placeholder.

## Proposed Reproducibility Workflow

```text
data/raw/
  -> code/01_measure_bias/measure_bias_full.qmd
  -> results/measure_bias/
  -> data/processed/explain_bias/
  -> code/02_explain_bias/*
  -> results/explain_bias/
  -> code/03_figures_tables/ and manuscript/figures/code/
  -> manuscript/figures/ and manuscript/tables/
```

The README should be explicit that final figures and tables were manually assembled from the computational outputs, while the scripts and outputs used for assembly are included.

## Documentation Files To Add

- `README.md`: main public-facing guide.
- `data-availability.md`: journal-ready data/code availability statement.
- `LICENSE`: placeholder until the project owner selects a license.
- `CITATION.cff`: citation metadata placeholder.
- `.gitignore`: ignore `.DS_Store`, rendered temporary files, and local environment files.
- `archive/README.md`: explains that historical exploratory results are not included in the journal-facing repository.

## Decisions Applied

1. Code is licensed under MIT.
2. Data, documentation, figures, and other non-code materials are licensed under CC BY 4.0 unless otherwise noted.
3. `Figures/` was treated as output plots, not final manuscript figures, and moved to `results/plots/legacy_outputs/`.
4. `.rds` model objects were retained in `results/`.
5. Duplicated result-level `Inputs/` folders were removed after canonical data copies were placed under `data/raw/` and `data/processed/`.
6. `Old Results/` was excluded from the journal-facing branch.
7. The paper title used in public documentation is: "One country, multiple portraits: representativeness in GPS-based mobility data is source-specific and spatially dependent".

## Implementation Summary

1. Added `.gitignore`.
2. Created the new directory structure.
3. Moved raw data, processed data, workflows, reusable source modules, results, manuscript assets, and docs.
4. Removed `.DS_Store` files.
5. Excluded `Old Results/` from the branch.
6. Updated main QMD script paths to match the new layout.
7. Wrote `README.md`, `data-availability.md`, `archive/README.md`, `CITATION.cff`, `LICENSE`, and `LICENSE-DATA.md`.
8. Ran lightweight checks to verify key documented paths and cleanup decisions.
