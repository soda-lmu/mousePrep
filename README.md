README
================
Theerdha
2026-02-23

<!-- README.md is generated from README.Rmd. Please edit README.Rmd -->

<img src="docs/assets/mouseprep-icon.png" align="right" width="120" />

# mousePrep

`mousePrep` provides a lightweight set of preprocessing utilities for
**mouse movement datasets**. It helps turn raw, mouse logs (often
containing inconsistent column names, mixed event types and device,
timing issues etc) into **clean, standardized, analysis-ready**
trajectory tables.

The package focuses on the practical steps that typically come *before*
feature extraction and modeling: standardizing raw inputs, filtering and
scoping to valid observations, handling device- and event-related
observations (e.g., touch devices, resize events), calculating and
aligning time variables, resolving repeated visits, and preparing
outputs that can be exported in desired formats.

## Workflow overview

A typical workflow looks like:

1.  **Standardize raw inputs** so downstream functions can be applied
    consistently.
2.  **Clean and scope** trajectories (remove invalid/empty cases,
    non-mouse events, optional dataset-specific filters).
3.  **Normalize geometry and time**, and derive key timing variables
    (e.g., initiation time, RT, move time).
4.  **Handle quality issues** such as resize-event artifacts, slow-move
    outliers, and repeated visits per screen.
5.  **Export a cleaned dataset** or pass the standardized output to
    downstream tooling.

## Standardization first

For consistent use of the functions, it is recommended that the raw
datasets are standardized. This step maps column names and formats into
a common schema, which will reduce the need to specify parameter names
in later functions.

## Precursor to `mousetrap`

`mousePrep` can be used in conjunction with the **`mousetrap`** package.
Together, the two packages support data cleaning and preparation for
subsequent analyses such as feature engineering and statistical
modeling: `mousePrep` focuses broadly on standardizing and filtering raw
logs, while `mousetrap` can be used for trajectory processing and
analysis workflows after preprocessing.

## Reference

A list of all the package functions are available here -
`docs/reference.html`
