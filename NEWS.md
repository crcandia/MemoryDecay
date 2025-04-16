# MemoryDecay News

## Version 0.1.0

*Initial release on CRAN*

- Introduces core functions for fitting forgetting curves using **biexponential**, **exponential**, and **log-normal** models.
- Supports three data types: **survey-based recall**, **time-series citations**, and **cross-sectional popularity**.
- Includes tools for:
  - Smoothing noisy memory data (LOESS/GAM).
  - Estimating **critical time** (inflection point of attention decay).
  - Grouped and stratified model fitting.
  - Model comparison using **AIC**, **BIC**, and visual diagnostics.
- Provides **publication-quality plots** with customization options for log scaling, grouping, and export.
- Built on the methodology developed in Candia et al. (2019, *Nature Human Behaviour*) and Candia & Uzzi (2021, *American Psychologist*).
