## CRAN submission comments

This is the first submission of the `MemoryDecay` package.

The package provides tools to model and visualize forgetting curves in collective memory studies. It supports biexponential, exponential, and log-normal decay forms, and includes functionality for grouped fitting, model comparison (AIC/BIC), and publication-quality plots. It is compatible with survey-based, cross-sectional, and time-series datasets.

The methodology is based on:
- Candia et al. (2019) <doi:10.1038/s41562-018-0474-5>
- Candia & Uzzi (2021) <doi:10.1037/amp0000863>

### R CMD check results

Checked with:
- R version 4.3.2
- macOS Big Sur
- `--as-cran`

Results:
- **0 ERRORs**
- **0 WARNINGs**
- **4 NOTEs**, all of which are benign:

1. **Author field mismatch** due to inclusion of ORCID in `Authors@R` but not in `Author:` field. This is expected behavior.
2. **Line > 100 characters** in a single Rd example. Does not affect rendering or functionality.
3. **HTML validation warnings** due to automatically generated Rd2HTML files. These are not under user control.
4. **Non-ASCII character replacements** in a few Rd files, handled correctly with UTF-8 encoding.

All vignettes, examples, and documentation were successfully built and passed validation.

Thank you for reviewing this submission.

Cristian Candia
