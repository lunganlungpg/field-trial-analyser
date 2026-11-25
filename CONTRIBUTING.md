# Contributing to Field Trial Analyser

Contributions from agricultural statisticians, agronomists, R developers, and educators are warmly welcomed.

## Local Development Workflow

1. Edit statistical engine files under `R/`.
2. Mirror changes into `app/R/` (and any dataset additions into both `inst/extdata/` and `app/data/`).
3. Run the verification suite before committing:

```bash
Rscript tests/run_tests.R
```

This checks:
- Exact file parity between `R/` and `app/R/` as well as `inst/extdata/` and `app/data/`
- `app/app.R` syntax validity
- ANOVA degrees of freedom and F-ratios across CRD, RCBD, Latin Square, Factorial RCBD, and Split-Plot (`Error(a)` / `Error(b)`)
- Piepho (2004) Compact Letter Display pairwise invariance
- Transformation advisor, replicate power planner, HTML report SVG output, and R script export

## WebAssembly (webR / Shinylive) Guardrails

Keep `app/` dependencies lean (`shiny` and `bslib` only) and avoid compiled C/C++ packages inside the browser bundle unless verified on `repo.r-wasm.org`.
