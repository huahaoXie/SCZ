# SCZ: Biomarker Signatures as Predictors of Future Impulsivity in Schizophrenia

> Analysis scripts for a multi‑center study exploring how peripheral biomarker signatures predict future impulsivity among inpatients with schizophrenia.

## 📁 Repository Structure

```
.
├── 1.psm.R          # Propensity-score matching
├── 2.lasso.R        # LASSO variable selection
├── 3.logistic.R     # Logistic regression modeling
├── 4.time_trend.R   # Temporal trend analysis
├── 5.sex_trend.R    # Sex-specific trend analysis
├── 6.roc.py         # ROC / AUC evaluation in Python
└── data/            # (Put your cleaned data here; not tracked by git)
```

## 🔧 Requirements & Installation

### R (≥ 4.0)
Install the needed packages (edit as necessary):
```r
install.packages(c(
  "MatchIt",   # PSM
  "glmnet",    # LASSO
  "ggplot2",   # plotting
  "dplyr",     # data wrangling
  "survival"   # if survival analyses are used
))
```

### Python (≥ 3.7)
```bash
pip install numpy pandas scikit-learn matplotlib
```

## ▶️ Usage

1. **Prepare data**  
   Put your cleaned dataset (e.g., `data.csv`) into the repo root or `data/`.  
   Make sure column names match what each script expects (check the header of each script).

2. **Run the pipeline (R):**
   ```bash
   Rscript 1.psm.R
   Rscript 2.lasso.R
   Rscript 3.logistic.R
   Rscript 4.time_trend.R
   Rscript 5.sex_trend.R
   ```

3. **Evaluate model performance (Python):**
   ```bash
   python 6.roc.py
   ```

> **Tip:** Each script reads/writes intermediate files. Adjust input/output paths in the scripts if your folder layout differs.

## 📦 Outputs (expected)

- Matched cohort tables and balance diagnostics  
- Selected biomarker list (coefficients / lambda plots)  
- Odds ratios and CIs from logistic models  
- Trend plots (time / sex stratifications)  
- ROC curves, AUC values, sensitivity/specificity

## 🤝 Contributing

Issues and pull requests are welcome!  
Please describe:
- What you changed
- Why it matters
- How to reproduce (minimal example)

## 📄 License

This project is licensed under the MIT License. See `LICENSE` for details.

---

*Last updated*: 2025-07-22
