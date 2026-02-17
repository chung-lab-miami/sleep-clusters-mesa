# Sleep Clusters & All-Cause Mortality in MESA

Unsupervised machine learning (sparse K-means clustering) applied to wearable actigraphy data from the Multi-Ethnic Study of Atherosclerosis (MESA) to identify sleep phenotypes and model their association with all-cause mortality.

**[View the Interactive Code Review & Portfolio Page](https://joonchungpersonal-dev.github.io/sleep-clusters-mesa/)**

## Key Findings

- Identified 2 distinct sleep clusters from 7 actigraphy-derived metrics (N = 1,759)
- "Irregular sleepers" showed **67% higher mortality risk** (HR = 1.67, 95% CI: 1.22–2.30)
- Results robust across 40+ Cox PH models, propensity score matching, and sensitivity analyses

## Technical Skills Demonstrated

| Skill | Methods |
|-------|---------|
| Unsupervised Learning | Sparse K-means, gap statistic, NbClust (30 indices), bootstrap stability |
| Causal Inference | Propensity score matching (5 specifications), IPTW, DAG-informed adjustment |
| Survival Analysis | Cox PH (40+ models), Kaplan-Meier, age-as-time-metric sensitivity |
| Data Engineering | 8-script reproducible pipeline, multi-source data harmonization |
| Dimensionality Reduction | PCA composite scoring, feature selection via sparsity penalty |

## Repository Structure

```
├── index.html          # Interactive code review & portfolio page
├── R_code/
│   ├── 01_mesa_mortality (import, clean).R
│   ├── 02_mesa_mortality (sleep, composites).R
│   ├── 03_mesa_mortality (diet, other covariates, analytic df).R
│   ├── 04_mesa_mortality (shs_df - analytical dataset).R
│   ├── 05_mesa_clusters vs individual components.R
│   ├── 06_mesa_mortality_cluster_survplots.R
│   ├── 07_Cluster Propensity Score matching (MESA).R
│   ├── 08_mesa_cluster_tables.R
│   ├── 12 - mesa_mortality sensitvity (exclude 18 deaths).R
│   ├── Cluster paper FINAL RDA.R
│   └── MESA sleep cluster code.R
└── README.md
```

## Publication

Chung J, et al. (2023). Sleep regularity and mortality in the Multi-Ethnic Study of Atherosclerosis. *Sleep Health*.

## Author

**Joon Chung, PhD**
University of Miami, Miller School of Medicine
*Originally conducted at Brigham and Women's Hospital / Harvard Medical School (2022)*
