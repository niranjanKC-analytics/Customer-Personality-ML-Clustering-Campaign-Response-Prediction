# Customer Personality ML — Clustering & Campaign Response Prediction

**Course:** CIS 468 Data Mining — Towson University | Spring 2026
**Team:** Niranjan K C · Sofia Foutzitzi · Jivitesh Marken · Oluwadamilola Bright-Awonuga
**Tools:** R · K-Means · Decision Tree · KNN · Logistic Regression · ggplot2 · caret

---

## Overview

This project applies data mining techniques to the Customer Personality Analysis
dataset to segment customers and predict marketing campaign responses. Only 14.9%
of customers responded to the latest campaign — the goal was to identify which
customers are worth targeting and why.

---

## Research Questions

1. Can we group customers into meaningful segments based on income and spending?
2. Can we predict whether a customer will accept a marketing campaign?
3. Which features matter most — income, recency, prior campaign acceptance, or spending?
4. Which classification algorithm best handles the 15/85 class imbalance?

---

## Dataset

- **Source:** Kaggle — Customer Personality Analysis (Patel, 2021)
- **Size:** 2,240 customers · 29 features
- **Target variable:** Response (campaign acceptance — binary 0/1)
- **Class imbalance:** 14.9% positive (responded) vs 85.1% negative

---

## Methods

### Preprocessing
- Median imputation for missing Income values
- Created engineered features: Age, TotalSpend, TotalPurchases, TotalCampaignsAccepted
- Standardized numerical variables for clustering
- Removed unnecessary columns to reduce noise

### Clustering — K-Means (k=4)
Optimal k selected via Elbow Method and Silhouette scores

| Cluster | n | Avg Income | Avg Spend | Response Rate |
|---|---|---|---|---|
| Cluster 1 | 189 | $82,547 | $1,612 | 57.7% |
| Cluster 2 | 854 | $67,477 | $1,036 | 13.2% |
| Cluster 3 | 604 | $35,603 | $131 | 14.4% |
| Cluster 4 | 593 | $37,576 | $148 | 4.2% |

### Classification Models

| Model | Accuracy | F1 Score | Sensitivity | Specificity |
|---|---|---|---|---|
| Decision Tree | 85.25% | **0.4649** | 0.43 | 0.9264 |
| Logistic Regression | 87.33% | 0.4444 | 0.34 | 0.9667 |
| KNN | 85.84% | 0.2963 | 0.20 | 0.9737 |

---

## Key Findings

- Cluster 1 (high income/spend) had a 57.7% response rate vs 4.2% for Cluster 4
- Decision Tree achieved the best F1 score (0.4649) across all 3 classifiers
- Income and TotalSpend were the strongest predictors of campaign response
- Recency and prior campaign acceptance were also highly influential features
- Class imbalance (14.9% positive) was a key challenge for all classifiers

---

## Repository Structure

```
customer-personality-ml/
├── data/
│   ├── marketing_campaign.csv
│   └── marketing_clean.csv
├── scripts/
│   └── FinalProject_Complete.R
├── report/
│   └── CIS_468_Final_project.docx
├── presentation/
│   └── CIS468_FinalProject_Presentation.pptx
└── README.md
```

---

## R Packages Used

`caret` · `rpart` · `class` · `ggplot2` · `dplyr` · `cluster` · `factoextra`

---

## References

Patel, A. (2021). Customer Personality Analysis. Kaggle.
MacQueen, J. (1967). K-Means clustering.
Breiman et al. (1984). Classification and Regression Trees.
He & Garcia (2009). Learning from imbalanced data.

---

## 👤 Author
**Niranjan K C**
Data Analyst | B.S. Information Technology — Towson University, May 2026

[![GitHub](https://img.shields.io/badge/GitHub-niranjanKC--analytics-black?logo=github)](https://github.com/niranjanKC-analytics)
[![Portfolio](https://img.shields.io/badge/Portfolio-Data%20Analytics-green?logo=github)](https://github.com/niranjanKC-analytics/data-analytics-portfolio)
[![LinkedIn](https://img.shields.io/badge/LinkedIn-Niranjan%20K%20C-blue?logo=linkedin)](https://www.linkedin.com/in/niranjan-k-c-44b681334/)
[![Tableau](https://img.shields.io/badge/Tableau-Portfolio-orange?logo=tableau)](https://public.tableau.com/app/profile/niranjan.k.c5704/vizzes)

---
⭐ *Check out my full analytics portfolio for more projects!*
