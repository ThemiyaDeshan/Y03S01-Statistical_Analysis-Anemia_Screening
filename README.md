# Predictive Anemia Screening and Healthcare Resource Planning
Statistical modelling and predictive analytics for anemia screening, risk identification, and healthcare resource planning using FBC data.

## Project Overview

This project presents a statistical modelling consultancy study focused on **predictive anemia screening and healthcare resource planning using Full Blood Count (FBC) data in Sri Lankan healthcare institutions**.

The project investigates how routinely collected FBC and demographic information can be transformed into statistically supported insights for identifying anemia-associated patterns, developing predictive screening models, and supporting healthcare resource planning.

The project was developed as part of the **IT3081 – Statistical Modelling** consultancy project.

---

## Problem Statement

Healthcare institutions generate large volumes of routine Full Blood Count data. However, these data are often primarily interpreted at the individual patient level, creating an opportunity to develop analytical approaches that can identify population-level patterns and support healthcare planning.

This project addresses the following questions:

- Which demographic and institutional groups show different levels of anemia prevalence?
- Which non-definitional FBC variables are useful for anemia risk screening?
- How do different statistical and predictive modelling approaches perform?
- How can multicollinearity among FBC variables be addressed?
- How can predictive risk be connected to decision-making?
- How can analytical findings support healthcare resource planning?

---

## Project Objectives

1. Construct and assess an FBC dataset suitable for statistical analysis.
2. Quantify anemia patterns across sex, age and healthcare institution.
3. Develop and evaluate predictive screening models using demographic and non-definitional FBC variables.
4. Evaluate statistical approaches including regularisation, PCA, Bayesian methods and time-series applicability.
5. Translate the analytical evidence into a practical healthcare decision-support and resource-planning framework.

---

## Dataset

The final analytical dataset contains:

- **27,034 FBC records**
- **11,793 anemia records**
- Demographic variables
- Full Blood Count variables
- Institution information

Key variables considered include:

- Age
- Sex
- RBC
- PCV
- MCV
- MCH
- MCHC
- Institution

Hemoglobin (HB) was excluded from the predictive model to avoid label leakage because it directly contributes to the anemia definition.

> **Important:** The predictive model is therefore intended as a screening-support mechanism rather than a replacement for direct clinical diagnosis.

---

## Methodology

The project follows a structured statistical modelling workflow:

```text
Raw FBC Data
     ↓
Data Quality & Preparation
     ↓
Exploratory Data Analysis
     ↓
Statistical Analysis
     ↓
Predictive Modelling
     ↓
Model Evaluation
     ↓
PCA & Multicollinearity Analysis
     ↓
Bayesian Decision Analysis
     ↓
Healthcare Resource Planning
     ↓
AEWRAF Framework
```

### Statistical Analysis

The analysis examines:

- Overall anemia prevalence
- Anemia variation by sex
- Anemia variation by age
- Institution-level differences
- Associations between anemia and FBC variables
- Multicollinearity among predictors

### Predictive Modelling

Multiple approaches were evaluated, including:

- Logistic Regression
- LASSO
- Ridge Regression
- Elastic Net
- Bayesian Regression
- Naïve Bayes

Model evaluation considers more than accuracy, including discrimination, sensitivity, specificity, AUC and model stability.

### PCA

Principal Component Analysis was evaluated because several FBC variables are strongly correlated.

The analysis found that the first two principal components explained **82.8% of the variance**. However, reducing the predictors to two principal components resulted in lower predictive performance and reduced clinical interpretability.

Therefore, PCA was treated primarily as an exploratory and diagnostic technique rather than the main preprocessing method for the operational screening framework.

### Bayesian Decision Analysis

Bayesian decision analysis was used to connect predicted risk with potential decision costs.

An illustrative threshold of **0.15** produced an illustrative expected-loss reduction of approximately **44.5%** under the specified assumptions.

These values are not presented as universal clinical thresholds. A real implementation would require locally defined clinical costs, expert input and prospective validation.

---

## Key Findings

The analysis identified several important patterns.

### Overall Anemia Prevalence

Anemia was identified in:

**11,793 of 27,034 records**

corresponding to an overall prevalence of approximately:

**43.6%**

### Sex Differences

The observed anemia prevalence was:

- **Female: 54.2%**
- **Male: 29.2%**

### Institutional Differences

Institution-level variation was also observed, with **Institution C recording approximately 57.3% anemia prevalence**.

These findings demonstrate the importance of considering demographic and institutional context rather than relying only on an overall population estimate.

---

## Predictive Modelling Findings

The evaluated models demonstrated strong retrospective predictive performance.

The highest raw accuracy observed was approximately **94.4%**.

However, the project does not interpret accuracy alone as evidence of clinical readiness.

Strong correlations among FBC variables produced substantial multicollinearity, including a maximum VIF of approximately **461**.

The **Elastic Net** approach was selected as the operational candidate because it provides a balance between predictive performance and regularisation of correlated predictors.

The selected model achieved approximately:

- **92.4% Accuracy**
- **0.983 AUC**

These results represent retrospective model-development performance and require external and prospective validation before any clinical deployment.

---

## AEWRAF

### Anemia Early Warning and Resource Allocation Framework

The project proposes **AEWRAF** as a practical framework for translating statistical evidence into healthcare decision support.

```text
FBC & Demographic Data
          ↓
   Risk Scoring Engine
          ↓
 Bayesian Decision Layer
          ↓
 Clinical Triage Flag
          ↓
Resource Planning Dashboard
```

AEWRAF consists of four major components:

1. **Risk Scoring Engine**  
   Uses the selected predictive model to estimate anemia risk.

2. **Bayesian Decision Layer**  
   Connects predicted risk with an agreed decision-cost structure.

3. **Clinical Triage Flag**  
   Provides an interpretable indication for potential follow-up or further attention.

4. **Resource Planning Dashboard**  
   Aggregates risk and prevalence indicators to support institutional and regional resource planning.

The framework is intended as **decision support**, not as a replacement for clinical diagnosis.

---

## Industry Expert Validation

The completed consultancy analysis was reviewed by an **in-house medical doctor at SLIIT Kandy**.

The expert reviewed:

- Identified anemia-associated variables
- Statistical findings
- Predictive modelling
- Proposed AEWRAF framework

The expert provided positive professional feedback and considered the identified variables relevant to anemia assessment.

This validation provides professional clinical perspective on the relevance of the analytical findings. Further validation using independent datasets, multiple healthcare experts and prospective studies would still be required before clinical deployment.

---

## Recommendations

The project recommends a staged approach to implementation:

1. Standardise FBC data collection and reporting across institutions.
2. Conduct site-specific retrospective validation of the predictive model.
3. Investigate institutional differences in anemia prevalence.
4. Establish clinically informed decision costs and thresholds.
5. Conduct prospective institution-blocked validation.
6. Monitor model performance, fairness and data drift.
7. Evaluate clinical utility before operational deployment.
8. Use validated analytical indicators to support healthcare resource planning.

---

## Project Limitations

Important limitations include:

- The dataset represents retrospective observations.
- Institution sizes and case mixes are uneven.
- Repeated patient encounters may introduce dependence between observations.
- The model was developed using a specific dataset and requires external validation.
- Missingness in future operational data may differ from the development dataset.
- Excluding HB avoids label leakage but means the model should not be treated as a replacement for direct HB-based diagnosis.
- The Bayesian threshold analysis uses illustrative costs and requires clinical calibration.
- Prospective clinical utility has not yet been established.

---

## Technologies and Methods

The project uses statistical and machine-learning techniques including:

- Exploratory Data Analysis
- Descriptive Statistics
- Hypothesis Testing
- Logistic Regression
- LASSO
- Ridge Regression
- Elastic Net
- Bayesian Regression
- Naïve Bayes
- Principal Component Analysis
- Variance Inflation Factor Analysis
- Bayesian Decision Analysis
- Time-Series Analysis
- Multicentre Validation Concepts

---

## Repository Structure

```text
predictive-anemia-screening-healthcare-resource-planning/
│
├── README.md
│
├── data/
│   └── README.md
│
├── notebooks/
│   ├── data_preparation/
│   ├── exploratory_analysis/
│   ├── statistical_analysis/
│   ├── predictive_modelling/
│   ├── pca/
│   └── bayesian_analysis/
│
├── src/
│   ├── data_preprocessing/
│   ├── statistical_analysis/
│   ├── modelling/
│   └── evaluation/
│
├── results/
│   ├── figures/
│   ├── tables/
│   └── model_results/
│
├── documentation/
│   ├── consultancy_report/
│   └── literature/
│
└── presentation/
    └── linkedin_showcase/
```

---

## Team Members

| Student ID | Student |
|---|---|
| IT24102479 | Bandara D.M.A.S. |
| IT24103178 | Rathnayaka R.M.T.D. |
| IT24103159 | Athauda K.K.A.M.S.R.V.B. |
| IT24102904 | Fernando H.M.D. |

---

## Academic Project

**Module:** IT3081 – Statistical Modelling  
**Project Type:** Consultancy Project  
**Theme:** Public Health Indicator Analysis  
**Context:** Sri Lankan Healthcare Institutions

---

## Disclaimer

This repository represents an academic statistical modelling consultancy project.

The predictive models and decision-support framework presented here are intended for research and academic purposes. They should **not be used for clinical diagnosis, treatment decisions or direct healthcare deployment** without appropriate clinical validation, governance, regulatory review and prospective evaluation.

---

## Acknowledgements

We acknowledge the healthcare perspective provided during the industry expert validation of the project and the academic guidance provided throughout the IT3081 Statistical Modelling consultancy project.
