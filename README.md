# NBA Player Archetypes Using Unsupervised Learning

## Overview
Player comparisons are a constant part of NBA discourse, often relying on subjective narratives tied to play style, size, or star status. This project takes a data-driven approach to identifying NBA player archetypes by applying unsupervised learning to standardize on-court performance statistics. Rather than assigning predefined roles or positions, the goal is to uncover latent structure in player performance and interpret these patterns as meaningful basketball archetypes.

---

## Data
The dataset consists of NBA player performance statistics from a single season. To ensure stable and interpretable roles, the analysis focuses on players with sufficient playing time, filtering out low-minute and low-game appearances. Statistics were standardized prior to clustering to prevent scale-driven
artifacts and to allow direct comparison across features.

---

## Methodology
The analysis follows a structured unsupervised learning pipeline:

1. **Data cleaning and filtering** to construct a consistent player cohort  
2. **Feature selection and standardization** using z-scores  
3. **K-means clustering**, with the number of clusters selected using a
   combination of the elbow method, silhouette scores, and gap statistics  
4. **Dimensionality reduction (PCA)** for visualization and interpretability  
5. **Cluster profiling** via mean standardized feature values  

The clustering process is designed for interpretability rather than prediction, with an emphasis on discovering latent player roles.

---

## Results
The model identifies seven distinct player archetypes:

- Defensive Rim Protectors  
- Perimeter Shooters and Secondary Guards  
- Interior Bigs  
- Low-Usage Role Players  
- Superstar Offensive Hubs  
- Versatile Two-Way Forwards  
- Primary Shot Creators
Each archetype reflects a unique combination of statistical tendencies rather than reliance on a single defining metric. Representative players are used to ground these archetypes in real basketball contexts.  
---

## Interpretation
Cluster-level feature profiles reveal how offensive responsibility, defensive impact, and versatility interact to define player roles. Overlap between some archetypes highlights the fluid nature of modern NBA basketball, where players often contribute across multiple dimensions rather than fitting into rigid
categories.

---

## Limitations and Future Work
This analysis is limited by its reliance on a single season of data and the assumptions inherent to k-means clustering. Future extensions could incorporate multi-season data, player tracking statistics, or alternative clustering methods to explore role evolution and archetype stability over time.

---

## Repository Structure

```text
NBA_Archetypes/
├── nba_player_archetypes.ipynb
├── data/
│ └── raw/
├── scripts/
├── output/
│ ├── figures/
│ └── tables/
└── README.md
---
```   
## How to Run
The analysis can be run end-to-end using the Jupyter notebook `nba_player_archetypes.ipynb`. The notebook is designed to pull intermediate artifacts from the repository and does not require rerunning all scripts to reproduce figures and tables.

---

## Takeaway
This project demonstrates how unsupervised learning can be used to transform high-dimensional sports data into interpretable insights, bridging statistical analysis with an intuitive understanding of player roles in the NBA.

