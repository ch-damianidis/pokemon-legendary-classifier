# ⚡ Pokémon Legendary Classifier: ML Pipeline & Blind Inference App

An end-to-end Machine Learning pipeline and interactive Shiny web application designed to predict whether a Pokémon belongs to the **Legendary** tier based strictly on underlying game mechanics, breeding biology, and base combat statistics—without ever revealing the Pokémon's identity or name.

<img width="1920" height="1112" alt="image" src="https://github.com/user-attachments/assets/64efa789-1674-458f-99c3-0556771be778" />


---

## 📌 Project Overview

Can an algorithm deduce whether a Pokémon is Legendary based solely on raw numbers?

While base combat statistics (Base Stat Total) are high for Legendary species, they often overlap with Pseudo-Legendaries (e.g., *Dragonite*, *Tyranitar*, *Garchomp*). This project demonstrates that **Game Freak's intrinsic game mechanics**—specifically **Breeding Egg Cycles (`base_egg_steps`)** and **Catch Difficulty (`capture_rate`)**—create near-deterministic decision boundaries that classify Legendary Pokémon with **>99% accuracy**.

---

## 🧪 Modeling Architecture & Key Findings

1. **Quasi-Complete Separation (GLM Logit):**
   * A baseline Binomial Logistic Regression model identified severe quasi-complete separation.
   * `base_egg_steps` ($p = 0.0248$) served as the primary linear boundary driver, while standard combat stats offered negligible additional information once breeding mechanics were controlled.

2. **Interpretable Decision Tree (`rpart`):**
   * A pruned tree achieved **98.8% accuracy** on the unseen test set using just two sequential splits:
     * **Step 1:** $\text{base\_egg\_steps} \ge 15,000 \rightarrow \mathbf{Legendary}\ (98\%)$
     * **Step 2:** For $\text{base\_egg\_steps} < 15,000$, if $\text{capture\_rate} < 9 \rightarrow \mathbf{Legendary}\ (67\%)$, else $\mathbf{Standard}\ (100\%)$.

3. **Random Forest Ensemble (`ranger`):**
   * **Test Accuracy:** 99.38% (160 / 161 correct predictions)
   * **Sensitivity (Recall):** 100% (Identified all 18 Legendary targets)
   * **Specificity:** 99.30%
   * **Out-of-Bag (OOB) Brier Error:** 0.0056

---

## 🔍 Model Performance Summary (Test Set, $n = 161$)

| Model | Accuracy (%) | Sensitivity (%) | Specificity (%) | Primary Split Drivers |
| :--- | :---: | :---: | :---: | :--- |
| **Decision Tree (`rpart`)** | 98.76% | 100.0% | 98.60% | `base_egg_steps`, `capture_rate` |
| **Random Forest (`ranger`)** | **99.38%** | **100.0%** | **99.30%** | Gini Impurity via Core Mechanics |

---

## 👾 Case Study: The Narrative Edge Cases

The algorithm was intentionally evaluated blind to species names, leading to fascinating mechanical edge cases:

* **Type: Null (False Positive — $P(\text{Yes}) = 73.7\%$):**
  * *Metrics:* 30,720 Egg Steps, Capture Rate of 3.
  * *Context:* Lore-wise, *Type: Null* is an artificial chimera created to synthesize the powers of the Legendary Arceus. Game Freak assigned it legendary-tier breeding mechanics and catch rates, which perfectly fool the machine learning model despite being categorized in code as a standard species.
* **Eelektross (Combat Outlier):**
  * Possesses the *Levitate* ability on a pure Electric typing (zero type weaknesses) combined with a restrictive capture rate (30), testing the model's reliance on non-combat mechanics over raw defensive profiles.

---

## 🖥️ Interactive Blind Inference Application (`Shiny` + `bslib`)

The repository includes a modern dark-mode Shiny application for live blind-testing:
* **Blind Evaluation Mode:** Input custom stats or test mystery species without knowing the name.
* **Random Mystery Pokémon (🎲):** Automatically populates sliders with an unseen target from the dataset.
* **Identity Reveal (👁️):** Spoiler-safe reveal button to compare the model's confidence against actual ground truth.

---

## 📂 Repository Structure

```text
├── figures/
│   └── decision_tree.png    # High-resolution decision path plot
├── .gitignore
├── renv.lock                # Dependency management lockfile
├── main.R                   # Ingestion, EDA, GLM, RF, and evaluation
├── app.R                    # Interactive Shiny web application
└── README.md
