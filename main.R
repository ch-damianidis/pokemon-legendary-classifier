################################################################################
### Project: Pokemon Legendary Classification (Gen 1-7)
### Pipeline: Load -> Subset & Feature Engineering -> Data Quality Check
################################################################################

# Required libraries
library(readr)
library(dplyr)
library(stringr)
library(tibble)


# ==============================================================================
# 1. DATA INGESTION
# ==============================================================================

# Download dataset if not already present in workspace
csv_path <- "pokemon.csv"

if (!file.exists(csv_path)) {
  # Backup raw GitHub mirror for the Rounak Banik dataset
  data_url <- "https://raw.githubusercontent.com/rounakbanik/pokemon/master/pokemon.csv"
  download.file(data_url, destfile = csv_path, mode = "wb")
  message("Dataset downloaded successfully: ", csv_path)
}

# Read raw dataset
df_raw <- read_csv(csv_path, show_col_types = FALSE)

# Initial dimension check
dim(df_raw) # Should be 802 rows and 41 columns


# ==============================================================================
# 2. SUBSETTING & FEATURE ENGINEERING
# ==============================================================================

pokemon_ml <- df_raw %>%
  mutate(
    # Clean non-numeric characters from capture_rate 
    capture_rate = as.numeric(str_extract(as.character(capture_rate), "^[0-9]+")),
    
    # Target variable as binary factor
    is_legendary = factor(is_legendary, levels = c(0, 1), labels = c("No", "Yes")),
    
    # Binary indicator for genderless status (NA percentage_male represents genderless)
    is_genderless = factor(if_else(is.na(percentage_male), 1, 0), levels = c(0, 1), labels = c("No", "Yes")),
    
    # Factors for categorical modeling
    type1 = as.factor(type1),
    generation = as.factor(generation)
  ) %>%
  select(
    # Identifiers (metadata)
    pokedex_number,
    name,
    
    # Target outcome
    is_legendary,
    
    # Base combat stats
    base_total,
    hp,
    attack,
    defense,
    sp_attack,
    sp_defense,
    speed,
    
    # In-game capture & breeding mechanics
    capture_rate,
    base_egg_steps,
    base_happiness,
    
    # Biological & classification features
    is_genderless,
    type1,
    generation
  )

# Verify subset structure
glimpse(pokemon_ml)


# ==============================================================================
# 3. DATA QUALITY & INTEGRITY CHECKS (Cleaned Dataset)
# ==============================================================================

### A. Missing Values Check ###
na_counts <- colSums(is.na(pokemon_ml))
na_props  <- colMeans(is.na(pokemon_ml)) * 100

tibble(
  variable = names(na_counts),
  na_count = na_counts,
  na_pct   = round(na_props, 2)
)

# Assert zero NAs in the final modeling subset
stopifnot(sum(is.na(pokemon_ml)) == 0)


### B. Uniqueness & Duplication ###
cat("Duplicate rows:", sum(duplicated(pokemon_ml)), "\n")
cat("Unique Pokedex IDs:", length(unique(pokemon_ml$pokedex_number)) == nrow(pokemon_ml), "\n")
cat("Unique Names:", length(unique(pokemon_ml$name)) == nrow(pokemon_ml), "\n")


### C. Target Variable Distribution (Class Imbalance) ###
cat("Target class counts:\n")
print(table(pokemon_ml$is_legendary, useNA = "always"))

cat("Target class proportions (%):\n")
print(round(prop.table(table(pokemon_ml$is_legendary)) * 100, 2))


### D. Structural Consistency Check: Individual Stats vs Base Total ###
# Verify that base_total strictly equals the sum of the 6 core stats
stat_cols <- c("hp", "attack", "defense", "sp_attack", "sp_defense", "speed")
stat_sum_check <- rowSums(pokemon_ml[, stat_cols]) == pokemon_ml$base_total

cat("Total stat calculation mismatches:", sum(!stat_sum_check), "\n")


### E. Summary Statistics & Boundary Checks ###
summary(pokemon_ml %>% select(base_total, capture_rate, base_egg_steps, base_happiness))




names(pokemon_ml)


library(ggplot2)
library(tidyr)



# ==============================================================================
# 4. EXPLORATORY DATA ANALYSIS (EDA)
# ==============================================================================

### A. Summary Stats per Class ###
pokemon_ml %>%
  group_by(is_legendary) %>%
  summarise(
    n = n(),
    pct = round(n() / nrow(pokemon_ml) * 100, 1),
    mean_bst = round(mean(base_total), 1),
    sd_bst = round(sd(base_total), 1),
    median_capture = median(capture_rate),
    median_egg_steps = median(base_egg_steps),
    pct_genderless = round(mean(is_genderless == "Yes") * 100, 1)
  )


### B. Visualizing the Core Decision Boundary (BST vs Capture Rate) ###
# Scatter plot displaying separation and overlap (pseudo-legendaries)
ggplot(pokemon_ml, aes(x = base_total, y = capture_rate, color = is_legendary)) +
  geom_point(alpha = 0.7, size = 2.5) +
  scale_y_log10() + # Log scale highlights the low capture rates (e.g., 3 vs 45)
  scale_color_manual(values = c("No" = "#4A90E2", "Yes" = "#D0021B")) +
  theme_minimal() +
  labs(
    title = "Base Stat Total vs. Capture Rate (Log Scale)",
    subtitle = "Legendaries cluster in high BST and ultra-low capture rate",
    x = "Base Stat Total (BST)",
    y = "Capture Rate (Log10)",
    color = "Legendary"
  )


### C. Distributions of Core Mechanics (Boxplots) ###
# Comparing base stats, egg steps, and happiness
pokemon_ml %>%
  select(is_legendary, base_total, base_egg_steps, base_happiness) %>%
  pivot_longer(cols = -is_legendary, names_to = "metric", values_to = "value") %>%
  ggplot(aes(x = is_legendary, y = value, fill = is_legendary)) +
  geom_boxplot(alpha = 0.7, outlier.colour = "black", outlier.shape = 16) +
  facet_wrap(~metric, scales = "free_y") +
  scale_fill_manual(values = c("No" = "#4A90E2", "Yes" = "#D0021B")) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(
    title = "Feature Distributions across Target Classes",
    x = "Is Legendary",
    y = "Value"
  )



################################################################################
### 5. STRATIFIED TRAIN / TEST SPLIT
################################################################################

# Install rsample if not present
if (!requireNamespace("rsample", quietly = TRUE)) {
  install.packages("rsample")
}

library(rsample)


# Set random seed for reproducibility
set.seed(42)

# Perform stratified split (80% train, 20% test) based on target variable
data_split <- initial_split(pokemon_ml, prop = 0.80, strata = is_legendary)

train_df <- training(data_split)
test_df  <- testing(data_split)

# Verify split dimensions
cat("Training set size:", nrow(train_df), "\n")
cat("Testing set size :", nrow(test_df), "\n\n")

# Verify that class proportions are preserved across splits
cat("Original distribution:\n")
print(round(prop.table(table(pokemon_ml$is_legendary)) * 100, 2))

cat("\nTrain distribution:\n")
print(round(prop.table(table(train_df$is_legendary)) * 100, 2))

cat("\nTest distribution:\n")
print(round(prop.table(table(test_df$is_legendary)) * 100, 2))



################################################################################
### 6. BASELINE MODEL: LOGISTIC REGRESSION (GLM)
################################################################################

# Fit binomial GLM on training data
glm_model <- glm(
  is_legendary ~ hp + attack + defense + sp_attack + sp_defense + speed +
    capture_rate + base_egg_steps + base_happiness + is_genderless,
  data = train_df,
  family = binomial(link = "logit")
)

# Inspect model summary
summary(glm_model)



################################################################################
### 7. RANDOM FOREST CLASSIFICATION (RANGER)
################################################################################

if (!requireNamespace("ranger", quietly = TRUE)) {
  install.packages("ranger")
}

library(ranger)

# Train Random Forest classifier
# - importance = "impurity" (Gini index drop)
# - probability = TRUE allows probabilistic predictions (for ROC/PR-AUC)
set.seed(42)
rf_model <- ranger(
  formula = is_legendary ~ hp + attack + defense + sp_attack + sp_defense + speed +
    capture_rate + base_egg_steps + base_happiness + is_genderless,
  data = train_df,
  num.trees = 500,
  importance = "impurity",
  probability = TRUE
)

# Print training summary & Out-of-Bag (OOB) error
print(rf_model)


# Install rpart.plot if not available
if (!requireNamespace("rpart.plot", quietly = TRUE)) {
  install.packages("rpart.plot")
}

library(rpart)
library(rpart.plot)

# Train a single interpretable decision tree on the training set
tree_model <- rpart(
  formula = is_legendary ~ hp + attack + defense + sp_attack + sp_defense + speed +
    capture_rate + base_egg_steps + base_happiness + is_genderless,
  data = train_df,
  method = "class",
  control = rpart.control(cp = 0.01, minsplit = 10, maxdepth = 4)
)

# Custom palette: Soft gray/blue for "No", vivid red/coral for "Yes"
node_colors <- c("#D5E8D4", "#F8CECC")

# High-quality decision tree plot
rpart.plot(
  tree_model,
  type = 4,                       # Split label on branch, node info inside box
  extra = 104,                    # Display: Predicted class, probabilities, % of obs
  under = FALSE,                  # Keep stats inside the node box
  fallen.leaves = TRUE,           # Align terminal leaves neatly at the bottom
  box.palette = list("No" = "#E1F5FE", "Yes" = "#FFCDD2"), # Soft blue & red
  shadow.col = "#B0BEC5",         # Elegant drop shadow
  border.col = "#37474F",         # Dark crisp border
  branch.lty = 1,                 # Solid branches
  branch.lwd = 1.8,               # Thicker connecting lines
  branch.col = "#455A64",         # Muted dark branch color
  split.cex = 1.1,                # Font size for branch questions
  split.font = 2,                 # Bold font for questions
  cex = 0.95,                     # Node text size
  yesno = 2,                      # Print explicit "yes" and "no" on branches
  digits = -3,                    # Fixes 15e+3 into readable 15000
  main = "Legendary Pokémon Decision Flow\n(Deterministic Rules)"
)


# ==============================================================================
# 8. MODEL EVALUATION & COMPARISON ON TEST SET (n = 161)
# ==============================================================================

# --- A. Decision Tree Predictions ---
pred_tree_class <- predict(tree_model, newdata = test_df, type = "class")

cat("=== Decision Tree Confusion Matrix ===\n")
cm_tree <- table(Actual = test_df$is_legendary, Predicted = pred_tree_class)
print(cm_tree)

acc_tree  <- sum(diag(cm_tree)) / sum(cm_tree)
sens_tree <- cm_tree["Yes", "Yes"] / sum(cm_tree["Yes", ])
spec_tree <- cm_tree["No", "No"] / sum(cm_tree["No", ])


# --- B. Random Forest Predictions ---
rf_preds <- predict(rf_model, data = test_df)$predictions
test_df$pred_prob_rf <- rf_preds[, "Yes"]

pred_rf_class <- factor(
  if_else(test_df$pred_prob_rf >= 0.50, "Yes", "No"),
  levels = c("No", "Yes")
)

cat("\n=== Random Forest Confusion Matrix ===\n")
cm_rf <- table(Actual = test_df$is_legendary, Predicted = pred_rf_class)
print(cm_rf)

acc_rf  <- sum(diag(cm_rf)) / sum(cm_rf)
sens_rf <- cm_rf["Yes", "Yes"] / sum(cm_rf["Yes", ])
spec_rf <- cm_rf["No", "No"] / sum(cm_rf["No", ])


# --- C. Model Comparison Table ---
model_comparison <- tibble(
  Model       = c("Decision Tree (rpart)", "Random Forest (ranger)"),
  Accuracy    = round(c(acc_tree, acc_rf) * 100, 2),
  Sensitivity = round(c(sens_tree, sens_rf) * 100, 2),
  Specificity = round(c(spec_tree, spec_rf) * 100, 2)
)

print(model_comparison)


# --- D. Inspect False Positives / False Negatives ---
misclassified <- test_df %>%
  mutate(pred_rf = pred_rf_class) %>%
  filter(is_legendary != pred_rf) %>%
  select(name, is_legendary, pred_rf, pred_prob_rf, base_egg_steps, capture_rate)

if (nrow(misclassified) > 0) {
  cat("\nMisclassified Pokemon (Random Forest):\n")
  print(misclassified)
} else {
  cat("\nZero misclassifications on the test set for Random Forest!\n")
}


# ==============================================================================
# 9. LIVE PREDICTION FUNCTION
# ==============================================================================

predict_pokemon_tier <- function(name = "Custom Mon",
                                 hp, attack, defense, sp_attack, sp_defense, speed,
                                 capture_rate, base_egg_steps, base_happiness = 70,
                                 is_genderless = "No",
                                 model = rf_model) {
  
  # Construct single-row input tibble
  input_data <- tibble(
    hp             = hp,
    attack         = attack,
    defense        = defense,
    sp_attack      = sp_attack,
    sp_defense     = sp_defense,
    speed          = speed,
    capture_rate   = capture_rate,
    base_egg_steps = base_egg_steps,
    base_happiness = base_happiness,
    is_genderless  = factor(is_genderless, levels = c("No", "Yes"))
  )
  
  # Run probability prediction
  prob_legendary <- predict(model, data = input_data)$predictions[1, "Yes"]
  class_decision <- if_else(prob_legendary >= 0.50, "Legendary", "Non-Legendary")
  
  cat("--- Pokemon Classification Report ---\n")
  cat("Subject       :", name, "\n")
  cat("Predicted Tier:", class_decision, "\n")
  cat("Probability   :", round(prob_legendary * 100, 2), "%\n")
  cat("BST           :", sum(c(hp, attack, defense, sp_attack, sp_defense, speed)), "\n")
  
  invisible(list(class = class_decision, probability = prob_legendary))
}

# Example test: Mewtwo vs Dragonite
predict_pokemon_tier(
  name = "Mewtwo",
  hp = 106, attack = 110, defense = 90, sp_attack = 154, sp_defense = 90, speed = 130,
  capture_rate = 3, base_egg_steps = 30720, base_happiness = 0, is_genderless = "Yes"
)

predict_pokemon_tier(
  name = "Dragonite",
  hp = 91, attack = 134, defense = 95, sp_attack = 100, sp_defense = 100, speed = 80,
  capture_rate = 45, base_egg_steps = 10240, base_happiness = 35, is_genderless = "No"
)
