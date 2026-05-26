# ============================================================
# CIS 468 - Final Project
# Team: Sofia Foutzitzi | Niranjan K C | Jivitesh Marken | Oluwadamilola Bright-Awonuga
# Professor: Dr. McGuire
# Dataset: Customer Personality Analysis (marketing_campaign.csv)
# ============================================================



# ── 1. LOAD LIBRARIES ────────────────────────────────────────
library(ggplot2)
library(dplyr)
library(corrplot)
library(scales)
library(gridExtra)
library(caret)
library(cluster)
library(factoextra)
library(fpc)
library(rpart)
library(rpart.plot)
library(class)

# ── 2. LOAD DATA ─────────────────────────────────────────────
marketing <- read.csv("marketing_campaign.csv", sep = "\t", header = TRUE)

dim(marketing)
str(marketing)
head(marketing)

# ── 3. MISSING VALUES ────────────────────────────────────────
cat("Total missing values:", sum(is.na(marketing)), "\n")
colSums(is.na(marketing))

# Impute Income with median (24 missing values)
marketing$Income[is.na(marketing$Income)] <- median(marketing$Income, na.rm = TRUE)

cat("Missing values after imputation:", sum(is.na(marketing)), "\n")

# ── 4. FEATURE ENGINEERING ───────────────────────────────────
# Derive Age from Year_Birth
marketing$Age <- 2024 - marketing$Year_Birth

# Total spending across all 6 product categories
marketing$TotalSpend <- marketing$MntWines +
                        marketing$MntFruits +
                        marketing$MntMeatProducts +
                        marketing$MntFishProducts +
                        marketing$MntSweetProducts +
                        marketing$MntGoldProds

# Total number of prior campaigns accepted
marketing$TotalCampaignsAccepted <- marketing$AcceptedCmp1 +
                                     marketing$AcceptedCmp2 +
                                     marketing$AcceptedCmp3 +
                                     marketing$AcceptedCmp4 +
                                     marketing$AcceptedCmp5

# Total purchases across all channels
marketing$TotalPurchases <- marketing$NumWebPurchases +
                             marketing$NumCatalogPurchases +
                             marketing$NumStorePurchases +
                             marketing$NumDealsPurchases

# ── 5. ENCODE CATEGORICAL VARIABLES ──────────────────────────
marketing$Education      <- as.factor(marketing$Education)
marketing$Marital_Status <- as.factor(marketing$Marital_Status)
marketing$Response       <- as.factor(marketing$Response)

# ── 6. DROP CONSTANT / IRRELEVANT COLUMNS ────────────────────
marketing <- marketing[ , !(names(marketing) %in% c("Z_CostContact", "Z_Revenue", "ID"))]

cat("\nFinal dataset dimensions:", dim(marketing), "\n")
summary(marketing)

# ── 7. EDA — SUMMARY STATISTICS ──────────────────────────────
cat("\n--- Summary Statistics ---\n")
summary(marketing[, c("Age", "Income", "TotalSpend", "TotalPurchases", "Recency")])

cat("\n--- Response Distribution ---\n")
table(marketing$Response)
prop.table(table(marketing$Response))

# ── 8. EDA — VISUALIZATIONS ──────────────────────────────────

# Figure 1: Age Distribution
ggplot(marketing, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "#2E75B6", color = "white") +
  labs(title = "Figure 1: Age Distribution of Customers",
       x = "Age", y = "Count") +
  theme_minimal(base_size = 13)

# Figure 2: Income Distribution
ggplot(marketing, aes(x = Income)) +
  geom_histogram(binwidth = 5000, fill = "#1F4E79", color = "white") +
  scale_x_continuous(labels = comma) +
  labs(title = "Figure 2: Income Distribution of Customers",
       x = "Annual Income ($)", y = "Count") +
  theme_minimal(base_size = 13)

# Figure 3: Total Spend Distribution
ggplot(marketing, aes(x = TotalSpend)) +
  geom_histogram(binwidth = 100, fill = "#70AD47", color = "white") +
  labs(title = "Figure 3: Total Spending Distribution",
       x = "Total Spend ($)", y = "Count") +
  theme_minimal(base_size = 13)

# Figure 4: Response Rate (Campaign Acceptance)
ggplot(marketing, aes(x = Response, fill = Response)) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "#D9534F", "1" = "#2E75B6")) +
  labs(title = "Figure 4: Campaign Response Distribution",
       x = "Response (0 = No, 1 = Yes)", y = "Count") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

# Figure 5: Total Spend by Response
ggplot(marketing, aes(x = Response, y = TotalSpend, fill = Response)) +
  geom_boxplot() +
  scale_fill_manual(values = c("0" = "#D9534F", "1" = "#2E75B6")) +
  labs(title = "Figure 5: Total Spend by Campaign Response",
       x = "Response (0 = No, 1 = Yes)", y = "Total Spend ($)") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

# Figure 6: Income by Response
ggplot(marketing, aes(x = Response, y = Income, fill = Response)) +
  geom_boxplot() +
  scale_fill_manual(values = c("0" = "#D9534F", "1" = "#2E75B6")) +
  scale_y_continuous(labels = comma) +
  labs(title = "Figure 6: Income by Campaign Response",
       x = "Response (0 = No, 1 = Yes)", y = "Annual Income ($)") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

# Figure 7: Education Level Distribution
ggplot(marketing, aes(x = Education, fill = Education)) +
  geom_bar() +
  labs(title = "Figure 7: Education Level Distribution",
       x = "Education Level", y = "Count") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

# Figure 8: Average Spending by Product Category
spend_cols <- c("MntWines", "MntFruits", "MntMeatProducts",
                "MntFishProducts", "MntSweetProducts", "MntGoldProds")
avg_spend <- colMeans(marketing[, spend_cols])
spend_df  <- data.frame(Category = names(avg_spend), AvgSpend = avg_spend)

ggplot(spend_df, aes(x = reorder(Category, -AvgSpend), y = AvgSpend, fill = Category)) +
  geom_bar(stat = "identity") +
  labs(title = "Figure 8: Average Spending by Product Category",
       x = "Product Category", y = "Average Spend ($)") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 30, hjust = 1))

# Figure 9: Correlation Matrix
numeric_vars <- marketing[, c("Age", "Income", "TotalSpend", "TotalPurchases",
                               "Recency", "NumWebVisitsMonth",
                               "MntWines", "MntFruits", "MntMeatProducts",
                               "MntFishProducts", "MntSweetProducts", "MntGoldProds")]
cor_matrix <- cor(numeric_vars, use = "complete.obs")

corrplot(cor_matrix,
         method = "color",
         type   = "upper",
         tl.cex = 0.8,
         title  = "Figure 9: Correlation Matrix of Numeric Variables",
         mar    = c(0, 0, 2, 0))

# ============================================================
# SECTION 2: K-MEANS CLUSTERING (Jivitesh)
# ============================================================
cat("\n========== K-MEANS CLUSTERING ==========\n")

# Select numeric features for clustering
cluster_vars <- marketing[, c("Income", "TotalSpend", "TotalPurchases",
                               "Age", "Recency", "TotalCampaignsAccepted")]

# Scale features — K-Means is distance-based, scaling is required
cluster_scaled <- scale(cluster_vars)

set.seed(4567)

# ── Figure 10: Elbow Method ───────────────────────────────────
wss <- numeric(10)
for (k in 1:10) {
  km     <- kmeans(cluster_scaled, centers = k, nstart = 25, iter.max = 100)
  wss[k] <- km$tot.withinss
}

elbow_df <- data.frame(k = 1:10, WSS = wss)

ggplot(elbow_df, aes(x = k, y = WSS)) +
  geom_line(color = "#2E75B6", linewidth = 1) +
  geom_point(color = "#1F4E79", size = 3) +
  geom_vline(xintercept = 4, linetype = "dashed", color = "#D9534F") +
  labs(title = "Figure 10: Elbow Method — Optimal K Selection",
       x = "Number of Clusters (K)", y = "Total Within-Cluster SS") +
  theme_minimal(base_size = 13)

# ── Figure 11: Silhouette Analysis ───────────────────────────
sil_scores <- numeric(9)
for (k in 2:10) {
  set.seed(4567)
  km             <- kmeans(cluster_scaled, centers = k, nstart = 25, iter.max = 100)
  sil            <- silhouette(km$cluster, dist(cluster_scaled))
  sil_scores[k - 1] <- mean(sil[, 3])
}

sil_df      <- data.frame(k = 2:10, AvgSilhouette = sil_scores)
optimal_k   <- sil_df$k[which.max(sil_df$AvgSilhouette)]
cat("Optimal K by silhouette:", optimal_k, "\n")

ggplot(sil_df, aes(x = k, y = AvgSilhouette)) +
  geom_line(color = "#70AD47", linewidth = 1) +
  geom_point(color = "#375623", size = 3) +
  geom_vline(xintercept = optimal_k, linetype = "dashed", color = "#D9534F") +
  labs(title = "Figure 11: Silhouette Scores by Number of Clusters",
       x = "Number of Clusters (K)", y = "Average Silhouette Width") +
  theme_minimal(base_size = 13)

# ── Final K-Means Model ───────────────────────────────────────
# K=4 selected based on elbow method; silhouette used for validation
set.seed(4567)
km_final         <- kmeans(cluster_scaled, centers = 4, nstart = 25, iter.max = 100)
marketing$Cluster <- as.factor(km_final$cluster)

cat("Cluster sizes:\n")
print(table(marketing$Cluster))

# ── Figure 12: Cluster Visualization ─────────────────────────
fviz_cluster(km_final,
             data          = cluster_scaled,
             geom          = "point",
             ellipse.type  = "convex",
             ggtheme       = theme_minimal(base_size = 13),
             main          = "Figure 12: K-Means Cluster Plot (PCA-Reduced)")

# ── Cluster Profiling ─────────────────────────────────────────
cat("\n--- Cluster Profile Summary ---\n")
cluster_profile <- marketing %>%
  group_by(Cluster) %>%
  summarise(
    Count          = n(),
    Avg_Income     = round(mean(Income), 0),
    Avg_TotalSpend = round(mean(TotalSpend), 0),
    Avg_Purchases  = round(mean(TotalPurchases), 2),
    Avg_Age        = round(mean(Age), 1),
    Avg_Recency    = round(mean(Recency), 1),
    Avg_Campaigns  = round(mean(TotalCampaignsAccepted), 2),
    Response_Rate  = round(mean(as.numeric(as.character(Response))), 3)
  )

print(as.data.frame(cluster_profile))

# ── Figure 13: Income vs TotalSpend by Cluster ───────────────
ggplot(marketing, aes(x = Income, y = TotalSpend, color = Cluster)) +
  geom_point(alpha = 0.5, size = 1.5) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Figure 13: Income vs Total Spend by Cluster",
       x = "Annual Income ($)", y = "Total Spend ($)") +
  theme_minimal(base_size = 13)

# ============================================================
# SECTION 3: CLASSIFICATION MODELS (Jivitesh)
# ============================================================
cat("\n========== CLASSIFICATION MODELS ==========\n")

# Select features — including individual campaign columns per professor's feedback
# Response is the target; AcceptedCmp1-5 are predictor features
model_data <- marketing %>%
  select(Income, Age, TotalSpend, TotalPurchases, Recency,
         NumWebVisitsMonth, TotalCampaignsAccepted,
         Kidhome, Teenhome,
         MntWines, MntFruits, MntMeatProducts,
         MntFishProducts, MntSweetProducts, MntGoldProds,
         AcceptedCmp1, AcceptedCmp2, AcceptedCmp3,
         AcceptedCmp4, AcceptedCmp5,
         Response) %>%
  mutate(Response = as.factor(Response))

cat("Class distribution:\n")
print(table(model_data$Response))
cat("Class proportions:\n")
print(round(prop.table(table(model_data$Response)), 3))

# ── Train/Test Split (70/30) ──────────────────────────────────
set.seed(4567)
trainIndex <- createDataPartition(model_data$Response, p = 0.70, list = FALSE)
train_data  <- model_data[ trainIndex, ]
test_data   <- model_data[-trainIndex, ]

cat("Training rows:", nrow(train_data), "| Test rows:", nrow(test_data), "\n")

# Rename factor levels for caret compatibility
levels(train_data$Response) <- c("No", "Yes")
levels(test_data$Response)  <- c("No", "Yes")

# ── Cross-Validation Control ──────────────────────────────────
ctrl <- trainControl(method          = "cv",
                     number          = 10,
                     classProbs      = TRUE,
                     summaryFunction = twoClassSummary,
                     savePredictions = "final")

# ── Figure 14 & 15: Decision Tree ────────────────────────────
cat("\n--- Decision Tree ---\n")
set.seed(4567)
dt_model <- train(Response ~ .,
                  data       = train_data,
                  method     = "rpart",
                  metric     = "ROC",
                  trControl  = ctrl,
                  tuneLength = 10)

rpart.plot(dt_model$finalModel,
           type  = 4,
           extra = 104,
           main  = "Figure 14: Decision Tree",
           cex   = 0.7)

dt_pred <- predict(dt_model, newdata = test_data)
dt_cm   <- confusionMatrix(dt_pred, test_data$Response,
                            mode = "everything", positive = "Yes")
cat("Decision Tree Confusion Matrix:\n")
print(dt_cm)

cat("\nDecision Tree — Variable Importance:\n")
dt_imp <- varImp(dt_model, scale = TRUE)
print(dt_imp)
plot(dt_imp, top = 10,
     main = "Figure 15: Decision Tree — Top 10 Variable Importances")

# ── Figure 16 & 17: KNN ──────────────────────────────────────
cat("\n--- KNN ---\n")
set.seed(4567)
knn_model <- train(Response ~ .,
                   data       = train_data,
                   method     = "knn",
                   metric     = "ROC",
                   trControl  = ctrl,
                   preProcess = c("center", "scale"),
                   tuneGrid   = expand.grid(k = seq(3, 21, by = 2)))

cat("Best K:", knn_model$bestTune$k, "\n")

plot(knn_model,
     main = "Figure 16: KNN — Cross-Validated ROC by K")

knn_pred <- predict(knn_model, newdata = test_data)
knn_cm   <- confusionMatrix(knn_pred, test_data$Response,
                             mode = "everything", positive = "Yes")
cat("KNN Confusion Matrix:\n")
print(knn_cm)

cat("\nKNN — Variable Importance:\n")
knn_imp <- varImp(knn_model, scale = TRUE)
print(knn_imp)
plot(knn_imp, top = 10,
     main = "Figure 17: KNN — Top 10 Variable Importances")

# ── Figure 18: Logistic Regression ───────────────────────────
cat("\n--- Logistic Regression ---\n")
set.seed(4567)
lr_model <- train(Response ~ .,
                  data       = train_data,
                  method     = "glm",
                  family     = "binomial",
                  metric     = "ROC",
                  trControl  = ctrl,
                  preProcess = c("center", "scale"))

cat("Logistic Regression Coefficients:\n")
print(summary(lr_model$finalModel))

lr_pred <- predict(lr_model, newdata = test_data)
lr_cm   <- confusionMatrix(lr_pred, test_data$Response,
                            mode = "everything", positive = "Yes")
cat("Logistic Regression Confusion Matrix:\n")
print(lr_cm)

cat("\nLogistic Regression — Variable Importance:\n")
lr_imp <- varImp(lr_model, scale = TRUE)
print(lr_imp)
plot(lr_imp, top = 10,
     main = "Figure 18: Logistic Regression — Top 10 Variable Importances")

# ── Figure 19: Model Comparison ──────────────────────────────
cat("\n========== MODEL COMPARISON ==========\n")

extract_metrics <- function(cm, model_name) {
  data.frame(
    Model       = model_name,
    Accuracy    = round(cm$overall["Accuracy"],    4),
    Kappa       = round(cm$overall["Kappa"],       4),
    Sensitivity = round(cm$byClass["Sensitivity"], 4),
    Specificity = round(cm$byClass["Specificity"], 4),
    Precision   = round(cm$byClass["Precision"],   4),
    F1          = round(cm$byClass["F1"],          4),
    row.names   = NULL
  )
}

comparison_table <- rbind(
  extract_metrics(dt_cm,  "Decision Tree"),
  extract_metrics(knn_cm, "KNN"),
  extract_metrics(lr_cm,  "Logistic Regression")
)

cat("\nModel Comparison Table:\n")
print(comparison_table)

comp_long <- reshape(comparison_table,
                     varying   = c("Accuracy", "F1"),
                     v.names   = "Value",
                     timevar   = "Metric",
                     times     = c("Accuracy", "F1"),
                     direction = "long")

ggplot(comp_long, aes(x = Model, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Accuracy" = "#2E75B6", "F1" = "#70AD47")) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = "Figure 19: Model Comparison — Accuracy & F1 Score",
       x = "Model", y = "Score") +
  theme_minimal(base_size = 13) +
  theme(legend.title = element_blank())

best_model <- comparison_table$Model[which.max(comparison_table$F1)]
cat("\nBest model by F1 Score:", best_model, "\n")

# ── END OF COMPLETE SCRIPT ────────────────────────────────────
