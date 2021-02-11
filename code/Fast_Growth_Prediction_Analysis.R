#########################################################################################
# Prepared for Bruno and Maeva's Data Analysis
#
# DA3 Assignment 2:Finding fast growing firms
#########################################################################################



# ------------------------------------------------------------------------------------------------------
#### SET UP
# It is advised to start a new session for every case study
# CLEAR MEMORY
rm(list=ls())

# Import libraries
library(haven)
library(glmnet)
library(purrr)
library(margins)
library(skimr)
library(kableExtra)
library(Hmisc)
library(cowplot)
library(gmodels) 
library(lspline)
library(sandwich)
library(modelsummary)
library(viridis)
library(rattle)
library(caret)
library(pROC)
library(ranger)
library(rpart)
library(partykit)
library(rpart.plot)
library(dplyr)


# set working directory
setwd("C:/Users/helme/Desktop/CEU/WINTER_Term/DA3")
data_dir <- paste0(getwd(),"/DA3_A2_Bruno_Maeva")


# load theme and functions
SourceFunctions <- paste0("https://raw.githubusercontent.com/Maeva2408/DA3_A2_Bruno_Maeva/main/code/functions/")
source(paste0(SourceFunctions,"theme_bg.R"))
source(paste0(SourceFunctions,"da_helper_functions.R"))

data_in <- paste(data_dir,"data/clean/", sep = "/")
output <- paste0(data_dir,"/output/")
create_output_if_doesnt_exist(output)
data_out <- paste(data_dir,"data/clean/", sep = "/")
#-----------------------------------------------------------------------------------------

# THIS IS THE SECOND PART OF THE ch17 CODE
# USES INTERMEDIATE OUTPUT by ch17-firm-exit-data-prep.R


# Loading and preparing data ----------------------------------------------
url <- "https://raw.githubusercontent.com/Maeva2408/DA3_A2_Bruno_Maeva/main/data/clean/bisnode_firms_clean.rds"
data <- readRDS(url(url, method="libcurl"))

# Define variable sets ----------------------------------------------
# (making sure we use ind2_cat, which is a factor)

firm <- c("age", "age2", "new", "ind2_cat", "m_region_loc", "urban_m")

Fin1 <-  c("curr_assets", "curr_liab", "extra_exp", 
           "extra_inc", "extra_profit_loss", "fixed_assets",
           "inc_bef_tax", "intang_assets", "inventories", 
           "liq_assets", "material_exp", "personnel_exp",
           "profit_loss_year", "sales", "share_eq", "subscribed_cap",
           "tang_assets","amort", "EBITDA")

Fin2 <- c("extra_profit_loss_pl_quad", "inc_bef_tax_pl_quad",
          "profit_loss_year_pl_quad", "share_eq_bs_quad",
          c(grep("*flag_low$", names(data), value = TRUE),
            grep("*flag_high$", names(data), value = TRUE),
            grep("*flag_error$", names(data), value = TRUE),
            grep("*flag_zero$", names(data), value = TRUE)))

Fin3 <- c("total_assets_bs", "fixed_assets_bs", "liq_assets_bs", "curr_assets_bs",
          "share_eq_bs", "subscribed_cap_bs", "intang_assets_bs", "extra_exp_pl",
          "extra_inc_pl", "extra_profit_loss_pl", "inc_bef_tax_pl", "inventories_pl",
          "material_exp_pl", "profit_loss_year_pl", "personnel_exp_pl","working_capital_TO",
          "EBITDA_pl")

Growth <- data %>% select(matches("^d1_")) %>% select(-matches(Fin2))%>% colnames()

HR <- c("ceo_age", "ceo_young","flag_high_ceo_age", "flag_low_ceo_age",
        "flag_miss_ceo_age", "ceo_count", "labor_avg_mod",
        "flag_miss_labor_avg", "foreign_management")

qualityvars <- c("balsheet_flag", "balsheet_length", "balsheet_notfullyear")

data %>% select(-matches(c(firm,Fin1,Fin2,Fin3,Growth,HR,qualityvars))) %>% colnames()


# interactions for logit, LASSO
interactions1 <- c("ind2_cat*age", "ind2_cat*age2",
                   "ind2_cat*d1_sales_mil_log_mod", "ind2_cat*sales_mil_log",
                   "ind2_cat*ceo_age", "ind2_cat*foreign_management",
                   "ind2_cat*female",   "ind2_cat*urban_m", "ind2_cat*labor_avg_mod",
                   paste0("ind2_cat*", Growth))

interactions2 <- c("sales_mil_log*age", "sales_mil_log*female",
                   "sales_mil_log*profit_loss_year_pl", "sales_mil_log*foreign_management")

X1 <- c("sales_mil_log", "sales_mil_log_sq", "d1_sales_mil_log_mod", "profit_loss_year_pl", "ind2_cat")
X2 <- c(X1, "fixed_assets_bs","share_eq_bs","curr_liab_bs ",   "curr_liab_bs_flag_high ", 
        "curr_liab_bs_flag_error",  "age","foreign_management" )

X3 <- c("sales_mil_log", "sales_mil_log_sq", firm, Fin1, Fin3, Growth)
X4 <- c("sales_mil_log", "sales_mil_log_sq", firm, Fin1, Fin3, Growth, HR, qualityvars, Fin2)
X5 <- c("sales_mil_log", "sales_mil_log_sq", firm, Fin1, Fin3, Growth, HR, qualityvars, Fin2, interactions1, interactions2)

# separate datasets -------------------------------------------------------


table(round(data$CAGR,0))

library(ggthemes)
ggplot(data,aes(x = CAGR)) +
  geom_histogram(binwidth = 5, color = "orangered4", fill = "salmon") + 
  geom_vline( xintercept = 25, color = "coral4", linetype = "dashed") +
  scale_x_continuous(limits = c(-100,400), breaks = c(seq(-100,400,100),25)) +
  theme_tufte() + labs(title = "CAGR % Sample Distribution & Cut-off Point ",
                       y = "Frequency among Firms", y = "CAGR % ")

set.seed(13505)
train_indices <- as.integer(createDataPartition(data$HyperGrowth, p = 0.8, list = FALSE))
data_train <- data[train_indices, ]
data_holdout <- data[-train_indices, ]

dim(data_train)
dim(data_holdout)

Hmisc::describe(data$HyperGrowth_f)
Hmisc::describe(data_train$HyperGrowth_f)
Hmisc::describe(data_holdout$HyperGrowth_f)

#######################################################x
# PART I PREDICT PROBABILITIES
# Predict logit models ----------------------------------------------
#######################################################x

# 5 fold cross-validation
train_control <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummaryExtended,
  savePredictions = TRUE
)


# Train Logit Models ----------------------------------------------

logit_model_vars <- list("X1" = X1, "X2" = X2, "X3" = X3, "X4" = X4, "X5" = X5)

CV_RMSE_folds <- list()
logit_models <- list()
data$HyperGrowth_f <- factor( data$HyperGrowth_f, levels = c("no_Hyp.Growth","Hyp.Growth"), 
        labels = c("no_Hyp.Growth" = 0,"Hyp.Growth" = 1))
for (model_name in names(logit_model_vars)) {

  features <- logit_model_vars[[model_name]]

  set.seed(13505)
  glm_model <- train(
    formula(paste0("HyperGrowth_f ~", paste0(features, collapse = " + "))),
    method = "glm",
    data = data_train,
    family = binomial,
    trControl = train_control
  )

  logit_models[[model_name]] <- glm_model
  # Calculate RMSE on test for each fold
  CV_RMSE_folds[[model_name]] <- glm_model$resample[,c("Resample", "RMSE")]

}

CV_RMSE_folds
logit_models

# Logit lasso -----------------------------------------------------------
logitvars <- c("sales_mil_log", "sales_mil_log_sq", 
               firm, Fin3, Fin2, Growth, HR, qualityvars, interactions1, interactions2)

lambda <- 10^seq(-1, -4, length = 10)
grid <- expand.grid("alpha" = 1, lambda = lambda)

set.seed(13505)
system.time({
  logit_lasso_model <- train(
    formula(paste0("HyperGrowth_f ~", paste0(logitvars, collapse = " + "))),
    data = data_train,
    method = "glmnet",
    preProcess = c("center", "scale"),
    family = "binomial",
    trControl = train_control,
    tuneGrid = grid,
    na.action=na.exclude
  )
})

tuned_logit_lasso_model <- logit_lasso_model$finalModel
best_lambda <- logit_lasso_model$bestTune$lambda
logit_models[["LASSO"]] <- logit_lasso_model
lasso_coeffs <- as.matrix(coef(tuned_logit_lasso_model, best_lambda))
#write.csv(lasso_coeffs, paste0(output, "lasso_logit_coeffs.csv"))

CV_RMSE_folds[["LASSO"]] <- logit_lasso_model$resample[,c("Resample", "RMSE")]
CV_RMSE_folds[["LASSO"]]


logit_models
CV_RMSE_folds

# Random Probability Forest --------------------------------------

#################################################
# Probability forest
# Split by gini, ratio of 1's in each tree, average over trees
#################################################
# for RF (no interactions, no modified features)
# Variables from Best Logit-Model -> NO INTERACTIONS

#mean(CV_RMSE_folds[[1]][,2])

rbindlist(lapply(1:6,function(x) {
  tl <- list()
  tl[['mean']] <- mean(CV_RMSE_folds[[x]][,2])
  return(tl)
}))

library(data.table)

# 5 fold cross-validation
rfvars  <-  X4

train_control <- trainControl(
  method = "cv",
  n = 5,
  classProbs = TRUE, # same as probability = TRUE in ranger
  summaryFunction = twoClassSummaryExtended,
  savePredictions = TRUE
)
train_control$verboseIter <- TRUE

tune_grid <- expand.grid(
  .mtry = c( 9, 10, 11),
  .splitrule = "gini",
  .min.node.size = c(10, 15)
)

# getModelInfo("ranger")
set.seed(13505)
rf_model_p <- train(
  formula(paste0("HyperGrowth_f ~ ", paste0(rfvars , collapse = " + "))),
  method = "ranger",
  data = data_train,
  tuneGrid = tune_grid,
  trControl = train_control
)

rf_model_p$results

best_mtry <- rf_model_p$bestTune$mtry
best_min_node_size <- rf_model_p$bestTune$min.node.size
logit_models[["RandForest"]] <- rf_model_p
CV_RMSE_folds[["RandForest"]] <- rf_model_p$resample[,c("Resample", "RMSE")]

# Get average (ie over the folds) RMSE and AUC ------------------------------------

# AUC
# Draw ROC Curve and calculate AUC for each folds --------------------------------
CV_AUC_folds <- list()

for (model_name in names(logit_models)) {
  
  auc <- list()
  model <- logit_models[[model_name]]
  for (fold in c("Fold1", "Fold2", "Fold3", "Fold4", "Fold5")) {
    cv_fold <-
      model$pred %>%
      filter(Resample == fold)
    
    roc_obj <- roc(cv_fold$obs, cv_fold$Hyp.Growth)
    auc[[fold]] <- as.numeric(roc_obj$auc)
  }
  CV_AUC_folds[[model_name]] <- data.frame("Resample" = names(auc),
                                           "AUC" = unlist(auc))
}

CV_AUC_folds
CV_RMSE_folds

# For each model: average RMSE and average AUC for models ----------------------------------
CV_RMSE <- list()
CV_AUC <- list()
for (model_name in names(logit_models)) {
  CV_RMSE[[model_name]] <- mean(CV_RMSE_folds[[model_name]]$RMSE)
  CV_AUC[[model_name]] <- mean(CV_AUC_folds[[model_name]]$AUC)
}
CV_RMSE
CV_AUC 

# Model Selection ---------------------------------

# 7 models, (5 logit , 1-1 logit LASSO & RF). For each we have a 5-CV RMSE and AUC.
# Nice Summary Table -------------------

nvars <- lapply(logit_models, FUN = function(x) length(x$coefnames))
nvars[["LASSO"]] <- sum(lasso_coeffs != 0)

logit_summary1 <- data.frame("Number of predictors" = unlist(nvars),
                             "CV RMSE" = unlist(CV_RMSE),
                             "CV AUC" = unlist(CV_AUC))

# Take best model and estimate RMSE on holdout  -------------------------------------------

best_logit_no_loss <- logit_models[["RandForest"]]

logit_predicted_probabilities_holdout <- predict(best_logit_no_loss, newdata = data_holdout, type = "prob")
data_holdout[,"best_logit_no_loss_pred"] <- logit_predicted_probabilities_holdout[,"Hyp.Growth"]
RMSE(data_holdout[, "best_logit_no_loss_pred", drop=TRUE], data_holdout$HyperGrowth)

# discrete ROC (with thresholds in steps) on holdout -------------------------------------------------
thresholds <- seq(0.05, 0.75, by = 0.01)

cm <- list()
true_positive_rates <- c()
false_positive_rates <- c()

#thr <- thresholds[1]
for (thr in thresholds) {
  holdout_prediction <- ifelse(data_holdout[,"best_logit_no_loss_pred"] < thr, "no_Hyp.Growth", "Hyp.Growth") %>%
    factor(levels = c("no_Hyp.Growth", "Hyp.Growth"))
  cm_thr <- confusionMatrix(holdout_prediction,data_holdout$HyperGrowth_f)$table
  cm[[as.character(thr)]] <- cm_thr
  true_positive_rates <- c(true_positive_rates, cm_thr["Hyp.Growth", "Hyp.Growth"] /
                             (cm_thr["Hyp.Growth", "Hyp.Growth"] + cm_thr["no_Hyp.Growth", "Hyp.Growth"]))
  false_positive_rates <- c(false_positive_rates, cm_thr["Hyp.Growth", "no_Hyp.Growth"] /
                              (cm_thr["Hyp.Growth", "no_Hyp.Growth"] + cm_thr["no_Hyp.Growth", "no_Hyp.Growth"]))
}

tpr_fpr_for_thresholds <- tibble(
  "threshold" = thresholds,
  "true_positive_rate" = true_positive_rates,
  "false_positive_rate" = false_positive_rates
)

discrete_roc_plot <- ggplot(
  data = tpr_fpr_for_thresholds,
  aes(x = false_positive_rate, y = true_positive_rate, color = threshold)) +
  labs(x = "False positive rate (1 - Specificity)", y = "True positive rate (Sensitivity)") +
  geom_point(size=2, alpha=0.8) +
  scale_color_viridis(option = "D", direction = -1) +
  scale_x_continuous(expand = c(0.01,0.01), limit=c(0,1), breaks = seq(0,1,0.1)) +
  scale_y_continuous(expand = c(0.01,0.01), limit=c(0,1), breaks = seq(0,1,0.1)) +
  theme_bg() +
  theme(legend.position ="right") +
  theme(legend.title = element_text(size = 4), 
        legend.text = element_text(size = 4),
        legend.key.size = unit(.4, "cm")) 
discrete_roc_plot
save_fig("ch17-figure-2a-roc-discrete", output, "small")

# continuous ROC on holdout with best model (Logit 4) -------------------------------------------

roc_obj_holdout <- roc(data_holdout$HyperGrowth_f, data_holdout$best_logit_no_loss_pred)

createRocPlot(roc_obj_holdout, "best_logit_no_loss_roc_plot_holdout")

# Confusion table with different tresholds ----------------------------------------------------------

# default: the threshold 0.5 is used to convert probabilities to binary classes
logit_class_prediction <- predict(best_logit_no_loss, newdata = data_holdout)
summary(logit_class_prediction)

# confusion matrix: summarize different type of errors and successfully predicted cases
# positive = "yes": explicitly specify the positive case
cm_object1 <- confusionMatrix(logit_class_prediction, data_holdout$HyperGrowth_f, positive = "Hyp.Growth")
cm1 <- cm_object1$table
cm1

# we can apply different thresholds

# 0.5 same as before
holdout_prediction <-
  ifelse(data_holdout$best_logit_no_loss_pred < 0.5, "no_default", "default") %>%
  factor(levels = c("no_default", "default"))
cm_object1b <- confusionMatrix(holdout_prediction,data_holdout$default_f)
cm1b <- cm_object1b$table
cm1b

# a sensible choice: mean of predicted probabilities
mean_predicted_default_prob <- mean(data_holdout$best_logit_no_loss_pred)
mean_predicted_default_prob
holdout_prediction <-
  ifelse(data_holdout$best_logit_no_loss_pred < mean_predicted_default_prob, "no_default", "default") %>%
  factor(levels = c("no_default", "default"))
cm_object2 <- confusionMatrix(holdout_prediction,data_holdout$default_f)
cm2 <- cm_object2$table
cm2






# Calibration curve -----------------------------------------------------------
# how well do estimated vs actual event probabilities relate to each other?

#data_holdout$best_logit_with_loss_pred
  create_calibration_plot(data_holdout, 
  file_name = "ch17-figure-1-logit-m4-calibration", 
  prob_var = "best_logit_no_loss_pred", 
  actual_var = "default",
  n_bins = 10)


#############################################x
# PART II.
# We have a loss function
################### W LOSS FUNCTION #####################

# Introduce loss function ------------------
# relative cost of of a false negative classification (as compared with a false positive classification)
 # False Positive: 
    # We invest but should not have -> 20% to loss capital & did not invest in correct firm = -2.2
 # False Negative: 
    # We dont invest but we should have -> we dont triple our money =
 # Scale it to whole numbers -> 10-11
FP=3  
FN=5 
cost <- FN/FP

# CAGR for Hypergrowth companies -> triple in size in 2 years 
data[data$HyperGrowth == 1,] %>% summarize(AvgCAGR = round(mean(CAGR),2))

# CAGR for Non-Hypergrowth companies -> 
  #   No default -> no loss no gain
  #   Default   -> all money lost - ca. 20% among no hypergrowth firms
data[(data$HyperGrowth == 0) && (data$default == 0),] %>% summarize(AvgCAGR = round(mean(CAGR),2))
prop.table(table(data$default,data$HyperGrowth),2)
  # this would mean O * 0.8 - 1 * 0.2 = -0.2
    # we also could have used our money else where -> to triple it by giving it to hypergrowth firm so:
  # 0 * 0.8 - 1 * 0.2 - 2 = -2.2

# the prevalence, or the proportion of cases in the population (n.cases/(n.controls+n.cases))
prevelance = sum(data_train$HyperGrowth_f == "Hyp.Growth")/length(data_train$HyperGrowth_f)

# Draw ROC Curve and find optimal threshold with loss function --------------------------

best_tresholds <- list()
expected_loss <- list()
logit_cv_rocs <- list()
logit_cv_threshold <- list()
logit_cv_expected_loss <- list()

#model_name <- "LASSO"

for (model_name in names(logit_models)) {
  model <- logit_models[[model_name]]
  colname <- paste0(model_name,"_prediction")
  best_tresholds_cv <- list()
  expected_loss_cv <- list()

#  fold <- "Fold5"
  for (fold in c("Fold1", "Fold2", "Fold3", "Fold4", "Fold5")) {
    cv_fold <-
      model$pred %>%
      filter(Resample == fold)

    roc_obj <- roc(cv_fold$obs, cv_fold$Hyp.Growth)
    best_treshold <- coords(roc_obj, "best", ret="all", transpose = FALSE,
                            best.method="youden", best.weights=c(cost, prevelance))
    best_tresholds_cv[[fold]] <- best_treshold$threshold
    expected_loss_cv[[fold]] <- (best_treshold$fp*FP + best_treshold$fn*FN)/length(cv_fold$Hyp.Growth)
  }

  # average
  best_tresholds[[model_name]] <- mean(unlist(best_tresholds_cv)[is.finite(unlist(best_tresholds_cv)) == T])
  expected_loss[[model_name]] <- mean(unlist(expected_loss_cv)[is.finite(unlist(expected_loss_cv)) == T])

  # for fold #5
  logit_cv_rocs[[model_name]] <- roc_obj
  logit_cv_threshold[[model_name]] <- best_treshold
  logit_cv_expected_loss[[model_name]] <- expected_loss_cv[[fold]]

  }

logit_summary2 <- data.frame("Avg of optimal thresholds" = unlist(best_tresholds),
                             "Threshold for Fold5" = sapply(logit_cv_threshold, function(x) {x$threshold}),
                             "Avg expected loss" = unlist(expected_loss),
                             "Expected loss for Fold5" = unlist(logit_cv_expected_loss))

kable(x = logit_summary2, format = "latex", booktabs=TRUE,  digits = 3, row.names = TRUE,
      linesep = "", col.names = c("Avg of optimal thresholds","Threshold for fold #5",
                                  "Avg expected loss","Expected loss for fold #5")) %>%
  cat(.,file= paste0(output, "logit_summary1.tex"))

# ROC / AUC visuals  - Create plots based on Fold5 in CV ----------------------------------------------

for (model_name in names(logit_cv_rocs)) {

  r <- logit_cv_rocs[[model_name]]
  best_coords <- logit_cv_threshold[[model_name]]
  createLossPlot(r, best_coords,
                 paste0(model_name, "_loss_plot"))
  createRocPlotWithOptimal(r, best_coords,
                           paste0(model_name, "_roc_plot"))
}

# Pick best model based on average expected loss ----------------------------------

best_logit_with_loss <- logit_models[["X4"]]
best_logit_optimal_treshold <- best_tresholds[["X4"]]

logit_predicted_probabilities_holdout <- predict(best_logit_with_loss, newdata = data_holdout, type = "prob")
data_holdout[,"best_logit_with_loss_pred"] <- logit_predicted_probabilities_holdout[,"Hyp.Growth"]

# ROC curve on holdout
roc_obj_holdout <- roc(data_holdout$HyperGrowth, data_holdout[, "best_logit_with_loss_pred", drop=TRUE])

# Get expected loss on holdout
holdout_treshold <- coords(roc_obj_holdout, x = best_logit_optimal_treshold, input= "threshold",
                           ret="all", transpose = FALSE)

expected_loss_holdout <- (holdout_treshold$fp*FP + holdout_treshold$fn*FN)/length(data_holdout$HyperGrowth)
expected_loss_holdout

hist(data_holdout$best_logit_with_loss_pred)

# Confusion table on holdout with optimal threshold
data_holdout$holdout_prediction <-
  ifelse(data_holdout$best_logit_with_loss_pred < best_logit_optimal_treshold, "no_Hyp.Growth", "Hyp.Growth") %>%
  factor(levels = c("no_Hyp.Growth", "Hyp.Growth"))
cm_object3 <- confusionMatrix(data_holdout$holdout_prediction,data_holdout$HyperGrowth_f)
cm3 <- cm_object3$table
cm3

# Benchmark Model --------------------
summary_results
benchmark_model <- logit_models[["X1"]]
benchmark_threshold <- best_tresholds[["X1"]]

benchmark_predicted_probabilities_holdout <- predict(benchmark_model, newdata = data_holdout, type = "prob")
data_holdout[,"benchmark_pred"] <- benchmark_predicted_probabilities_holdout[,"Hyp.Growth"]

data_holdout$holdout_prediction_benchmark <-
  ifelse(data_holdout$benchmark_pred < benchmark_threshold, "no_Hyp.Growth", "Hyp.Growth") %>%
  factor(levels = c("no_Hyp.Growth", "Hyp.Growth"))
cm_object_benchmark <- confusionMatrix(data_holdout$holdout_prediction_benchmark,data_holdout$HyperGrowth_f)
cm4 <- cm_object_benchmark$table
cm4


# Average Growth among invested companies
data_holdout[data_holdout$holdout_prediction == "Hyp.Growth",] %>% group_by(HyperGrowth_f) %>% 
  summarize(AvgAnnGrowth = mean(CAGR))

# Loss
 (21 * 10000 * (1 - 0.29)^2) - 210000  
# Win 
 (21 * 10000 * (1+ 0.85)^2) - 210000


#table(data_holdout$ind)


data_holdout[data_holdout$holdout_prediction == "Hyp.Growth",] %>% 
  group_by(HyperGrowth_f,ind2) %>% 
  summarize(AvgAnnGrowth = mean(CAGR),
            Nr_Investments = n())

# Summary results ---------------------------------------------------

summary_results <- data.frame("Number of predictors" = unlist(nvars),
                              "CV RMSE" = unlist(CV_RMSE),
                              "CV AUC" = unlist(CV_AUC),
                              "CV threshold" = unlist(best_tresholds),
                              "CV expected Loss" = unlist(expected_loss))

#model_names <- c("Logit X1", "Logit X4",
#                 "Logit LASSO","RF probability")
#summary_results <- summary_results %>%
#  filter(rownames(.) %in% c("X1", "X4", "LASSO", "rf_p"))
#rownames(summary_results) <- model_names

kable(x = summary_results, format = "latex", booktabs=TRUE,  digits = 3, row.names = TRUE,
      linesep = "", col.names = c("Number of predictors", "CV RMSE", "CV AUC",
                                  "CV threshold", "CV expected Loss")) %>%
  cat(.,file= paste0(output, "summary_results.tex"))



