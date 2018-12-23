#===============================================================================
# 09-supporting-info-C.R
# Purpose: to replicate Table 3 of the paper, where we provide a summary of the
#           hitchhiker discovered at each stage of the discovering process, as
#           well as information about the best performing models.
# Article: "More Effective Than We Thought: Accounting for Legislative 
#           Hitchhikers Reveals a More Inclusive and Productive Lawmaking 
#           Process."
# Journal: American Journal of Political Science
# Year: 2018
# Authors: Andreu Casas, Matt Denny, and John Wilkerson
#===============================================================================

# PACKAGES
#===============================================================================
# - install packages if needed
install.packages("dplyr")

# - load the packages
library(dplyr)

# PATHS & CONSTANTS
#===============================================================================
# - path to where the predictions made during the hitchhiker discovering process
#   are located
pred_path <- "../data/predictions/"

# MAIN
#===============================================================================
# - initializing final summary dataset
results <- NULL

# ========== Iteration 1 ===========

iter = 1

# - reading the training set and splitting the set into True positives and True
#   Negatives
labs_db <- read.csv("../data/hr146_bi80_uni90_labeled.csv")
Tpos <- as.numeric(table(labs_db$true_match_num2)[2])
Tneg <- as.numeric(table(labs_db$true_match_num2)[1])

# - reading the information about the models and parameters in stage 1 and 2 of
#   the discovering process
# ... the first best models selected in stage 01 (before adding extra constraints)
best_models <- read.csv(paste0(pred_path, "best_models1.csv"))
stage1_n <- nrow(best_models)
# ... applying the extra constraints to select the best performing models in 
#     stage 1
stage1_par <- min(c(min(best_models$precision_pe),
                    min(best_models$recall_pe)))
# ... reading the information for these best performing models in stage 1
best_best_models <- read.csv(paste0(pred_path, "best_best_models1.csv"))
stage2_n <- nrow(best_best_models)
stage2_par <- 10

# ... reading the information about the hitchhiker predicted in the first stage
preds <- read.csv(paste0(pred_path, "hitchhiker_predictions_stage01.csv"))
preds_n <- nrow(preds)

# ... reading the information about the training set in the second stage of the
#     process
next_labs_n <- nrow(read.csv(paste0(pred_path, "labs_db_iter_2a.csv")))
new_preds_n <- next_labs_n - nrow(labs_db)
old_preds_n <- preds_n - new_preds_n

# ... calculating ensemble precision and recall
ensemble_pr <- NA
ensemble_rec <- NA
new_row <- data.frame(
  iter = iter, 
  training_size = Tpos + Tneg,
  Tpos = Tpos,
  Tneg = Tneg,
  stage1_n = stage1_n,
  stage1_par = stage1_par,
  stage2_n = stage2_n,
  stage2_par = stage2_par,
  preds_n = preds_n,
  old_preds_n = old_preds_n,
  new_preds_n = new_preds_n,
  ensemble_pr = ensemble_pr,
  ensemble_rec = ensemble_rec
)
results <- rbind(results, new_row)

# - building the training set for the next iteration of the process (#2)
labs_db$BillID_a <-  as.character(sapply(as.character(labs_db$version_a), function(x)
  paste0(strsplit(x, split = "-")[[1]][1:3], collapse = "-")))
labs_db$BillID_b <-  as.character(sapply(as.character(labs_db$version_b), function(x)
  paste0(strsplit(x, split = "-")[[1]][1:3], collapse = "-")))
labs_db$comp2 <- paste0(labs_db$BillID_a, "&", labs_db$BillID_b)


# ======== Iteration 2 to 4 ========
# - looping through the data of iter 2, 3, and 4.
for (i in 2:4){
  iter = i
  fname = paste0("iter_", iter, ".csv")
  
  # - training set
  labs_db <- read.csv(paste0(pred_path, "labs_db_", fname))
  Tpos <- as.numeric(table(labs_db$true_match_num2)[2])
  Tneg <- as.numeric(table(labs_db$true_match_num2)[1])
  
  # - models and parameters for this stage
  best_models <- read.csv(paste0(pred_path, "best_models_", fname))
  stage1_n <- nrow(best_models)
  stage1_par <- min(c(min(best_models$precision_pe),
                      min(best_models$recall_pe)))
  best_best_models <- read.csv(paste0(pred_path, "best_best_models_", fname))
  stage2_n <- nrow(best_best_models)
  stage2_par <- max(best_best_models$bill_mult_match)
  
  # - predictions: old and new
  preds <- read.csv(paste0(pred_path, "ensemble_preds_", fname))
  preds_n <- nrow(preds)
  if (!("comp2" %in% names(labs_db))) {
    labs_db$version_a <- as.character(sapply(as.character(labs_db$comp), function(x)
      strsplit(x, split = "&")[[1]][1]))
    labs_db$version_b <- as.character(sapply(as.character(labs_db$comp), function(x)
      strsplit(x, split = "&")[[1]][2]))
    labs_db$BillID_a <-  as.character(sapply(as.character(labs_db$version_a), function(x)
      paste0(strsplit(x, split = "-")[[1]][1:3], collapse = "-")))
    labs_db$BillID_b <-  as.character(sapply(as.character(labs_db$version_b), function(x)
      paste0(strsplit(x, split = "-")[[1]][1:3], collapse = "-")))
    labs_db$comp2 <- paste0(labs_db$BillID_a, "&", labs_db$BillID_b)
  }
  labs_db2 <- labs_db %>% dplyr::select(comp2, true_match_num2)
  labs_db2$comp2 <- as.character(labs_db2$comp2)
  preds$comp2 <- as.character(preds$comp2)
  preds <- left_join(preds, labs_db2)
  preds <- preds %>%
    mutate(true_match_num2 = ifelse(is.na(true_match_num2), 99, true_match_num2))
  new_preds_n <- length(which(preds$true_match_num2 == 99))
  old_preds_n <- nrow(preds) - new_preds_n
  
  # - calculating ensemble precision and recall
  crossval <- read.csv(paste0(pred_path, "crossval_res_", fname))
  ensemble_pr <- paste0(crossval$pr_pe, "% {", round(crossval$pr_lwr,2), "-", 
                        round(crossval$pr_upr,2), "}")
  ensemble_rec <- paste0(crossval$rec_pe, "% {", round(crossval$rec_lwr,2), "-", 
                         round(crossval$rec_upr,2), "}")
  new_row <- data.frame(
    iter = iter, 
    training_size = Tpos + Tneg,
    Tpos = Tpos,
    Tneg = Tneg,
    stage1_n = stage1_n,
    stage1_par = stage1_par,
    stage2_n = stage2_n,
    stage2_par = stage2_par,
    preds_n = preds_n,
    old_preds_n = old_preds_n,
    new_preds_n = new_preds_n,
    ensemble_pr = ensemble_pr,
    ensemble_rec = ensemble_rec
  )
  results <- rbind(results, new_row)
}

# TABLE: information to plug into and replicate Table 3 of the article, in 
#         Supporting Information C.
#===============================================================================
print(results)
