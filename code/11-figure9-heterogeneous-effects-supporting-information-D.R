#===============================================================================
# 11-figure9-heterogeneous-effects-supporting-information-D.R
# Purpose: to replicate Figure 9 in Supporting Information D, where we show that
#           the key model coefficients do not vary much by Congress, 
#           strengthening the robustness of our findings.
# Article: "More Effective Than We Thought: Accounting for Legislative 
#           Hitchhikers Reveals a More Inclusive and Productive Lawmaking 
#           Process."
# Journal: American Journal of Political Science
# Year: 2018
# Authors: Andreu Casas, Matt Denny, and John Wilkerson
#===============================================================================

# PACKAGES
#===============================================================================
library(dplyr)
library(foreign)
library(rio)
library(tidyr)
library(readstata13)
library(ggplot2)
source("./code/00-functions.R")

# DATA
#===============================================================================
# - loading the dataset used for data modeling
db <- import("./data/main_db.csv") %>%
  filter(ImpBill == 1)

# DATA WRANGLING
#===============================================================================
# - the dataset for the first model predicting the probability of a bill to be 
#   enacted as stand alone LAW
model_data_01 <- db %>%
  mutate(outcome = ifelse(outcome1 == "law", 1, 0))

# - the dataset for the second model predicting the probability of a bill to be 
#   enacted as HITCHHIKER
model_data_02 <- db %>%
  filter(outcome1 != "law") %>%
  mutate(outcome = ifelse(outcome1 == "no law", 0, 1))

# MAIN
#===============================================================================
# - create the two model formulas
mformula_01 <- formula(outcome ~ MRef + MRef * Majority + Majority + 
                        Major + Cosponsr_log + AA + Hisp +
                         ChRef + RankRef + 
                         extremism + bills_spons_n  +
                         experience + Gender + 
                         Chamber * revenue_bill +
                         reauth + 
                         companion +
                         by_request)

mformula_02 <- formula(outcome ~ MRef + MRef * Majority + Majority + 
                         Major + Cosponsr_log + AA + Hisp +
                         ChRef + RankRef + 
                         extremism + bills_spons_n  +
                         experience + Gender + 
                         Chamber * revenue_bill +
                         reauth +
                         companion +
                         by_request)

# - iterate through Congresses, and for the covariates of interest, estimate the
#   marginal effects on the outcome in each Congress.
results <- NULL
congs <- unique(model_data_01$Cong)

for (cong in congs) {
  # - initializing a dataset with the marginal effects for both models
  full_marfx <- NULL
  
  # - a list with the data for this Congress
  datasets <- list(model_data_01 %>% filter(Cong == cong) %>% 
                     mutate(Major = as.character(Major)),
                   model_data_02 %>% filter(Cong == cong) %>%
                     mutate(Major = as.character(Major)))
  
  # - estimate the two models
  logisitc_model1 <- glm(mformula_01, 
                         data = datasets[[1]],
                         family = "binomial")
  logisitc_model2 <- glm(mformula_02, 
                         data = datasets[[2]],
                         family = "binomial")
  models <- list(logisitc_model1, logisitc_model2)
  
  for (i in 1:length(models)) {
    # - select the model and the datset
    model <- models[[i]]
    model_dataset <- datasets[[i]]
    
    # - calculating marginal effects for the model (see how in the 00-functions.R)
    model_marfx <- get_marfx_logistic(model = model, 
                                      model_dataset = model_dataset,
                                      type = "likelihood",
                                      cat_variables = c("Major"))
    
    # - adding a variable indicating the model used to calculate marginal effects
    model_marfx$model <- paste0("model", i)
    
    # - add results to macro marginal effects dataset
    full_marfx <- rbind(full_marfx, model_marfx)
  }
  # - add Congress variable
  full_marfx$Cong <- cong
  results <- rbind(results, full_marfx)
}

# - pulling the results for the coefficients of interest
results_to_plot <- results %>%
  filter(v %in% c("Majority", "ChRef", "Chamber"))


# PLOT: Figure 9 in Supporting Information D.
#===============================================================================
png("./figures/figure9-hetero-effects.png", width = 1200, height = 600)
ggplot(results_to_plot %>%
         mutate(model = recode(model, 
                               `model1` = "LAW", `model2` = "HITCHHIKER"),
                v = recode(v,
                           `ChRef` = "Committee Chair",
                           `Chamber` = "Senate"),
                model = factor(model, levels = c(
                  "LAW", "HITCHHIKER"
                ))),
       aes(x = factor(Cong), y = pe, ymin = lwr, ymax = upr)) + 
  geom_pointrange() +
  facet_grid(model ~ v, scales = "free_x") +
  geom_hline(yintercept = 1, color = "red") +
  xlab("Congress") +
  ylab("Relative likelihood of a bill becoming a law on its own or as a hitchhiker\n") +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(linetype = "dotted", color = "gray80"),
    axis.line = element_line(color = "black"),
    strip.text = element_text(size = 14)
  )
dev.off()
