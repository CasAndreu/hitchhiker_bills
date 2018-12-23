#===============================================================================
# 03-models.R
# Purpose: To replicate Figure 4 and model coefficients in Table 4 in Supporting
#          Information D, showing the relationship between a relevant set of 
#          covariates and the probability of a bill to be enacted as stand alone
#          legislation or as a hitchhiker bill.
# Article: "More Effective Than We Thought: Accounting for Legislative 
#           Hitchhikers Reveals a More Inclusive and Productive Lawmaking 
#           Process."
# Journal: American Journal of Political Science
# Year: 2018
# Authors: Andreu Casas, Matt Denny, and John
#===============================================================================

# PACKAGES
#===============================================================================
# - install packages if needed
install.packages("dplyr")
install.packages("rio")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("broom")
install.packages("xtable")

# - load the packages
library(dplyr)
library(rio)
library(tidyr)
library(ggplot2)
library(broom)
library(xtable)
source("../code/00-functions.R") # our own utils

# DATA
#===============================================================================
db <- import("../data/main_db.csv")

# DATA
#===============================================================================
# - a version of the dataset only with important (non-minor) bills
db_important <- db %>% 
  filter(ImpBill == 1)

# - make sure the Major topic code variable is treated as factor
db_important$Major <- as.character(db_important$Major)

# - a version of the dataset where the outcome variable is whether a bill was
#   enacted as stand-alone legislation
model_data_01 <- db_important %>%
  mutate(outcome = ifelse(outcome1 == "law", 1, 0))

# - a version of the dataset where the outcome variable is whether a bill 
#   became law as hitchhiker. We exclude in here the bills that were already
#   enacted as stand-alone legislation
model_data_02 <- db_important %>%
  filter(outcome1 != "law") %>%
  mutate(outcome = ifelse(outcome1 == "no law", 0, 1))

# MODELS
#===============================================================================
# - model predicting whether a bill will become stand alone law
logisitc_model1 <- glm(outcome ~ MRef + MRef * Majority + Majority + 
                         unified_cong +
                         nomgrid +
                         Major + Cosponsr_log + AA + Hisp +
                         ChRef + RankRef + SubChRef + SubRankRef +
                         extremism + bills_spons_n  +
                         experience + Gender + 
                         Chamber * revenue_bill +
                         reauth + 
                         companion +
                         by_request,
                       data = model_data_01,
                       family = "binomial")

logisitc_model2 <- glm(outcome ~ MRef + MRef * Majority + Majority + 
                         unified_cong +
                         nomgrid +
                         Major + Cosponsr_log + AA + Hisp +
                         ChRef + RankRef + SubChRef + SubRankRef +
                         extremism + bills_spons_n  +
                         experience + Gender + 
                         Chamber * revenue_bill +
                         reauth +
                         companion +
                         by_request,
                       data = model_data_02,
                       family = "binomial")


# CALCULATING MARGINAL EFFECTS
#===============================================================================
# - initializing a dataset with the marginal effects for both models
full_marfx <- NULL

# - a list with the models
models <- list(logisitc_model1, logisitc_model2)
datasets <- list(model_data_01, model_data_02)

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

# - remove the marginal effects for the separate effect of variables in 
#   interactions
full_marfx <- full_marfx %>% 
  filter(v != "MRef",
         v != "revenue_bill")

# - sorting the covariates for the plot
full_marfx$v <- factor(full_marfx$v,
                       levels =  rev(c("Majority", 
                                       "ChRef", "SubChRef",
                                       "Majority1 (MRef)",
                                       "RankRef", "SubRankRef", 
                                       "Majority0 (MRef)",
                                       "experience", "extremism", "bills_spons_n",
                                       "Gender", "AA", "Hisp",
                                       "Cosponsr_log", 
                                       "unified_cong",
                                       "nomgrid",
                                       "Chamber", 
                                       "reauth",
                                       "Chamber0 (revenue_bill)", 
                                       "Chamber1 (revenue_bill)",
                                       "companion",
                                       "by_request")
                       ))



# - giving better names to the outcome variables
full_marfx$var <- recode(full_marfx$v,
                         `Chamber` = "Senate",  
                         `unified_cong` = "Unified Congress",
                         `ChRef` = "Committee Chair",
                         `RankRef` = "Committee Rank Member",
                         `SubChRef` = "Subcommittee Chair",
                         `SubRankRef` = "Subcommittee Rank Member",
                         `Majority0 (MRef)` = "Committee Member (Minority)",
                         `Majority1 (MRef)` = "Committee Member (Majority)",
                         `Cosponsr_log` = "Number of Co-sponsors (log)",
                         `nomgrid` = "Gridlock Interval",
                         `unified_gov` = "Unified Government",
                         `experience` = "Years in Congress",
                         `extremism` = "Extremism",
                         `AA` = "African American",
                         `Hisp` = "Hispanic",
                         `companion` = "Companion Bill",
                         `bills_spons_n` = "Bills Sponsored",
                         `by_request` = "Administration Bill",
                         `Gender` = "Female",
                         `reauth` = "Reauthorization bill",
                         `Chamber0 (revenue_bill)` = 
                           "Revenue Bill (House)",
                         `Chamber1 (revenue_bill)` = 
                           "Revenue Bill (Senate)"
)

# - better labels to the model variable
full_marfx$model <- ifelse(full_marfx$model == "model1",
                           "LAW", "HITCHHIKER")

# - invert the order of the models: LAW first
full_marfx$model <- relevel(factor(full_marfx$model), ref = "LAW")

# - we are not interested in this coefficient for the House
full_marfx_to_plot <- full_marfx  %>%
  filter(!(var == "Revenue Bill (House)"))

# - transform some large coefficients to fit a secondary axis
vars_to_transf <- c("Companion Bill", "Administration Bill")
scalar <- 4
for (v in vars_to_transf) {
  full_marfx_to_plot$pe[full_marfx_to_plot$var == v] <- 
    full_marfx_to_plot$pe[full_marfx_to_plot$var == v] / scalar
  full_marfx_to_plot$lwr[full_marfx_to_plot$var == v] <- 
    full_marfx_to_plot$lwr[full_marfx_to_plot$var == v] / scalar
  full_marfx_to_plot$upr[full_marfx_to_plot$var == v] <- 
    full_marfx_to_plot$upr[full_marfx_to_plot$var == v] / scalar
}

# - a variable indicating which coefficients have been transformed (we'll give
#   them a stronger/lighter color)
full_marfx_to_plot <- full_marfx_to_plot %>%
  mutate(transf_coef = ifelse(var %in% vars_to_transf, 0, 1))

# PLOT: Figure 4
#===============================================================================
png("../figures/figure4-coefficient-plot.png", width = 1000, height = 700)
ggplot(full_marfx_to_plot,
       aes(x = var, y = pe, ymin = lwr, ymax = upr)) +
  geom_pointrange(aes(shape = as.character(transf_coef)),
                  show.legend = FALSE) +
  geom_hline(yintercept = 1) +
  geom_hline(yintercept = 0.3, linetype = "dashed") +
  xlab("") +
  ylab("\nRelative likelihood of a bill becoming a law on its own or as a hitchhiker") +
  coord_flip() +
  scale_x_discrete() +
  facet_wrap(~ model) +
  geom_polygon(inherit.aes = FALSE, 
               data = data.frame(x = c(0, 0, 2.5, 2.5), y = c(0, 5, 5, 0)),
               aes(x = x , y = y),
               fill = "gray90", alpha = 0.3) +
  # - kind of weird, but I want to flip the primary and secondary x-axes so
  #   that the re-scaled one is at the bottom, where the re-scaled coefficients
  #   are
  scale_y_continuous(breaks = seq(0, 
                                  round(max(full_marfx_to_plot$pe) + 1), 1),
                     labels = paste0("x", 
                                     seq(0,
                                         round(max(full_marfx$pe) + 6), 
                                         4)),
                     sec.axis = sec_axis(
                       trans = ~.*2,
                       breaks = seq(0,
                                    round(max(full_marfx$pe) + 6), 
                                    2)[1:6],
                       labels = paste0("x", 
                                       seq(0, 
                                           round(max(full_marfx_to_plot$pe)) + 1,
                                           1)))) +
  scale_color_manual(values = c(orange, blue)) +
  scale_shape_manual(values = c(17, 16)) +
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.grid.major.x = element_line(color = "gray"),
        panel.grid.major.y = element_line(color = "gray90", linetype = "dashed"),
        strip.text.x = element_text(size = 16),
        strip.placement = "outside",
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        legend.title = element_text(size = 14),
        legend.text=element_text(size=16),
        panel.spacing = unit(2, "lines"))
dev.off()


# COEFFICIENT TABLE: Table 4, in Supporting Information D
#===============================================================================
# Creating coefficient tables for these two models, to be placed in appendix

# - pull the coefficient tables from the model objects
m1 <- tidy(logisitc_model1)
m1$model <- "LAW"
m2 <- tidy(logisitc_model2)
m2$model <- "HITCHHIKER"

# - remove Cong and Major topic fix effects coefficients
m1 <- m1 %>%
  filter(!(grepl("Major[0-9]", term)))


m2 <- m2 %>%
  filter(!(grepl("Major[0-9]", term)))

# - sorting the variables
m1$term <- factor(m1$term,
                  levels =  c(
                    "Majority", 
                    "ChRef", "SubChRef", 
                    "Majority1 (MRef)",
                    "RankRef", "SubRankRef", 
                    "Majority0 (MRef)",
                    "MRef",
                    "MRef:Majority",
                    "experience", "extremism", "bills_spons_n",
                    "Gender", "AA", "Hisp",
                    "Cosponsr_log", 
                    "unified_cong",
                    "nomgrid",
                    "beyond_introduction",
                    "Chamber", 
                    "reauth",
                    "revenue_bill",
                    "Chamber:revenue_bill",
                    "Chamber0 (revenue_bill)", 
                    "Chamber1 (revenue_bill)",
                    "leadership_bill", 
                    "companion",
                    "clean_bill",
                    "by_request",
                    "(Intercept)")
)

m2$term <- factor(m2$term,
                  levels =  c(
                    "Majority", 
                    "ChRef", "SubChRef", 
                    "Majority1 (MRef)",
                    "RankRef", "SubRankRef", 
                    "Majority0 (MRef)",
                    "MRef",
                    "MRef:Majority",
                    "experience", "extremism", "bills_spons_n",
                    "Gender", "AA", "Hisp",
                    "Cosponsr_log", 
                    "unified_cong",
                    "nomgrid",
                    "beyond_introduction",
                    "Chamber", 
                    "reauth",
                    "revenue_bill",
                    "Chamber:revenue_bill",
                    "Chamber0 (revenue_bill)", 
                    "Chamber1 (revenue_bill)",
                    "leadership_bill", 
                    "companion",
                    "clean_bill",
                    "by_request",
                    "(Intercept)")
)

# - better labels for the variables
m1$term <- recode(m1$term,
                  `(Intercept)` = "Constant",
                  `MRef` = "Committee Member",
                  `MRef:Majority` = "Committee Member x Majority",
                  `Chamber:revenue_bill` = "Revenue Bill x Senate",
                  `Chamber` = "Senate",                   
                  `ChRef` = "Committee Chair",
                  `RankRef` = "Committee Rank Member",
                  `SubChRef` = "Subcommittee Chair",
                  `SubRankRef` = "Subcommittee Rank Member",
                  `Majority0 (MRef)` = "Committee Member (Minority)",
                  `Majority1 (MRef)` = "Committee Member (Majority)",
                  `Cosponsr_log` = "Number of Co-sponsors (log)",
                  `unified_cong` = "Unified Congress",
                  `nomgrid` = "Gridlock Interval",
                  `experience` = "Years in Congress",
                  `extremism` = "Extremism",
                  `AA` = "African American",
                  `Hisp` = "Hispanic",
                  `companion` = "Companion Bill",
                  `clean_bill` = "Clean Bill",
                  `bills_spons_n` = "Bills Sponsored",
                  `by_request` = "Administration Bill",
                  `beyond_introduction` = "Beyond Introduction",
                  #`first_ver_tokens_log_n` = 
                  # "Bill Length (log number of tokens)",
                  `Gender` = "Female",
                  `reauth` = "Reauthorization bill",
                  `revenue_bill` = "Revenue Bill",
                  `leadership_bill` = "Leadership Bill",
                  `Chamber0 (revenue_bill)` = 
                    "Revenue Bill (House)",
                  `Chamber1 (revenue_bill)` = 
                    "Revenue Bill (Senate)")

m2$term <- recode(m2$term,
                  `(Intercept)` = "Constant",
                  `MRef` = "Committee Member",
                  `MRef:Majority` = "Committee Member x Majority",
                  `Chamber:revenue_bill` = "Revenue Bill x Senate",
                  `Chamber` = "Senate",                   
                  `ChRef` = "Committee Chair",
                  `RankRef` = "Committee Rank Member",
                  `SubChRef` = "Subcommittee Chair",
                  `SubRankRef` = "Subcommittee Rank Member",
                  `Majority0 (MRef)` = "Committee Member (Minority)",
                  `Majority1 (MRef)` = "Committee Member (Majority)",
                  `Cosponsr_log` = "Number of Co-sponsors (log)",
                  `unified_cong` = "Unified Congress",
                  `nomgrid` = "Gridlock Interval",
                  `experience` = "Years in Congress",
                  `extremism` = "Extremism",
                  `AA` = "African American",
                  `Hisp` = "Hispanic",
                  `companion` = "Companion Bill",
                  `clean_bill` = "Clean Bill",
                  `bills_spons_n` = "Bills Sponsored",
                  `by_request` = "Administration Bill",
                  `beyond_introduction` = "Beyond Introduction",
                  `Gender` = "Female",
                  `reauth` = "Reauthorization bill",
                  `revenue_bill` = "Revenue Bill",
                  `leadership_bill` = "Leadership Bill",
                  `Chamber0 (revenue_bill)` = 
                    "Revenue Bill (House)",
                  `Chamber1 (revenue_bill)` = 
                    "Revenue Bill (Senate)")

# - keeping only coefficients and standard error
m1_table <- m1 %>%
  dplyr::select(term, estimate, std.error) %>%
  gather(estimate, value_law, -term) %>%
  mutate(value_law = round(value_law, 4)) %>%
  arrange(term)

m2_table <- m2 %>%
  dplyr::select(term, estimate, std.error) %>%
  gather(estimate, value_hhicker, -term)%>%
  mutate(value_hhicker = round(value_hhicker, 4)) %>%
  arrange(term)


# - merging the two tables
m1_table$term <- as.character(m1_table$term)
m2_table$term <- as.character(m2_table$term)

m_table <- full_join(m1_table, m2_table)

for (v in unique(m_table$term)) {
  for (out in c("value_law", "value_hhicker")) {
    pe <- m_table[m_table$term == v & m_table$estimate == "estimate", out]
    se <- m_table[m_table$term == v & m_table$estimate == "std.error", out]
    if (abs(as.numeric(pe)) > (1.96 * as.numeric(se))) {
      m_table[m_table$term == v & m_table$estimate == "estimate", out] <- 
        paste0(as.character(
          m_table[m_table$term == v & m_table$estimate == "estimate", out]
        ),
        "*")
    } else {
      m_table[m_table$term == v & m_table$estimate == "estimate", out] <- 
        paste0(as.character(
          m_table[m_table$term == v & m_table$estimate == "estimate", out]
        ),
        " ")
    }
  }
}

final_table <- m_table %>% 
  group_by(term) %>% 
  summarize(LAW = paste0(value_law[estimate == "estimate"], "  (", 
                         value_law[estimate == "std.error"], ")"),
            HITCHHIKER = paste0(value_hhicker[estimate == "estimate"], "  (", 
                                value_hhicker[estimate == "std.error"], ")")) %>%
  as.data.frame()

final_table$term <- factor(final_table$term,
                           levels = as.character(unique(m_table$term)))

final_table <- final_table %>%
  arrange(term)

# - add into tables the number observations for each model 
law_modeldata_n <- nrow(model_data_01)
hhicker_modeldata_n <- nrow(model_data_02)
obs_row <- data.frame(
  term = "N", LAW = law_modeldata_n, HITCHHIKER = hhicker_modeldata_n)


# - add into talbes the AIC for both models
law_aic <- logisitc_model1$aic
hhicker_aic <- logisitc_model2$aic
aic_row <- data.frame(
  term = "AIC", LAW = law_aic, HITCHHIKER = hhicker_aic)

final_table <- rbind(final_table, obs_row, aic_row)

# - the following line prints the sharelatex code that generates the table
print(xtable(final_table),  include.rownames=FALSE)

