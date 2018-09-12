#===============================================================================
# 10-table4-descriptive-stats-table-supporting-information-D.R
# Purpose: to replicate Table 4 in Supporting Information D, where we show 
#           descriptive statistics for the variable in the statistical models we
#           use in the paper to test our hitchhiker hypotheses.
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
library(rio)
library(xtable)

# DATA
#===============================================================================
# - data used for modeling
bills <- import("./data/main_db.csv") %>%
  filter(ImpBill == 1)

# DATA WRANGLING
#===============================================================================
# - a list with the model variables
vars <- c("MRef", "Majority", "Cong", "Major","Cosponsr_log", "AA", "Hisp",
          "ChRef", "RankRef", "SubChRef", "SubRankRef", "extremism", 
          "bills_spons_n", "experience", "Gender", "Chamber", "revenue_bill",
          "reauth", "companion", "by_request")

# - calculate the Minimum, Maximum, Average, and Mode of each variable in the 
#   model data
mdata <- bills[, vars] %>%
  gather(var, value) %>%
  group_by(var) %>%
  summarize(Minimum = round(min(as.numeric(as.character(value)), na.rm = TRUE), 3),
            Maximum = round(max(as.numeric(as.character(value)), na.rm = TRUE), 3),
            Mean = round(mean(as.numeric(value), na.rm = TRUE), 3),
            `Standard Deviation` = round(sd(value, na.rm = TRUE), 3),
            Mode = round(
              as.numeric(names(sort(table(value), decreasing = TRUE)[1]), 3))
  ) 

# - no average nor standard deviation for the categorical variables
mdata$Mean[mdata$var %in% c("Cong", "Major")] <- ""
mdata$`Standard Deviation`[mdata$var %in% c("Cong", "Major")] <- ""

# - sorting the covariates for the plot
mdata$var <- factor(mdata$var,
                    levels =  rev(c(
                      "Majority", 
                      "ChRef", "SubChRef", 
                      "RankRef", "SubRankRef", 
                      "MRef",
                      "experience", "extremism", "bills_spons_n",
                      "Gender", "AA", "Hisp",
                      "Cosponsr_log", 
                      "beyond_introduction",
                      "Chamber", 
                      "reauth",
                      "revenue_bill",
                      "Chamber0 (revenue_bill)", 
                      "Chamber1 (revenue_bill)",
                      "companion",
                      "by_request",
                      "Cong",
                      "Major")
                    ))

# - better variable labels
mdata$var <- recode(mdata$var,
                    `Cong` = "Congress",
                    `Major` = "Major Policy Agendas Topic code",
                    `Chamber` = "Senate",                   
                    `ChRef` = "Committee Chair",
                    `RankRef` = "Committee Rank Member",
                    `SubChRef` = "Subcommittee Chair",
                    `SubRankRef` = "Subcommittee Rank Member",
                    `MRef` = "Referral Committee Member",
                    `Cosponsr_log` = "Number of Co-sponsors (log)",
                    `experience` = "Years in Congress",
                    `extremism` = "Extremism",
                    `AA` = "African American",
                    `Hisp` = "Hispanic",
                    `companion` = "Companion Bill",
                    `bills_spons_n` = "Bills Sponsored",
                    `by_request` = "Administration Bill",
                    `beyond_introduction` = "Beyond Introduction",
                    `Gender` = "Female",
                    `reauth` = "Reauthorization bill",
                    `revenue_bill` = "Revenue Bill")

mdata <- mdata %>%
  arrange(desc(var))

colnames(mdata)[1] <- ""

# TABLE: Table 4, descriptive statistics in Supporting Informcation D.
#===============================================================================
# - the following line of code generates the latex code to create Table 4
print(xtable(mdata), include.rownames = FALSE)
