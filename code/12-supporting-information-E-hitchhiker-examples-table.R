#===============================================================================
# 12-supporting-information-E-hitchhiker-examples-table.R
# Purpose: to replicate the table in Supporting Information E, where we provide
#           a list of hitchhiker bills added to the Affordable Care Act and the
#           Financial Freedom Act of 1999.
# Article: "More Effective Than We Thought: Accounting for Legislative 
#           Hitchhikers Reveals a More Inclusive and Productive Lawmaking 
#           Process."
# Journal: American Journal of Political Science
# Year: 2018
# Authors: Andreu Casas, Matt Denny, and John Wilkerson
#===============================================================================

# PACKAGES
#===============================================================================
library(rio)
library(dplyr)
library(xtable)

# DATA
#===============================================================================
# - data used for modeling
bills <- import("./data/main_db.csv") %>%
  filter(ImpBill == 1)

# DATA WRANLGING
#===============================================================================
# - selecting only hitchhikers that have 2 target Laws
hhikers <- bills %>%
  filter(outcome1 == "insertion",
         LawID %in% c("111-HR-3590", "106-HR-2488"))

# - ACA and the other hhikers
aca <- hhikers$BillID[hhikers$LawID == "111-HR-3590"]
other <- hhikers$BillID[hhikers$LawID == "106-HR-2488"]

# - adding one empty value at the end of the ACA hhikers (it has 1 less hhiker 
#   and I want them in the same table)
aca <- c(aca, "")

# - preparing the table that will be in the supporting information
out_table <- data.frame(
  aca1 = aca[1:24],
  aca2 = aca[25:length(aca)],
  other1 = other[1:24],
  other2 = other[25:length(other)]
)

# TABLE: Hitchhiker Examples in Supporting Information E.
#===============================================================================
# - getting LaTex code for the table: notice that they are not in the same order
#   than they appear in the paper, but they are the same.
print(xtable(out_table), include.rownames = FALSE)
