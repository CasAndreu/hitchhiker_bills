#===============================================================================
# 02-figure3-how-far-hitchhiker-get-on-their-own.R
# Purpose: To replicate figure 4 of the paper, showing how far hitchhiker bills 
#          get though the legislative process on their own.
# Article: "More Effective Than We Thought: Accounting for Legislative 
#           Hitchhikers Reveals a More Inclusive and Productive Lawmaking 
#           Process."
# Journal: American Journal of Political Science
# Year: 2018
# Author: Andreu Casas, Matt Denny, and John
#===============================================================================

# PACKAGES
#===============================================================================
library(rio)
library(dplyr)
library(tidyr)
library(grid)

# DATA
#===============================================================================
db <- import("./data/main_db.csv")

# DATA WRANGLIONG
#===============================================================================
# - making sure the last version generic naming is correct
db_02 <- db %>%
  filter(outcome1 == "insertion", ImpBill ==1, !is.na(last_vers)) %>%
  mutate(BillChamber = ifelse(Chamber == 1, "S", "H")) %>%
  mutate(last_vers_generic = NA,
         last_vers_generic = ifelse(
           last_vers %in% c("IH", "IS"), "introduced", last_vers_generic),
         last_vers_generic = ifelse(
           last_vers %in% c("RH", "RS"), "reported", last_vers_generic),
         last_vers_generic = ifelse(
           last_vers %in% c("EH", "ES"), "engrossed", last_vers_generic),
         last_vers_generic = ifelse(
           (BillChamber == "H" & last_vers %in% c("EAS", "AS")) |
             (BillChamber == "S" & last_vers == "EAH"),
           "other chamber amended", last_vers_generic),
         last_vers_generic = ifelse(
           (BillChamber == "S" & last_vers %in% c("EAS", "AS")) |
             (BillChamber == "H" & last_vers == "EAH"), 
           "same chamber amended", last_vers_generic))


db_02$last_vers_generic <- factor(db_02$last_vers_generic,
                                   levels = rev(c("introduced", "reported", 
                                                  "engrossed",
                                                  "other chamber amended",
                                                  "same chamber amended")))

db_02$BillChamber <- recode(db_02$BillChamber,
                             `H` = "House Hitchhikers",
                             `S` = "Senate Hitchhikers")

# PLOT: Figure 3
#===============================================================================
#pdf("./Paper/images/figure9_BW.pdf", width = 14, height = 5)
png("./figures/figure3_BW.png", width = 1400, height = 500)
ggplot(db_02, 
       aes(x = last_vers_generic)) +
  geom_bar(fill = "gray70") +
  ylab("Number of hitchhikers") +
  xlab("Last version") +
  #ggtitle("") +
  #scale_fill_manual("", values = c(orange, blue)) +
  ylim(c(0,1300)) +
  facet_wrap(~BillChamber) +
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        legend.position = "none",
        panel.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        strip.text.x = element_text(size = 16),
        axis.text.x = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.title = element_text(size = 14),
        legend.text=element_text(size=16))
dev.off()
