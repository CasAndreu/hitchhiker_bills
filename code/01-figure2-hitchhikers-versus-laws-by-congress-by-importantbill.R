#===============================================================================
# 01-figure2-hitchhikers-versus-laws-by-congress-by-importantbill.R
# Purpose: To replicate Figure 3 of the paper, showing the number of hitchhiker
#          bills and laws by Congress, distingushing between important and minor
#          bills.
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
library(ggplot2)

# DATA
#===============================================================================
# - dataset with both important and minor bills of interest
db <- import("./data/main_db.csv")

# DATA WRANGLING
#===============================================================================
# - organizing the dataset so we can easily plot the desired information: 
#   hitchhikers and laws by Congress and by Important-Minor bill status
plot_db <- db %>%
  # ... excluding non-enacted legislation
  filter(outcome1 != "no law") %>%
  # ... selecting the variables of interest
  dplyr::select(Cong, outcome1, ImpBill) %>%
  # ... provide human readable names to the variables 
  mutate(outcome1 = ifelse(outcome1 == "insertion", "hitchhicker bills", "laws"),
         important = ifelse(ImpBill == 1, "Important Bills", "Minor Bills"))

# PLOT: Figure 2
#===============================================================================
#pdf("./figures/figure2_BW.pdf", width = 12, height = 5)
png("./figures/figure2_BW.png", width = 1200, height = 500)
ggplot(plot_db, aes(x = factor(Cong), fill = factor(outcome1))) +
  geom_bar(position = "dodge") +
  ylab("Number of bills") +
  xlab("Congress") +
  #geom_text(stat="count", aes(label=..count..), vjust=1.75) +
  facet_wrap(~ important) +
  scale_fill_manual("", values = c("gray20", "gray70")) +
  theme(panel.background = element_blank(),
        axis.text.x = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        strip.text.x = element_text(size = 16),
        legend.text=element_text(size=16))
dev.off()
