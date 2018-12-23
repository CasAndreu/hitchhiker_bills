#===============================================================================
# 08-supporting-info-C.R
# Purpose: to replicate Figure 8 of the paper, where we show the distribution of
#           the number of models (out of 99 high performing models) that 
#           predicted the same hitchhiker in the first stage of the hitchhiker
#           discovering process.
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
install.packages("rio")
install.packages("ggplot2")

# - load the packages
library(dplyr)
library(rio)
library(ggplot2)
source("../code/00-functions.R")

# DATA
#===============================================================================
# - loading a dataset with information about the hitchhikers predicted by the 
#   99 high performing models in the first stage of the hitchhiker discovering 
#   process.
preds <- import("../data/predictions/hitchhiker_predictions_stage01.csv")

# PLOT: Figure 8, in Supporting Information C
#===============================================================================
png("../figures/figure8-hitchhikers_barplot_iter1.png", width = 1200, height = 500)
ggplot(preds, aes(x = sum)) +
  geom_bar(color = blue, fill = blue) +
  scale_x_continuous(breaks = seq(1, 99, 7),
                     labels = seq(1, 99, 7)) +
  xlab("# models predicting the same hitchhicker bill") +
  ylab("Hitchhicker bills count") +
  theme(panel.background = element_blank(),
        legend.title=element_text(size=14), 
        legend.text=element_text(size=16),
        strip.text.x = element_text(size = 16),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.text.x = element_text(size = 20))
dev.off()
