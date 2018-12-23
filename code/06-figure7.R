#===============================================================================
# 06-figure7.R
# Purpose: to replicate Figure 7 of the paper, showing where in the legislative
#           process hitchhiker bills get picked up.
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
install.packages("tidyr")
install.packages("ggplot2")

# - load the packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(rio)

# DATA
#===============================================================================
# - importing the main dataset with all bill level information
bills <- import("../data/main_db.csv")

# DATA WRANGLING
#===============================================================================
# - only taking important bills into consideration
bills <- bills %>%
  filter(ImpBill == 1)

# - only exploring hitchhiker bills
hitchhikers <- bills %>%
  filter(outcome1 == "insertion")

# - a variable indicating when the topic of the insertion is the same as the
#     target law. Otherwise, keep the information about the different topic of 
#     target law.
hitchhikers <- hitchhikers %>%
  mutate(topic = ifelse(Major == Major_Law, "same topic", Major))

# - adding topics labels
major_labels <- data.frame(
  Major = c(1:10, 12:21),
  label = c("Macroeconomics", "Civil Liberties", "Health", "Agriculture", 
            "Employment", "Education", "Environment","Energy", "Transportation",
            "Immigration", "Law & Crime", "Social Welfare", "Housing", 
            "Banking", "Defense", "Science & Tech", "Foreign Trade", 
            "International", "Government Operations", "Public Lands")
)

major_labels$Major <- as.character(major_labels$Major)
hitchhikers$Major <- as.character(hitchhikers$Major)
hitchhikers <- left_join(hitchhikers, major_labels)
hitchhikers <- hitchhikers %>%
  mutate(label = ifelse(topic == "same topic", "Same Topic as Target Law", 
                        as.character(label)))
major_labels2 <- major_labels %>%
  rename(law_label = label, Major_Law = Major)
major_labels2$Major_Law <- as.character(major_labels2$Major_Law)
hitchhikers$Major_Law <- as.character(hitchhikers$Major_Law)
hitchhikers <- left_join(hitchhikers, major_labels2)

# - selecting only the variables we need for the plot
hitchhikers_plotdb <- hitchhikers %>%
  dplyr::select(BillID, LawID, first_match, label, Chamber, Major_Law,
                law_label) %>%
  mutate(bill_chamber = ifelse(Chamber == 0, "House hitchhiker", 
                               "Senate hitchhiker"),
         law_chamber = ifelse(grepl("-S", LawID), "Senate law", "House law"),
         bill_law_chamber = paste0(bill_chamber, " >> ", law_chamber)) %>%
  dplyr::select(-Chamber) 

# - changing the reference class of the topic and insertion stage variables
hitchhikers_plotdb$label <- factor(hitchhikers_plotdb$label,
                                  levels = c(as.character(sort(major_labels$label)),
                                             "Same Topic as Target Law"))

hitchhikers_plotdb$law_label <- factor(hitchhikers_plotdb$law_label,
                                      levels = c(as.character(sort(major_labels$label)),
                                                 "Same Topic as Target Law"))


hitchhikers_plotdb$first_match <- factor(hitchhikers_plotdb$first_match,
                                        levels = rev(c("introduced", "reported", 
                                                       "engrossed", 
                                                       "other chamber amended", 
                                                       "same chamber amended",
                                                       "enrolled")))

# PLOT: Figure 7
#===============================================================================
png("../figures/figure7_where_hitchhiker_get_picked_up.png", 
    width = 1600, height = 800)
ggplot(na.omit(hitchhikers_plotdb), aes(x = first_match, fill = label)) +
  geom_bar(color = "gray50") +
  ylab("Number of hitchhiker bills") +
  xlab("") +
  coord_flip() +
  scale_fill_manual(values = c(rep("gray80", 19), "gray30")) +
  facet_grid(law_chamber ~ bill_chamber) +
  theme(legend.position = "none",
        axis.text.x=element_text(size = 16),
        panel.background = element_blank(),
        axis.title.x = element_text(size = 14),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        strip.text.x = element_text(size = 18),
        strip.text.y = element_text(size = 18),
        legend.text=element_text(size=14),
        legend.title = element_text(size = 14),
        panel.spacing = unit(2, "lines"))
dev.off()
