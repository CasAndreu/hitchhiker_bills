#===============================================================================
# 04-figure5-general-effects-on-effectiveness.R
# Purpose: to replicate figure 5 of the paper, showing how counting hitchhikers 
#           as enacted legislation increases the proportion of different types 
#           of members that get at least 1 bill enacted in any given Congress
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
library(tidyr)
library(ggplot2)
library(rio)

# DATA
#===============================================================================
# - importing main dataset
db <- import("./data/main_db.csv")

# - a dataset with member level information (committee assignments)
members <- import("./data/members_comm_assign_w_MemberID.csv")

# DATA WRANGLING
#===============================================================================
# - only selecting Important bills from the dataset
db <- db %>%
  filter(ImpBill == 1)

# - a version of the dataset only with the necessary variables
db <- db %>%
  dplyr::select(BillID, MemberID, Cong, NameFull, outcome1, Majority, 
                ChRef, SubChRef, SubChRef, RankRef, SubRankRef, Chamber, Gender)

# - merging the members and bill information
members2 <- left_join(members, db)

# - pre-processing some member-level information
members3 <- members2 %>%
  group_by(MemberID) %>%
  summarize(Majority = Majority[1],
            Gender = Gender[1],
            Chair = ifelse(nchar(cChair[1]) > 2, 1, 0),
            SubChair = ifelse(nchar(sChair[1]) > 2, 1, 0),
            Rank = ifelse(nchar(cRank[1]) > 2, 1, 0),
            SubRank = ifelse(nchar(sRank[1]) > 2, 1, 0),
            rankfile = ifelse(Chair == 0 & SubChair == 0 &
                                Rank == 0 & SubRank == 0, 1, 0))

members3$Cong <- as.character(sapply(members3$MemberID, function(x)
  strsplit(x, split = "-")[[1]][2]))

# - congress level denominators
denoms <- members3 %>%
  group_by(Cong) %>%
  summarize(minority = 535 - length(which(Majority == 1)),
            female = length(which(Gender == 1)),
            chair = length(which(Chair == 1)),
            subchair = length(which(SubChair == 1)),
            rank = length(which(Rank == 1)),
            subrank = length(which(SubRank == 1)),
            rank_file = length(which(rankfile == 1)),
            senate = 100,
            introduced = 535) %>%
  dplyr::select(Cong, minority, female, rank_file, senate, introduced) %>%
  gather(var, denom, -Cong)

# - a dataset indicating whether a member got at least one stand alone bill, and
#     weather it got at least one hitchhiker, by Congress
plotdb <- db %>%
  group_by(Cong, NameFull) %>%
  summarize(introduced = ifelse(n() > 1, 1, 0),
            law = ifelse(length(which("law" %in% outcome1)) > 0, 1, 0),
            hhiker = ifelse(length(which("insertion" %in% outcome1)) > 0, 1, 0),
            law_hhiker = ifelse(law == 1 | hhiker == 1, 1, 0),
            Majority = Majority[1],
            ChRef = max(ChRef, na.rm = T), 
            SubChRef = max(SubChRef, na.rm = T), 
            RankRef = max(RankRef, na.rm = T),
            SubRankRef = max(SubRankRef, na.rm = T), 
            Chamber = max(Chamber, na.rm = T),
            Gender = Gender[1]) %>%
  # - creating some new variables
  mutate(rank_file = ifelse(ChRef == 0 & SubChRef == 0 & 
                              RankRef == 0 & SubRankRef == 0,
                            1, 0),
         minority = ifelse(Majority, 0, 1)) %>%
  rename(senate = Chamber, female = Gender) %>%
  # - now get rid of original committee info
  dplyr::select(-ChRef, -SubChRef, -RankRef, -SubRankRef, 
                -Majority, -hhiker, introduced) %>%
  # - long format
  gather(var, value, -Cong, -NameFull, -law, -law_hhiker) %>%
  # - outcome to long format as well
  gather(outcome, out, -Cong, -NameFull, -var, -value) %>%
  filter(value == 1) %>%
  group_by(Cong, outcome, var) %>%
  #  summarize(n = length(which(out == 1)),
  #            total = n()) %>%
  #  as.data.frame()
  summarize(law_prop = length(which(out == 1)) / n())

# - better order for the variables variable
plotdb$var <- factor(plotdb$var,
                     levels = c("introduced",
                                "minority",
                                "rank_file",
                                "female",
                                "senate"))

# - better labels for the variables of interest
plotdb$var <- recode(plotdb$var,
                     `introduced` = "ALL MEMBERS",
                     `female` = "Female Sponsors",
                     `minority` = "Minority Sponsors",
                     `rank_file` = "Rank and File Sponsors",
                     `senate` = "Senators")

# - better labels for the outcome of interest
plotdb$outcome <- recode(plotdb$outcome,
                         `law` = "Stand-alone Law",
                         `law_hhiker` = "Hitchhiker or Stand-alone Law")

# - a dataset with averages to plot
avlines <- plotdb %>%
  group_by(var, outcome) %>%
  summarize(av = mean(law_prop))

# PLOT: Figure 5
#===============================================================================
png("./figures/figure5_so_what_BW.png", width = 1500, height = 500)
ggplot(plotdb, 
       aes(x = Cong, y = law_prop, col = outcome, group = outcome)) +
  geom_line(lwd = 2) +
  geom_hline(data = avlines,  
             aes(yintercept = av, col = outcome), lwd = 1.2, alpha = 0.5) +
  facet_wrap(~ var, nrow = 1) +
  scale_color_manual("% of members that sponsored at leat one ...",
                     values = c("gray30", "gray70")) +
  scale_y_continuous(name = "",
                     breaks = seq(0, 1, 0.1),
                     labels = paste0(seq(0, 100, 10), "%")) +
  xlab("Congress") +
  theme(panel.background = element_blank(),
        panel.grid.major.x = element_line(color = "gray90"),
        panel.grid.major.y = element_line(color = "gray90"),
        strip.text.x = element_text(size = 16),
        axis.text.x = element_text(size = 12,angle = 90, hjust = 1),
        axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.title = element_text(size = 14),
        legend.text=element_text(size=16),
        legend.position = "bottom")
dev.off()

