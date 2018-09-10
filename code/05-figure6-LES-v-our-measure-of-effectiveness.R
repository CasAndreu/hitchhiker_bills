#===============================================================================
# 05-figure6-LES-v-our-measure-of-effectiveness.R
# Purpose: to replicate Figure 6 of the paper, comparing our measure of 
#           effectivenes (legislation enacted as proportion of legislation 
#           introduced) v. Legislative Effectiveness Scores, of Volden and 
#           Weiseman.
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
library(tidyr)
library(ggplot2)

# DATA
#===============================================================================
# - dataset with the bill levels information
bills <- import("./data/main_db.csv")

# - exclude minor non-important bills
bills <- bills %>%
  filter(ImpBill == 1)

# - Volden and Weisman data
les01 <- import("./data/LEPData93to110Congresses.xlsx")[-1,]
les02 <- import("./data/LEPData111to113Congresses.xlsx")[-1,]

# - Loading committee assignments & resp data from Charles Stewart III
house_comm_data <- import("./data/house_assignments_103-115-3.xls")
senate_comm_data <- import("./data/senators_103-115-2.xls", skip = 1)

# DATA WRANGLING
#===============================================================================
# - clean committee responsibility data
house_comm_data_ready <- house_comm_data %>%
  mutate(member_id = paste0(Congress, "-", `ID #`)) %>%
  filter(`Senior Party Member` %in% c(11, 12, 13, 14, 16)) %>%
  mutate(chair = 1) %>%
  dplyr::select(Congress, member_id, chair) %>%
  rename(Cong = Congress)

# - mergind lse data
les <- rbind(les01 %>% dplyr::select(thomas_name, congress, icpsr, les),
             les02 %>% dplyr::select(thomas_name, congress, icpsr, les)) %>%
  # - omitting members don't have icpsr id (n = 111)
  na.omit()

# - combining congress and icpsr
les <- les %>%
  mutate(member_id = paste0(congress, "-", icpsr)) %>%
  dplyr::select(member_id, les)

# - merging the data with our own data
bills <- bills %>%
  mutate(member_id = paste0(Cong, "-", PooleID))

bills$member_id <- as.character(bills$member_id)
les$member_id <- as.character(les$member_id)

eff_dataset <- left_join(bills, les) %>%
  dplyr::select(NameFull, Cong, member_id, les)

# - a dataset with our measure of effectiveness
bills_eff <- bills %>%
  group_by(member_id) %>%
  summarize(introduced = n(),
            law = length(which(outcome1 == "law")),
            hhiker = length(which(outcome1 == "insertion")),
            law_hhiker = length(which(outcome1 == "law" |
                                        outcome1 == "insertion")),
            law_prop = round(law / introduced, 4),
            law_hhiker_prop = round(law_hhiker / introduced, 4),
            majority = Majority[1]) %>%
  mutate(law_hhiker_prop = ifelse(is.na(law_hhiker_prop), 0, 
                                  law_hhiker_prop),
         law_prop = ifelse(is.na(law_prop), 0, law_prop))

bills_eff_to_merge <- bills_eff %>%
  as.data.frame() %>%
  #dplyr::select(member_id, law_prop, law_hhiker_prop)
  dplyr::select(member_id, law, law_hhiker, majority)

# - merging our variables with V&M measures
eff_dataset$member_id <- as.character(eff_dataset$member_id)
bills_eff_to_merge$member_id <- as.character(bills_eff_to_merge$member_id)

final_data <- left_join(eff_dataset, bills_eff_to_merge)

# - add committee responsibility data
final_data$member_id <- as.character(final_data$member_id)
house_comm_data_ready$member_id <- as.character(house_comm_data_ready$member_id)
final_data$Cong <- as.character(final_data$Cong)
house_comm_data_ready$Cong <- as.character(house_comm_data_ready$Cong)

final_data_02 <- left_join(final_data, house_comm_data_ready) %>%
  mutate(chair = ifelse(is.na(chair), 0, chair))

# PLOT: Figure 6
#===============================================================================
# - ranking members by LES by Congress
final_data_rank_diff <- final_data_02 %>% 
  mutate(les = as.numeric(les)) %>%
  arrange(desc(les)) %>%
  unique() %>%
  filter(!is.na(les)) %>%
  # - standardize both effectiveness measures
  mutate(les_std = les / max(les),
         our_std = law_hhiker / max(law_hhiker)) %>%
  # - and calculating the difference, and sort by this difference
  mutate(diff_std = les_std - our_std) %>%
  arrange(desc(diff_std)) %>%
  mutate(label_01 = as.character(round(les, 2)),
         label_02 = paste0(law, " (", law_hhiker, ")"))

png("./figures/figure6a-LES-vs-OUR-indiv-diff.png", width = 1000, height = 1100)
ggplot(final_data_rank_diff %>%
         filter(Cong == 111) %>%
         mutate(Cong = "111th Congress") %>%
         mutate(NameFull = factor(NameFull, levels = as.character(
           unique(NameFull)))) %>%
         filter((diff_std < -0.03) | diff_std > 0.1) %>%
         mutate(chair = ifelse(chair == 1, "chair", "not-chair"),
                majority = ifelse(majority == 1, "majority", "minority")), 
       aes(x = NameFull, y = diff_std, pch = chair, 
           color = majority)) +
  geom_text(inherit.aes = FALSE,
            aes(x = NameFull, y = diff_std, 
                label=label_01),hjust=1.75, vjust= .25, size = 2.5) +
  geom_text(inherit.aes = FALSE,
            aes(x = NameFull, y = diff_std, 
                label=label_02),hjust=-.5, vjust= .25, size = 2.5) +
  geom_point(size = 2) +
  xlab("") +
  ylab("\nNormalized differences, {-1,1} range, between Number of Laws + Hitchhikers and LES\n(positive values indicate that LES overestimate efficiency)") +
  coord_flip() +
  facet_wrap(~Cong, ncol = 5) +
  scale_color_manual("", values = c("black", "gray85")) +
  geom_hline(yintercept = 0, color = "red", alpha = 0.3) +
  scale_shape_manual("", values = c(17, 16)) +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(linetype = "dotted", color = "gray80")
  )
dev.off()

# - a plot showing the full distribution 
png("./figures/figure6b-LES-vs-OUR-indiv-diff-FULL-DIST.png",
    width = 1000, height = 1100)
ggplot(final_data_rank_diff %>%
         filter(Cong == 111) %>%
         mutate(Cong = "111th Congress") %>%
         mutate(NameFull = factor(NameFull, levels = as.character(
           unique(NameFull)))) %>%
         mutate(chair = ifelse(chair == 1, "chair", "not-chair"),
                majority = ifelse(majority == 1, "majority", "minority")), 
       aes(x = NameFull, y = diff_std, color = majority)) +
  geom_point(size = 2) +
  xlab("Legislators") +
  ylab("") +
  coord_flip() +
  #facet_wrap(~Cong, ncol = 5) +
  scale_color_manual("", values = c("black", "gray85")) +
  geom_hline(yintercept = 0, color = "red", alpha = 0.3) +
  #scale_shape_manual("", values = c(17, 16)) +
  theme(
    axis.text.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.line = element_line(color="black"),
    #panel.border = element_rect(color = "black", fill = NA),
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major.x = element_line(linetype = "dotted", color = "gray80")
  )
dev.off()
