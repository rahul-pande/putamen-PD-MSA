library(dplyr)
library(ggplot2)
library(cowplot)

left_pathways <- read.csv("./figures_data/neurons_branch_left.csv",
                          sep = ",", header = T, stringsAsFactors = F)
right_pathways <- read.csv("./figures_data/neurons_branch_right.csv",
                           sep = ",", header = T, stringsAsFactors = F)

left_pathways$branch <- "left-branch"
right_pathways$branch <- "right-branch"

pathways_ipa <- rbind(left_pathways, right_pathways)
colnames(pathways_ipa) <- c("pathway", "logp", "intersection", "zscore", "molecules", "branch")

pathways_ipa <- pathways_ipa %>%
  group_by(pathway) %>%
  filter(logp >= quantile(pathways_ipa$logp, probs = 0.96)) %>%
  filter(abs(zscore) > 1) %>%
  filter(length(branch) > 1) %>%
  mutate(xlogp = ifelse(branch == "left-branch", -logp, logp) )

ggplot(pathways_ipa, aes(x = xlogp,
                         y = reorder(pathway, logp),
                         fill = zscore,
                         width = 0.4)) +
  geom_bar(stat = "identity") +
  geom_text(aes(x = 0.1, label=pathway,),
            hjust = "center", vjust = -0.7, 
            size = 7) +
  xlab("-log(p-value)") + ylab("") +
  geom_hline(yintercept=0) +
  scale_fill_gradientn(colours =c("blue3","white", "orange"),
                       na.value = "grey",
                       limits = c(-4, 4)) +
  scale_x_continuous(labels = abs) +
  theme_cowplot() +

  theme(strip.background = element_blank(),
        legend.position = "bottom",
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank()
  )
