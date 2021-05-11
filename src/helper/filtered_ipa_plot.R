library(dplyr)
library(ggplot2)
library(cowplot)

plot_data <- read.csv(
  "C:/Users/E0462428/Documents/projects/putamen/data/stim/neurons_comparison_filtered.csv",
  encoding = "UTF-8")

plot_data$cluster <- factor(plot_data$cluster, levels = c("D1MSN", "D2MSN", "MSN", "GABA1", "GABA2"))

ggplot(plot_data, aes(x = cluster,
                      y = reorder(pathway, neg_log_pvalue),
                      # size = neg_log_pvalue,
                      shape = is_na,
                      color = zscore) ) +
  geom_point(size = 7) +
  # geom_point() +
  facet_grid(~ stim) +
  scale_shape_manual(name="",
                     values= c("Missing zscore"=22, "zscore"=15),
                     guide = guide_legend(direction = "vertical",
                                          order = 2) ) +
  
  scale_colour_gradient2("Activation zscore",
                         low = "blue", mid = "grey", high = "red",
                         midpoint = 0,
                         guide = guide_colorbar(direction = "horizontal",
                                                title.position = "top",
                                                order = 1) ) +
  
  # scale_size_continuous("-log(pvalue)") +
  theme_cowplot() +
  theme(axis.text.x = element_text(angle = 90, face = "bold", vjust = 0.4),
        axis.text.y = element_text(hjust = 0, size = rel(1.5), face = "plain"),
        axis.title = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        # legend.position = "bottom",
        # legend.direction = "vertical",
        # legend.box = "vertical",
        panel.background = element_blank(),
        strip.background = element_rect(fill = "grey90"),
        strip.text = element_text(face = "bold") )
