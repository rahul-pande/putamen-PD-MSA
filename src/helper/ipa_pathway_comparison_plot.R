library(dplyr)
library(ggplot2)
library(cowplot)

file_list <- list.files(
  "C:/Users/E0462428/Documents/projects/putamen/data/stim/astrocytes/",
  full.names = T)

names(file_list) <- lapply(file_list, function(file_path){
  head(
    strsplit(tail(strsplit(file_path, fixed = T, split = "/")[[1]],1),
             fixed = T,
             split = ".")[[1]], 1)
  }
  )

df_list <- lapply(file_list, function(file_path){
  read.csv(file_path, sep = "\t", skip = 2, encoding = "UTF-8")
})

inter_data <- bind_rows(df_list, .id = "group")
inter_data <- inter_data[, 1:5]
colnames(inter_data) <- c("group", "pathway", "neg_log_pvalue", "ratio", "zscore")

inter_data <- inter_data%>%
  tidyr::separate(group, sep = "_", into = c("skip", "cluster", "stim"),
                  extra = "drop", remove = F) %>%
  group_by(pathway) %>%
  mutate(mean_neg_log_pvalue = mean(neg_log_pvalue, na.rm = T) )

plot_data_common <- inter_data %>%
  group_by(pathway) %>%
  arrange( mean_neg_log_pvalue ) %>%
  mutate(stim_coverage = length(unique(stim)) ) %>%
  filter(stim_coverage > 1) %>%
  ungroup() %>%
  filter( mean_neg_log_pvalue > quantile(mean_neg_log_pvalue, 0.94) )

plot_data_cluster <- inter_data %>%
  group_by(group) %>%
  arrange( desc(neg_log_pvalue) ) %>%
  slice(1:5)

plot_data <- subset(inter_data, inter_data$pathway %in% unique(
  bind_rows(list(plot_data_cluster, plot_data_common))$pathway)
  )
  
plot_data$is_na <- ifelse(is.na(plot_data$zscore),"Missing zscore","zscore")

# plot_data$cluster <- factor(plot_data$cluster, levels = c("D1MSN", "D2MSN", "MSN", "GABA1", "GABA2"))

ggplot(plot_data, aes(x = cluster,
                      y = reorder(pathway, neg_log_pvalue),
                      size = neg_log_pvalue,
                      shape = is_na,
                      color = zscore) ) +
  # geom_point(size = 5) +
  geom_point() +
  facet_grid(~ stim) +
  scale_shape_manual(name="",values=c("Missing zscore"=22,"zscore"=15)) +
  scale_colour_gradient2("Activation zscore",
                         low = "blue", mid = "grey", high = "red",
                         midpoint = 0) +
  # scale_size_continuous("-log(pvalue)") +
  theme_cowplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, face = "bold"),
        axis.title = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        strip.background = element_rect(fill = "grey90"),
        strip.text = element_text(face = "bold") )

readr::write_csv(plot_data,
                 "C:/Users/E0462428/Documents/projects/putamen/data/stim/astrocytes_comparison.csv")
