library(cowplot)
library(ggplot2)
module_pathways <- read.csv("./data/enrichment_data/microglia_branch_pd.csv",
                         sep = ",", stringsAsFactors = F)
module_pathways <- module_pathways[,c(1,2)]
colnames(module_pathways) <- c("pathway", "adj_pval")
module_pathways$adj_pval <- -log(module_pathways$adj_pval)

colors <- RColorBrewer::brewer.pal(n = 9, name = "Blues")[c(1,3)]


ggplot(module_pathways[1:5,], aes(x = adj_pval, y = reorder(pathway, adj_pval))) +
  geom_bar(aes(adj_pval, fill = adj_pval), stat = "identity") +
  geom_text(aes(x = 0, label=pathway),
            hjust = "left",
            size = 7) +
  xlab("-log(adj pvalue)") + ylab("") +
  scale_fill_gradient2(name = "-log(adj pvalue)",
                       low = colors[1], 
                       high = colors[2], n.breaks = 4) +
  theme_cowplot() +
  theme(strip.background = element_blank(),
        legend.position = "bottom",
        legend.justification = "center",
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank(),
        axis.title = element_blank()
  )
