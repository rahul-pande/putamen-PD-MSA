library(ggplot2)
library(scales)
library(cowplot)

module_pathways <- read.csv("./enrichment_data/brown_module_enrichment_kegg.csv",
                      sep = ",", skip = 0,
                      header = T, stringsAsFactors = F)

colors <- RColorBrewer::brewer.pal(n = 9, name = "Blues")[c(4,6)]


ggplot(module_pathways, aes(x = log10(P.Value), y = reorder(Pathway, P.Value))) +
  geom_bar(aes(P.Value, fill = P.Value), stat = "identity") +
  geom_text(aes(x = 0, label=Pathway,),
            hjust = "left",
            size = 7) +
  xlab("-log(pvalue)") + ylab("") +
  scale_fill_gradient2(name = "-log(pvalue)",
                       low = colors[1], 
                       high = colors[2]) +
  theme_cowplot() +
  theme(strip.background = element_blank(),
        legend.position = "bottom",
        axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank()
  )