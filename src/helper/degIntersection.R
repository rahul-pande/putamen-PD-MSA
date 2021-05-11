library(dplyr)
library(ggplot2)
library(cowplot)

for (type in c("neurons", "astrocytes", "microglia")){
  all.degs <- NULL
  
  for (file in list.files(paste0("./data/deg/", type, "/stim/"), full.names = T) ){
    all.degs <- rbind(all.degs, read.csv(file = file, ))
  }
  
  all.degs <- subset(all.degs, p_val_adj < 0.05)
  
  write.csv(unique(subset(all.degs, select = c("X"))),
            paste0("./data/deg/", type, "/", type, ".deg.intersection.txt"),
            row.names = F, quote = F, col.names = NULL)
}

deg_files <- list("Astrocytes" = "./data/deg/astrocytes/astrocytes.deg.intersection.txt",
                  "Microglia" = "./data/deg/microglia/microglia.deg.intersection.txt",
                  "Neurons" = "./data/deg/neurons/neurons.deg.intersection.txt"
                  )


nalls_nearest <- unique(
  scan("./data/auxiliary_data/Nalls_88_nearest_genes_from_GWAS_paper.txt",
       character(), quote = "")
  )

cell_type_degs <- mapply(function(x, i){
  unique(
    scan(
      x,
      character(),
      quote = "" , skip = 1
      )
    )
},
deg_files,
names(deg_files))


cell_type_deg_info <- lapply(cell_type_degs, function(x){
  c(length(unique(x)),
    length(unique(nalls_nearest)),
    length(intersect(x, nalls_nearest)),
    as.numeric(1 - phyper(q = length(intersect(x, nalls_nearest)),
               m = length(unique(nalls_nearest)),
               n = (18762 - length(unique(nalls_nearest))) ,
               k = length(unique(x)) )
               )
    , paste0(intersect(x, nalls_nearest), collapse = ", ")
  )}
  )

cell_type_deg_info <- data.frame(t(data.frame(cell_type_deg_info)))

colnames(cell_type_deg_info) <- c("num_deg", "nalls_nearest",
                                  "intersection", "pvalue"
                                  ,"intersection_genes"
                                  )

# cell_type_deg_info$"-log(pvalue)" <- -log(cell_type_deg_info$pvalue)

cell_type_deg_info <- cell_type_deg_info %>% tibble::rownames_to_column("cell_type")

write.csv(cell_type_deg_info, "./data/deg/cell_type_gwas_intersection.csv", row.names = F)


colors <- RColorBrewer::brewer.pal(n = 9, name = "Blues")[c(1,3)]



# ggplot(cell_type_deg_info, aes(x = -log(pvalue), y = reorder(cell_type, -log(pvalue) ))) +
#   geom_bar(aes_string("-log(pvalue)", fill = "-log(pvalue)"), stat = "identity") +
#   geom_text(aes_string(x = "0.5", label="cell_type"),
#             hjust = "left",
#             size = 7) +
#   xlab("-log(pvalue)") + ylab("") +
#   scale_fill_gradient2(name = "-log(pvalue)",
#                        low = colors[1], 
#                        high = colors[2], n.breaks = 4) +
#   theme_cowplot() +
#   theme(strip.background = element_blank(),
#         legend.position = "bottom",
#         axis.text.y = element_blank(),
#         axis.line.y = element_blank(),
#         axis.ticks.y = element_blank(),
#         axis.title = element_blank()
#   )
# 
