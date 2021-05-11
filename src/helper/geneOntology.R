library(org.Hs.eg.db)
library(ViSEAGO)
library(reactable)

.EntrezGene <- ViSEAGO::EntrezGene2GO()

.myGENE2GO <- annotate(id="9606", .EntrezGene)

.background <- keys(org.Hs.eg.db, keytype ='ENTREZID')


getGOTerms <- function(selection){
  .entrez_id_mapping <- select(org.Hs.eg.db, 
                      keys = selection,
                      columns = c("ENTREZID", "SYMBOL"),
                      keytype = "SYMBOL")
  
  .selection_entrez_id <- .entrez_id_mapping$ENTREZID[!is.na(.entrez_id_mapping$ENTREZID)]
  
  BP <- ViSEAGO::create_topGOdata(
    geneSel = .selection_entrez_id, #your DEG vector
    allGenes = .background, #your created background vector
    gene2GO=.myGENE2GO, 
    ont="BP",
    nodeSize=5
  )
  
  
  classic <- topGO::runTest(
    BP,
    algorithm ="classic",
    statistic = "fisher"
  )
  
  .BP_sResults <- ViSEAGO::merge_enrich_terms(
    Input=list(
      condition=c("BP","classic")
    ), envir = environment()
  )
  
  return(.BP_sResults@data)
}


plotGOTable <- function(genelist, pergroup = 5){
  go_bp_result <- lapply(gene_list, getGOTerms)
  go_df <- lapply(go_bp_result, function(x){
    x[order(x$condition.pvalue), c("term",
                                   "condition.Significant_genes_symbol",
                                   "condition.pvalue")][1:pergroup]
  })
  names(go_df) <- 1:length(go_df)
  
  cluster_go <- do.call("rbind", go_df)
  cluster_go$Cluster <- rep(names(go_df), sapply(go_df, nrow))
  
  return(cluster_go)
  
  ncolor = 20
  color_palette = colorRampPalette(c("darkblue", "grey"))(ncolor)
  
  cluster_go$scaled_color <- round(normalize(cluster_go$condition.pvalue, min = 1, max = ncolor))
  
  reactable(cluster_go[, c(4,1,2,3)],
            columns = list(
              term = colDef(
                style = function(value, idx){
                  return(list(color = color_palette[cluster_go$scaled_color[idx]]))
                }),
              Cluster = colDef()
            ),
            groupBy = "Cluster",
            defaultPageSize = 40
  )
}
