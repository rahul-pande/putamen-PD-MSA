install.packages("reticulate", Ncpus = 4)

library(reticulate)
library(Seurat)
library(Matrix)


install_miniconda()
conda_install("r-reticulate", packages = "anndata", channel = "bioconda")

# conda_list() output:
# r-reticulate /home/E0462428/.local/share/r-miniconda/envs/r-reticulate/bin/python
# conda_python(envname = "r-reticulate", conda = "auto")

anndata <- import("anndata", convert = FALSE)

seurat_object <- readRDS("./GROUP-TS/Genomics/rahul/putamen/data/clusters_labelled_102620_PD_MSA_CTR_res0_8_feat2000.rds")
seurat_object$cell_type <- Idents(seurat_object)

sampled_cells <- sample(colnames(seurat_object), 10000)
sampled_seurat_object <- seurat_object[,sampled_cells]
sampled_seurat_object <- UpdateSeuratObject(
  DietSeurat(sampled_seurat_object, assays = "RNA",
             dimreducs = c("umap", "tsne"))
)

adata <- anndata$AnnData(
  X = t(GetAssayData(object = sampled_seurat_object)),
  obs = data.frame(sampled_seurat_object@meta.data), 
  obsm  = list(
    "X_tsne" = Embeddings(sampled_seurat_object[["tsne"]]),
    "X_umap" = Embeddings(sampled_seurat_object[["umap"]])
  )
)

adata$var_names <- row.names(sampled_seurat_object)

anndata$AnnData$write(adata, 'converted.h5ad')
