library(ggplot2)
library(patchwork)


FacetedDotPlot <- function (features=NULL, flip=FALSE, legend=TRUE, ...){
  if(!is.list(features)){
    print("ERROR: features must be a list")
    return(-1)
  }
  
  plot.list <- lapply(seq_along(features), function(x) {
    current.features <- features[[x]]
    plot <- DotPlot(features = current.features, scale = FALSE, ...) + xlab("") + ggtitle(names(features)[x])
    if(flip) plot <- plot + coord_flip()
    
    if(!legend && x != length(features)) plot <- plot + NoLegend()
    
    return(plot)
    }
    )
  p <- patchwork::wrap_plots(plot.list, ncol = 2)
  return(p)
}
