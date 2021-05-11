.markerFile <- "./data/mmBrain_markers.txt"
.allLines <- readLines(.markerFile)

.markerLines <- list(
  "microglia" = c(447, 479),
  "neuron" = c(1, 443),
  "oligodendrocyte" = c(481, 570)
)


getMarkers <- function(type="microglia"){
  .lineRange <- .markerLines[[type]]
  .typeLines <- .allLines[.lineRange[1]:.lineRange[2]]
  
  .garnett_marker_type <- .typeLines[sapply(.typeLines, startsWith, prefix = ">")]
  .garnett_marker_type <- gsub(">", "", .garnett_marker_type)
  
  .garnett_markers <- .typeLines[sapply(.typeLines, startsWith, prefix = "expressed:")]
  .garnett_markers <- gsub("\\s", "", .garnett_markers)
  .garnett_markers <- gsub("expressed:", "", .garnett_markers)
  
  .garnett_markers <- sapply(.garnett_markers, function(x){
    x <- toupper(x)
    return(strsplit(x, split = ",", fixed = TRUE))
  })
  
  names(.garnett_markers) <- .garnett_marker_type
  
  return(.garnett_markers)
}



