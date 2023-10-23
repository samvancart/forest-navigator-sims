
# Functions

# Merge trees and clusters and calculate residuals
prepare_tabX <- function(trees, clusters, old_val) {
  setnames(clusters,old_val,"cluster")
  setnames(trees,old_val,"tree")
  tabX <- merge(clusters,trees)
  tabX <- tabX[,residuals:=cluster-tree]
  
  return(tabX)
}