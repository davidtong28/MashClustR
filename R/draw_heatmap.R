#' generates a heatmap of mash distances, reordered by UMPGA hierarchal clustering, annotated by clusters in colors.
#'
#' @param cutoff a hierarchical clustering cutoff, from 0 to 1.
#'
#' @return a heatmap. Currently only supports annotating 50 clusters or less. Adjust threshold accordingly.
#'
#' @import dplyr
#'
#' @export
#'
draw_heatmap <- function(cutoff) {

  color_palette <- c("#848534","#6f3cd6","#7be642","#b44fdd","#d6e73f","#482797","#70e37b","#de38c1","#44a731","#5e66dc","#95bb3b","#c752b3","#dcc950","#381a59","#c7de8c","#7c2e7e","#71e0af","#e14281","#59a461","#a877d1","#d59c32","#485098","#e1462a","#66cecc","#9b3a20","#6b8bd9","#d97c41","#60aed4","#dc5861","#38672b","#e687c2","#3e4627","#c9a8d9","#262225","#c4d8bc","#47152a","#b7c6de","#91343f","#6a8e78","#982e60","#d4b17e","#2e2e51","#ddaeaf","#30585c","#855f2d","#617797","#603325","#ac6f91","#b37b6c","#704b64")

if (max(cluster_list_gen(cutoff)$cluster) > 50 ){
  print("This will create more than 50 clusters. Minor clusters (>50) will not be annotated")
}

as.matrix(mash_matrix)[hclust(d = mash_matrix,method = "average")$order,hclust(d = mash_matrix,method = "average")$order] %>%
  heatmap(,revC = TRUE, scale = "none", labCol = FALSE, col=gray.colors(256),RowSideColors = color_palette[(cluster_list_gen(cutoff))[hclust(d = mash_matrix,method = "average")$order,]$cluster], Rowv= NA, Colv= NA) ;
  legend("topright", fill = color_palette[1:max(cluster_list_gen(cutoff)$cluster)], legend = 1:max(cluster_list_gen(cutoff)$cluster))
  }
