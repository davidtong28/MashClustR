#'Generates a UPGMA hierarchical clustered table of Mash sequences based on given threshold from 0 to 1.
#'
#' @param cutoff The mash distance cutoff for hierarchical clustering, from 0 to 1. cutoff=0 means sequences will be divided when their mash distance is above 0. cutoff >=1 means all sequences will form one cluster.
#'
#' @return A clustering list (tibble format). The first column is the mash sequences in original order, the second column is the assigned cluster bases on given cutoff.
#'
#' @import dplyr
#'
#' @export
cluster_list_gen <- function(cutoff){
  as_tibble(as.data.frame(cutree(hclust(d = mash_matrix,method = "average"),h=cutoff)) %>%
  rename(cluster=1),rownames = "sequence")
}
