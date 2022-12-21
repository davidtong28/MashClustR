#' Generates a histogram of cluster count distribution from cutoff
#'
#' @param cutoff a hierarchical clustering cutoff, from 0 to 1, same as cluster_list_gen
#'
#' @return a histogram of cluster count distribution
#'
#' @import  dplyr
#'          ggplot2
#'
#' @export

cluster_plot_gen <- function(cutoff) {
  ggplot(cluster_list_gen(cutoff)) +aes(cluster)+geom_histogram(binwidth=1)+ggtitle(label = paste("Cutoff=",cutoff,sep=''))+scale_x_continuous(breaks = c(1:max(cluster_list_gen(cutoff)$cluster)))
}
